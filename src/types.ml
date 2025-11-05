open Ast

exception Type_error of string

type symbol_table = {
  types : (identifier, type_expr) Hashtbl.t;
  vars : (identifier, type_expr) Hashtbl.t;
  funcs : (identifier, (param list * type_expr option)) Hashtbl.t;
}

let create_symbol_table () = {
  types = Hashtbl.create 17;
  vars = Hashtbl.create 53;
  funcs = Hashtbl.create 17;
}

let rec types_equal t1 t2 =
  match t1, t2 with
  | TInteger, TInteger | TReal, TReal | TBoolean, TBoolean
  | TChar, TChar | TString, TString -> true
  | TPointer t1', TPointer t2' -> types_equal t1' t2'
  | TArray (t1', s1), TArray (t2', s2) -> s1 = s2 && types_equal t1' t2'
  | TRecord fields1, TRecord fields2 ->
      List.length fields1 = List.length fields2 &&
      List.for_all2 (fun f1 f2 ->
        f1.field_name = f2.field_name && types_equal f1.field_type f2.field_type
      ) fields1 fields2
  | TNamed n1, TNamed n2 -> n1 = n2
  | _ -> false

let resolve_type symtab = function
  | TNamed name ->
      (match Hashtbl.find_opt symtab.types name with
       | Some t -> t
       | None -> raise (Type_error (Printf.sprintf "Undefined type: %s" name)))
  | t -> t

let rec check_expr symtab expr =
  match expr with
  | EInteger _ -> TInteger
  | EReal _ -> TReal
  | EBoolean _ -> TBoolean
  | EChar _ -> TChar
  | EString _ -> TString

  | EVar name ->
      (match Hashtbl.find_opt symtab.vars name with
       | Some t -> t
       | None -> raise (Type_error (Printf.sprintf "Undefined variable: %s" name)))

  | EBinop (op, e1, e2) ->
      let t1 = check_expr symtab e1 in
      let t2 = check_expr symtab e2 in
      (match op with
       | Add | Sub | Mul | Div | Mod ->
           if not (types_equal t1 TInteger && types_equal t2 TInteger ||
                   types_equal t1 TReal && types_equal t2 TReal) then
             raise (Type_error "Arithmetic operators require numeric operands");
           t1
       | Eq | Ne | Lt | Le | Gt | Ge ->
           if not (types_equal t1 t2) then
             raise (Type_error "Comparison operators require same types");
           TBoolean
       | And | Or ->
           if not (types_equal t1 TBoolean && types_equal t2 TBoolean) then
             raise (Type_error "Logical operators require boolean operands");
           TBoolean)

  | EUnop (op, e) ->
      let t = check_expr symtab e in
      (match op with
       | Neg ->
           if not (types_equal t TInteger || types_equal t TReal) then
             raise (Type_error "Negation requires numeric operand");
           t
       | Not ->
           if not (types_equal t TBoolean) then
             raise (Type_error "Not requires boolean operand");
           TBoolean)

  | ECall (name, args) ->
      (match Hashtbl.find_opt symtab.funcs name with
       | Some (params, ret_type) ->
           if List.length args <> List.length params then
             raise (Type_error (Printf.sprintf "Function %s: wrong number of arguments" name));
           List.iter2 (fun arg param ->
             let arg_type = check_expr symtab arg in
             let param_type = resolve_type symtab param.param_type in
             if not (types_equal arg_type param_type) then
               raise (Type_error (Printf.sprintf "Function %s: argument type mismatch" name))
           ) args params;
           (match ret_type with
            | Some t -> t
            | None -> raise (Type_error (Printf.sprintf "%s is a procedure, not a function" name)))
       | None -> raise (Type_error (Printf.sprintf "Undefined function: %s" name)))

  | EArrayAccess (arr, idx) ->
      let arr_type = check_expr symtab arr in
      let idx_type = check_expr symtab idx in
      if not (types_equal idx_type TInteger) then
        raise (Type_error "Array index must be integer");
      (match arr_type with
       | TArray (elem_type, _) -> elem_type
       | _ -> raise (Type_error "Array access on non-array type"))

  | ERecordAccess (rec_expr, field) ->
      let rec_type = check_expr symtab rec_expr in
      (match rec_type with
       | TRecord fields ->
           (match List.find_opt (fun f -> f.field_name = field) fields with
            | Some f -> f.field_type
            | None -> raise (Type_error (Printf.sprintf "Record has no field: %s" field)))
       | _ -> raise (Type_error "Record access on non-record type"))

  | EDeref ptr ->
      let ptr_type = check_expr symtab ptr in
      (match ptr_type with
       | TPointer t -> t
       | _ -> raise (Type_error "Dereference on non-pointer type"))

  | EAddress e ->
      let t = check_expr symtab e in
      TPointer t

  | ENew t ->
      let resolved = resolve_type symtab t in
      TPointer resolved

let rec check_stmt symtab return_type stmt =
  match stmt with
  | SAssign (lval, rval) ->
      let lval_type = resolve_type symtab (check_expr symtab lval) in
      let rval_type = resolve_type symtab (check_expr symtab rval) in
      if not (types_equal lval_type rval_type) then
        raise (Type_error "Assignment type mismatch")

  | SCall (name, args) ->
      (match Hashtbl.find_opt symtab.funcs name with
       | Some (params, _) ->
           if List.length args <> List.length params then
             raise (Type_error (Printf.sprintf "Procedure %s: wrong number of arguments" name));
           List.iter2 (fun arg param ->
             let arg_type = check_expr symtab arg in
             if not (types_equal arg_type param.param_type) then
               raise (Type_error (Printf.sprintf "Procedure %s: argument type mismatch" name))
           ) args params
       | None -> raise (Type_error (Printf.sprintf "Undefined procedure: %s" name)))

  | SIf (cond, then_stmts, else_stmts) ->
      let cond_type = check_expr symtab cond in
      if not (types_equal cond_type TBoolean) then
        raise (Type_error "If condition must be boolean");
      List.iter (check_stmt symtab return_type) then_stmts;
      (match else_stmts with
       | Some stmts -> List.iter (check_stmt symtab return_type) stmts
       | None -> ())

  | SWhile (cond, body) ->
      let cond_type = check_expr symtab cond in
      if not (types_equal cond_type TBoolean) then
        raise (Type_error "While condition must be boolean");
      List.iter (check_stmt symtab return_type) body

  | SFor (_var, start, stop, body) ->
      let start_type = check_expr symtab start in
      let stop_type = check_expr symtab stop in
      if not (types_equal start_type TInteger && types_equal stop_type TInteger) then
        raise (Type_error "For loop bounds must be integers");
      List.iter (check_stmt symtab return_type) body

  | SWriteln exprs | SWrite exprs ->
      List.iter (fun e -> ignore (check_expr symtab e)) exprs

  | SReadln vars ->
      List.iter (fun var ->
        match Hashtbl.find_opt symtab.vars var with
        | Some _ -> ()
        | None -> raise (Type_error (Printf.sprintf "Undefined variable: %s" var))
      ) vars

  | SReturn expr_opt ->
      (match expr_opt, return_type with
       | Some e, Some expected_type ->
           let actual_type = check_expr symtab e in
           if not (types_equal actual_type expected_type) then
             raise (Type_error "Return type mismatch")
       | None, None -> ()
       | Some _, None ->
           raise (Type_error "Procedure should not return a value")
       | None, Some _ ->
           raise (Type_error "Function must return a value"))

  | SBlock stmts ->
      List.iter (check_stmt symtab return_type) stmts

let check_program prog =
  let symtab = create_symbol_table () in

  (* First pass: collect type and function declarations *)
  List.iter (function
    | DType { type_name; type_def } ->
        Hashtbl.add symtab.types type_name type_def
    | DFunc { func_name; params; return_type; _ } ->
        Hashtbl.add symtab.funcs func_name (params, return_type)
    | DVar _ -> ()
  ) prog.declarations;

  (* Second pass: collect global variables and check functions *)
  List.iter (function
    | DVar { var_name; var_type; var_init } ->
        let resolved = resolve_type symtab var_type in
        Hashtbl.add symtab.vars var_name resolved;
        (match var_init with
         | Some init ->
             let init_type = check_expr symtab init in
             if not (types_equal resolved init_type) then
               raise (Type_error (Printf.sprintf "Variable %s: initializer type mismatch" var_name))
         | None -> ())

    | DFunc func ->
        (* Create function-local symbol table *)
        let local_symtab = {
          types = Hashtbl.copy symtab.types;
          vars = Hashtbl.copy symtab.vars;
          funcs = symtab.funcs;
        } in

        (* Add parameters to local scope *)
        List.iter (fun param ->
          let resolved = resolve_type local_symtab param.param_type in
          Hashtbl.add local_symtab.vars param.param_name resolved
        ) func.params;

        (* Add local variables *)
        List.iter (fun var ->
          let resolved = resolve_type local_symtab var.var_type in
          Hashtbl.add local_symtab.vars var.var_name resolved;
          (match var.var_init with
           | Some init ->
               let init_type = check_expr local_symtab init in
               if not (types_equal resolved init_type) then
                 raise (Type_error (Printf.sprintf "Variable %s: initializer type mismatch" var.var_name))
           | None -> ())
        ) func.local_vars;

        (* Check function body *)
        let return_type = match func.return_type with
          | Some t -> Some (resolve_type local_symtab t)
          | None -> None
        in
        List.iter (check_stmt local_symtab return_type) func.body

    | DType _ -> ()
  ) prog.declarations
