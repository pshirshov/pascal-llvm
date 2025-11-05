open Ast
open Llvm

exception Codegen_error of string

type codegen_context = {
  llcontext : llcontext;
  llmodule : llmodule;
  builder : llbuilder;
  mutable named_values : (string, llvalue) Hashtbl.t;
  mutable type_cache : (type_expr, lltype) Hashtbl.t;
  mutable func_types : (string, lltype) Hashtbl.t;
  mutable type_defs : (string, type_expr) Hashtbl.t;
  mutable var_types : (string, type_expr) Hashtbl.t;
}

let create_context module_name =
  let llcontext = global_context () in
  let llmodule = create_module llcontext module_name in
  let builder = builder llcontext in
  {
    llcontext;
    llmodule;
    builder;
    named_values = Hashtbl.create 53;
    type_cache = Hashtbl.create 17;
    func_types = Hashtbl.create 17;
    type_defs = Hashtbl.create 17;
    var_types = Hashtbl.create 53;
  }

let rec resolve_type ctx ty =
  match ty with
  | TNamed name ->
      (match Hashtbl.find_opt ctx.type_defs name with
       | Some resolved -> resolve_type ctx resolved
       | None -> raise (Codegen_error (Printf.sprintf "Undefined type: %s" name)))
  | _ -> ty

let rec lltype_of_type ctx ty =
  match Hashtbl.find_opt ctx.type_cache ty with
  | Some llty -> llty
  | None ->
      let llty = match ty with
        | TInteger -> i32_type ctx.llcontext
        | TReal -> double_type ctx.llcontext
        | TBoolean -> i1_type ctx.llcontext
        | TChar -> i8_type ctx.llcontext
        | TString -> pointer_type ctx.llcontext
        | TPointer _ -> pointer_type ctx.llcontext
        | TArray (elem_type, size) ->
            array_type (lltype_of_type ctx elem_type) size
        | TRecord fields ->
            let field_types = List.map (fun f -> lltype_of_type ctx f.field_type) fields in
            struct_type ctx.llcontext (Array.of_list field_types)
        | TNamed name ->
            (match Hashtbl.find_opt ctx.type_defs name with
             | Some resolved_type -> lltype_of_type ctx resolved_type
             | None -> raise (Codegen_error (Printf.sprintf "Unresolved named type: %s" name)))
      in
      Hashtbl.add ctx.type_cache ty llty;
      llty

let get_field_index fields field_name =
  let rec find_index i = function
    | [] -> None
    | f :: rest ->
        if f.field_name = field_name then Some i
        else find_index (i + 1) rest
  in
  find_index 0 fields

(* Declare external runtime functions *)
let declare_runtime_functions ctx =
  (* printf: i32 (ptr, ...) *)
  let printf_ty = var_arg_function_type (i32_type ctx.llcontext)
    [| pointer_type ctx.llcontext |] in
  Hashtbl.add ctx.func_types "printf" printf_ty;
  let printf_func = declare_function "printf" printf_ty ctx.llmodule in

  (* scanf: i32 (ptr, ...) *)
  let scanf_ty = var_arg_function_type (i32_type ctx.llcontext)
    [| pointer_type ctx.llcontext |] in
  Hashtbl.add ctx.func_types "scanf" scanf_ty;
  let scanf_func = declare_function "scanf" scanf_ty ctx.llmodule in

  (* malloc: ptr (i64) *)
  let malloc_ty = function_type (pointer_type ctx.llcontext)
    [| i64_type ctx.llcontext |] in
  Hashtbl.add ctx.func_types "malloc" malloc_ty;
  let malloc_func = declare_function "malloc" malloc_ty ctx.llmodule in

  (printf_func, scanf_func, malloc_func)

let build_string_literal ctx str =
  let str_const = const_stringz ctx.llcontext str in
  let global = define_global ("str_" ^ (string_of_int (Hashtbl.hash str))) str_const ctx.llmodule in
  let zero = const_int (i32_type ctx.llcontext) 0 in
  const_in_bounds_gep (lltype_of_type ctx TString) global [| zero; zero |]

let rec codegen_expr ctx expr =
  match expr with
  | EInteger i -> const_int (i32_type ctx.llcontext) i
  | EReal f -> const_float (double_type ctx.llcontext) f
  | EBoolean b -> const_int (i1_type ctx.llcontext) (if b then 1 else 0)
  | EChar c -> const_int (i8_type ctx.llcontext) (Char.code c)
  | EString s -> build_string_literal ctx s

  | EVar name ->
      (match Hashtbl.find_opt ctx.named_values name with
       | Some ptr ->
           let var_type = match Hashtbl.find_opt ctx.var_types name with
             | Some t -> resolve_type ctx t
             | None -> TInteger  (* fallback to integer for backward compatibility *)
           in
           build_load (lltype_of_type ctx var_type) ptr name ctx.builder
       | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" name)))

  | EBinop (op, e1, e2) ->
      let lhs = codegen_expr ctx e1 in
      let rhs = codegen_expr ctx e2 in
      (match op with
       | Add -> build_add lhs rhs "addtmp" ctx.builder
       | Sub -> build_sub lhs rhs "subtmp" ctx.builder
       | Mul -> build_mul lhs rhs "multmp" ctx.builder
       | Div -> build_sdiv lhs rhs "divtmp" ctx.builder
       | Mod -> build_srem lhs rhs "modtmp" ctx.builder
       | Eq -> build_icmp Icmp.Eq lhs rhs "eqtmp" ctx.builder
       | Ne -> build_icmp Icmp.Ne lhs rhs "netmp" ctx.builder
       | Lt -> build_icmp Icmp.Slt lhs rhs "lttmp" ctx.builder
       | Le -> build_icmp Icmp.Sle lhs rhs "letmp" ctx.builder
       | Gt -> build_icmp Icmp.Sgt lhs rhs "gttmp" ctx.builder
       | Ge -> build_icmp Icmp.Sge lhs rhs "getmp" ctx.builder
       | And -> build_and lhs rhs "andtmp" ctx.builder
       | Or -> build_or lhs rhs "ortmp" ctx.builder)

  | EUnop (op, e) ->
      let val_ = codegen_expr ctx e in
      (match op with
       | Neg -> build_neg val_ "negtmp" ctx.builder
       | Not -> build_not val_ "nottmp" ctx.builder)

  | ECall (name, args) ->
      let callee = match lookup_function name ctx.llmodule with
        | Some f -> f
        | None -> raise (Codegen_error (Printf.sprintf "Unknown function: %s" name))
      in
      let fn_type = match Hashtbl.find_opt ctx.func_types name with
        | Some t -> t
        | None -> raise (Codegen_error (Printf.sprintf "Function type not found: %s" name))
      in
      let params = params callee in
      if Array.length params <> List.length args then
        raise (Codegen_error (Printf.sprintf "Function %s: incorrect number of arguments" name));
      let args_vals = Array.of_list (List.map (codegen_expr ctx) args) in
      build_call fn_type callee args_vals "calltmp" ctx.builder

  | EArrayAccess (arr, idx) ->
      let arr_val = codegen_expr ctx arr in
      let idx_val = codegen_expr ctx idx in
      let ptr = build_gep (type_of arr_val) arr_val [| idx_val |] "arraytmp" ctx.builder in
      build_load (element_type (type_of arr_val)) ptr "arrayload" ctx.builder

  | ERecordAccess (rec_expr, field) ->
      (* Get the variable name and its type for now we only support direct variable access *)
      (match rec_expr with
       | EVar var_name ->
           let rec_type = match Hashtbl.find_opt ctx.var_types var_name with
             | Some t -> resolve_type ctx t
             | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" var_name))
           in
           (match rec_type with
            | TRecord fields ->
                let field_idx = match get_field_index fields field with
                  | Some i -> i
                  | None -> raise (Codegen_error (Printf.sprintf "Record has no field: %s" field))
                in
                let rec_ptr = match Hashtbl.find_opt ctx.named_values var_name with
                  | Some ptr -> ptr
                  | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" var_name))
                in
                let zero = const_int (i32_type ctx.llcontext) 0 in
                let field_idx_val = const_int (i32_type ctx.llcontext) field_idx in
                let field_ptr = build_in_bounds_gep (lltype_of_type ctx rec_type) rec_ptr
                  [| zero; field_idx_val |] (Printf.sprintf "%s.%s" var_name field) ctx.builder in
                let field_type = (List.nth fields field_idx).field_type in
                build_load (lltype_of_type ctx field_type) field_ptr "fieldload" ctx.builder
            | _ -> raise (Codegen_error "Record access on non-record type"))
       | _ -> raise (Codegen_error "Complex record expressions not yet supported"))

  | EDeref ptr ->
      let ptr_val = codegen_expr ctx ptr in
      build_load (element_type (type_of ptr_val)) ptr_val "deref" ctx.builder

  | EAddress e ->
      (match e with
       | EVar name ->
           (match Hashtbl.find_opt ctx.named_values name with
            | Some ptr -> ptr
            | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" name)))
       | _ -> raise (Codegen_error "Can only take address of variables"))

  | ENew ty ->
      let _llty = lltype_of_type ctx ty in
      let size = size_of _llty in
      let malloc_func = match lookup_function "malloc" ctx.llmodule with
        | Some f -> f
        | None -> raise (Codegen_error "malloc not declared")
      in
      let _malloc_call = build_call (function_type (pointer_type ctx.llcontext)
        [| i64_type ctx.llcontext |]) malloc_func [| size |] "malloctmp" ctx.builder in
      _malloc_call  (* In opaque pointer mode, no bitcast needed *)

let codegen_lvalue ctx expr =
  match expr with
  | EVar name ->
      (match Hashtbl.find_opt ctx.named_values name with
       | Some ptr -> ptr
       | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" name)))
  | EArrayAccess (arr, idx) ->
      let arr_val = codegen_expr ctx arr in
      let idx_val = codegen_expr ctx idx in
      build_gep (type_of arr_val) arr_val [| idx_val |] "arraylval" ctx.builder
  | ERecordAccess (rec_expr, field) ->
      (* Get the variable name and its type *)
      (match rec_expr with
       | EVar var_name ->
           let rec_type = match Hashtbl.find_opt ctx.var_types var_name with
             | Some t -> resolve_type ctx t
             | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" var_name))
           in
           (match rec_type with
            | TRecord fields ->
                let field_idx = match get_field_index fields field with
                  | Some i -> i
                  | None -> raise (Codegen_error (Printf.sprintf "Record has no field: %s" field))
                in
                let rec_ptr = match Hashtbl.find_opt ctx.named_values var_name with
                  | Some ptr -> ptr
                  | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" var_name))
                in
                let zero = const_int (i32_type ctx.llcontext) 0 in
                let field_idx_val = const_int (i32_type ctx.llcontext) field_idx in
                build_in_bounds_gep (lltype_of_type ctx rec_type) rec_ptr
                  [| zero; field_idx_val |] (Printf.sprintf "%s.%s_ptr" var_name field) ctx.builder
            | _ -> raise (Codegen_error "Record access on non-record type"))
       | _ -> raise (Codegen_error "Complex record expressions not yet supported"))
  | EDeref ptr ->
      codegen_expr ctx ptr
  | _ -> raise (Codegen_error "Invalid lvalue")

let rec codegen_stmt ctx printf_func scanf_func stmt =
  match stmt with
  | SAssign (lval, rval) ->
      let lval_ptr = codegen_lvalue ctx lval in
      let rval_val = codegen_expr ctx rval in
      ignore (build_store rval_val lval_ptr ctx.builder)

  | SCall (name, args) ->
      ignore (codegen_expr ctx (ECall (name, args)))

  | SIf (cond, then_stmts, else_stmts) ->
      let cond_val = codegen_expr ctx cond in
      let start_bb = insertion_block ctx.builder in
      let the_function = block_parent start_bb in

      let then_bb = append_block ctx.llcontext "then" the_function in
      position_at_end then_bb ctx.builder;
      List.iter (codegen_stmt ctx printf_func scanf_func) then_stmts;
      let new_then_bb = insertion_block ctx.builder in

      let else_bb = append_block ctx.llcontext "else" the_function in
      position_at_end else_bb ctx.builder;
      (match else_stmts with
       | Some stmts -> List.iter (codegen_stmt ctx printf_func scanf_func) stmts
       | None -> ());
      let new_else_bb = insertion_block ctx.builder in

      let merge_bb = append_block ctx.llcontext "ifcont" the_function in
      position_at_end new_then_bb ctx.builder;
      ignore (build_br merge_bb ctx.builder);
      position_at_end new_else_bb ctx.builder;
      ignore (build_br merge_bb ctx.builder);

      position_at_end start_bb ctx.builder;
      ignore (build_cond_br cond_val then_bb else_bb ctx.builder);

      position_at_end merge_bb ctx.builder

  | SWhile (cond, body) ->
      let start_bb = insertion_block ctx.builder in
      let the_function = block_parent start_bb in

      let loop_bb = append_block ctx.llcontext "loop" the_function in
      ignore (build_br loop_bb ctx.builder);
      position_at_end loop_bb ctx.builder;

      let cond_val = codegen_expr ctx cond in

      let body_bb = append_block ctx.llcontext "body" the_function in
      position_at_end body_bb ctx.builder;
      List.iter (codegen_stmt ctx printf_func scanf_func) body;
      ignore (build_br loop_bb ctx.builder);

      let after_bb = append_block ctx.llcontext "afterloop" the_function in
      position_at_end loop_bb ctx.builder;
      ignore (build_cond_br cond_val body_bb after_bb ctx.builder);

      position_at_end after_bb ctx.builder

  | SFor (var, start, stop, body) ->
      let start_val = codegen_expr ctx start in
      let var_ptr = match Hashtbl.find_opt ctx.named_values var with
        | Some ptr -> ptr
        | None -> raise (Codegen_error (Printf.sprintf "Unknown loop variable: %s" var))
      in
      ignore (build_store start_val var_ptr ctx.builder);

      let start_bb = insertion_block ctx.builder in
      let the_function = block_parent start_bb in

      let loop_bb = append_block ctx.llcontext "forloop" the_function in
      ignore (build_br loop_bb ctx.builder);
      position_at_end loop_bb ctx.builder;

      let var_val = build_load (i32_type ctx.llcontext) var_ptr var ctx.builder in
      let stop_val = codegen_expr ctx stop in
      let cond_val = build_icmp Icmp.Sle var_val stop_val "forcond" ctx.builder in

      let body_bb = append_block ctx.llcontext "forbody" the_function in
      position_at_end body_bb ctx.builder;
      List.iter (codegen_stmt ctx printf_func scanf_func) body;

      let next_var = build_add var_val (const_int (i32_type ctx.llcontext) 1) "nextvar" ctx.builder in
      ignore (build_store next_var var_ptr ctx.builder);
      ignore (build_br loop_bb ctx.builder);

      let after_bb = append_block ctx.llcontext "afterfor" the_function in
      position_at_end loop_bb ctx.builder;
      ignore (build_cond_br cond_val body_bb after_bb ctx.builder);

      position_at_end after_bb ctx.builder

  | SWriteln args | SWrite args ->
      List.iter (fun arg ->
        let arg_val = codegen_expr ctx arg in
        let fmt = match classify_type (type_of arg_val) with
          | TypeKind.Integer ->
              if integer_bitwidth (type_of arg_val) = 1 then
                build_string_literal ctx "%d"  (* boolean as int *)
              else
                build_string_literal ctx "%d"
          | TypeKind.Double -> build_string_literal ctx "%f"
          | TypeKind.Pointer -> build_string_literal ctx "%s"
          | _ -> build_string_literal ctx "%d"
        in
        ignore (build_call (var_arg_function_type (i32_type ctx.llcontext)
          [| pointer_type ctx.llcontext |])
          printf_func [| fmt; arg_val |] "printfcall" ctx.builder)
      ) args;
      (match stmt with
       | SWriteln _ ->
           let newline = build_string_literal ctx "\n" in
           ignore (build_call (var_arg_function_type (i32_type ctx.llcontext)
             [| pointer_type ctx.llcontext |])
             printf_func [| newline |] "printfcall" ctx.builder)
       | _ -> ())

  | SReadln vars ->
      List.iter (fun var ->
        let var_ptr = match Hashtbl.find_opt ctx.named_values var with
          | Some ptr -> ptr
          | None -> raise (Codegen_error (Printf.sprintf "Unknown variable: %s" var))
        in
        let fmt = build_string_literal ctx "%d" in
        ignore (build_call (var_arg_function_type (i32_type ctx.llcontext)
          [| pointer_type ctx.llcontext |])
          scanf_func [| fmt; var_ptr |] "scanfcall" ctx.builder)
      ) vars

  | SReturn expr_opt ->
      (match expr_opt with
       | Some e ->
           let ret_val = codegen_expr ctx e in
           ignore (build_ret ret_val ctx.builder)
       | None ->
           ignore (build_ret_void ctx.builder))

  | SBlock stmts ->
      List.iter (codegen_stmt ctx printf_func scanf_func) stmts

let codegen_function ctx printf_func scanf_func func =
  let param_types = Array.of_list (List.map (fun p -> lltype_of_type ctx p.param_type) func.params) in
  let ret_type = match func.return_type with
    | Some t -> lltype_of_type ctx t
    | None -> void_type ctx.llcontext
  in
  let func_type = function_type ret_type param_types in
  Hashtbl.add ctx.func_types func.func_name func_type;
  let the_function = declare_function func.func_name func_type ctx.llmodule in

  (* Create entry block *)
  let bb = append_block ctx.llcontext "entry" the_function in
  position_at_end bb ctx.builder;

  (* Save old named values and types, create new scope *)
  let old_values = Hashtbl.copy ctx.named_values in
  let old_types = Hashtbl.copy ctx.var_types in
  Hashtbl.clear ctx.named_values;
  Hashtbl.clear ctx.var_types;

  (* Allocate parameters *)
  Array.iteri (fun i param ->
    let param_info = List.nth func.params i in
    let param_name = param_info.param_name in
    let alloca = build_alloca (type_of param) param_name ctx.builder in
    ignore (build_store param alloca ctx.builder);
    Hashtbl.add ctx.named_values param_name alloca;
    Hashtbl.add ctx.var_types param_name param_info.param_type
  ) (params the_function);

  (* Allocate local variables *)
  List.iter (fun var ->
    let var_type = lltype_of_type ctx var.var_type in
    let alloca = build_alloca var_type var.var_name ctx.builder in
    Hashtbl.add ctx.named_values var.var_name alloca;
    Hashtbl.add ctx.var_types var.var_name var.var_type;
    (match var.var_init with
     | Some init ->
         let init_val = codegen_expr ctx init in
         ignore (build_store init_val alloca ctx.builder)
     | None -> ())
  ) func.local_vars;

  (* Generate function body *)
  List.iter (codegen_stmt ctx printf_func scanf_func) func.body;

  (* Add default return if needed *)
  (match block_terminator (insertion_block ctx.builder) with
   | Some _ -> ()
   | None ->
       match func.return_type with
       | Some _ -> ()  (* Should have explicit return *)
       | None -> ignore (build_ret_void ctx.builder));

  (* Restore named values and types *)
  ctx.named_values <- old_values;
  ctx.var_types <- old_types;
  the_function

let codegen_program prog =
  let ctx = create_context prog.program_name in
  let (printf_func, scanf_func, _malloc_func) = declare_runtime_functions ctx in

  (* Process type declarations *)
  List.iter (function
    | DType { type_name; type_def } ->
        Hashtbl.add ctx.type_defs type_name type_def
    | _ -> ()
  ) prog.declarations;

  (* Process global variables *)
  List.iter (function
    | DVar var ->
        let var_type = lltype_of_type ctx var.var_type in
        let global = define_global var.var_name (const_null var_type) ctx.llmodule in
        Hashtbl.add ctx.named_values var.var_name global;
        Hashtbl.add ctx.var_types var.var_name var.var_type
    | _ -> ()
  ) prog.declarations;

  (* Process functions *)
  List.iter (function
    | DFunc func ->
        ignore (codegen_function ctx printf_func scanf_func func)
    | _ -> ()
  ) prog.declarations;

  ctx.llmodule
