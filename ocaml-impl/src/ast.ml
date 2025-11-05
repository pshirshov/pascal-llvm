(** Abstract Syntax Tree for Pascal-like language *)

type position = {
  line : int;
  column : int;
}

type identifier = string

(** Type expressions *)
type type_expr =
  | TInteger
  | TReal
  | TBoolean
  | TChar
  | TString
  | TPointer of type_expr
  | TArray of type_expr * int  (** element type * size *)
  | TRecord of record_field list
  | TNamed of identifier  (** Named type reference *)

and record_field = {
  field_name : identifier;
  field_type : type_expr;
}

(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or

(** Unary operators *)
type unop =
  | Neg
  | Not

(** Expressions *)
type expr =
  | EInteger of int
  | EReal of float
  | EBoolean of bool
  | EChar of char
  | EString of string
  | EVar of identifier
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECall of identifier * expr list
  | EArrayAccess of expr * expr
  | ERecordAccess of expr * identifier
  | EDeref of expr  (** Pointer dereference: p^ *)
  | EAddress of expr  (** Address of: @x *)
  | ENew of type_expr  (** Dynamic allocation *)

(** Statements *)
type stmt =
  | SAssign of expr * expr  (** lvalue, rvalue *)
  | SCall of identifier * expr list
  | SIf of expr * stmt list * stmt list option
  | SWhile of expr * stmt list
  | SFor of identifier * expr * expr * stmt list
  | SWriteln of expr list
  | SWrite of expr list
  | SReadln of identifier list
  | SReturn of expr option
  | SBlock of stmt list
  | SVarDecl of identifier * type_expr * expr  (** var name: type := init *)
  | SValDecl of identifier * type_expr * expr  (** val name: type = init *)

(** Variable declaration *)
type var_decl = {
  var_name : identifier;
  var_type : type_expr;
  var_init : expr option;
}

(** Function parameter *)
type param = {
  param_name : identifier;
  param_type : type_expr;
  is_var : bool;  (** VAR parameter (by reference) *)
}

(** Function declaration *)
type func_decl = {
  func_name : identifier;
  params : param list;
  return_type : type_expr option;  (** None for procedures *)
  local_vars : var_decl list;
  body : stmt list;
}

(** Type declaration *)
type type_decl = {
  type_name : identifier;
  type_def : type_expr;
}

(** Top-level declarations *)
type declaration =
  | DType of type_decl
  | DVar of var_decl
  | DFunc of func_decl

(** Complete program *)
type program = {
  program_name : identifier;
  declarations : declaration list;
}
