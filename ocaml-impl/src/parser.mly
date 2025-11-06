%{
open Ast
%}

%token <int> INTEGER
%token <float> REAL
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token TRUE FALSE
%token PROGRAM VAR VAL TYPE DEF END
%token IF ELSE WHILE FOR TO OF ARRAY RECORD
%token TINTEGER TREAL TBOOLEAN TCHAR TSTRING
%token AND OR NOT DIV MOD
%token WRITELN WRITE READLN NEW RETURN
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token DOT COMMA COLON SEMICOLON ASSIGN DOTDOT
%token EQ NE LT LE GT GE
%token PLUS MINUS STAR SLASH CARET AT
%token EOF

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH DIV MOD
%nonassoc UNARY
%nonassoc DOT
%nonassoc LBRACK
%nonassoc CARET AT

%start <Ast.program> program

%%

program:
  | PROGRAM; name=IDENT; SEMICOLON; decls=list(declaration); DOT; EOF
    { { program_name = name; declarations = decls } }

declaration:
  | TYPE; type_decls=nonempty_list(type_declaration)
    { DType (List.hd type_decls) }
  | VAR; var_decls=nonempty_list(var_declaration)
    { DVar (List.hd var_decls) }
  | d=def_declaration
    { DFunc d }

type_declaration:
  | name=IDENT; EQ; t=type_expr; SEMICOLON
    { { type_name = name; type_def = t } }

var_declaration:
  | name=IDENT; COLON; t=type_expr; SEMICOLON
    { { var_name = name; var_type = t; var_init = None } }
  | name=IDENT; COLON; t=type_expr; ASSIGN; init=expr; SEMICOLON
    { { var_name = name; var_type = t; var_init = Some init } }

def_declaration:
  | DEF; name=IDENT; LPAREN; params=separated_list(COMMA, def_param); RPAREN;
    COLON; ret=type_expr; EQ; body=brace_block
    { { func_name = name; params; return_type = Some ret;
        local_vars = []; body } }
  | DEF; name=IDENT; LPAREN; params=separated_list(COMMA, def_param); RPAREN;
    EQ; body=brace_block
    { { func_name = name; params; return_type = None;
        local_vars = []; body } }

def_param:
  | name=IDENT; COLON; t=type_expr
    { { param_name = name; param_type = t; is_var = false } }
  | VAR; name=IDENT; COLON; t=type_expr
    { { param_name = name; param_type = t; is_var = true } }

type_expr:
  | TINTEGER  { TInteger }
  | TREAL     { TReal }
  | TBOOLEAN  { TBoolean }
  | TCHAR     { TChar }
  | TSTRING   { TString }
  | CARET; t=type_expr  { TPointer t }
  | ARRAY; LBRACK; size=INTEGER; RBRACK; OF; t=type_expr
    { TArray (t, size) }
  | ARRAY; LBRACK; t=type_expr; COMMA; size=INTEGER; RBRACK
    { TArray (t, size) }
  | RECORD; fields=nonempty_list(record_field); END
    { TRecord fields }
  | RECORD; LBRACE; fields=record_field_list_opt_semi; RBRACE
    { TRecord fields }
  | name=IDENT
    { TNamed name }

record_field_list_opt_semi:
  | (* empty *)
    { [] }
  | f=record_field_brace
    { [f] }
  | f=record_field_brace; SEMICOLON; rest=record_field_list_opt_semi
    { f :: rest }
  | f=record_field_brace; rest=record_field_list_opt_semi
    { f :: rest }

record_field_brace:
  | name=IDENT; COLON; t=type_expr
    { { field_name = name; field_type = t } }

record_field:
  | name=IDENT; COLON; t=type_expr; SEMICOLON
    { { field_name = name; field_type = t } }

statement:
  | VAR; name=IDENT; COLON; t=type_expr; assign_op; init=expr
    { SVarDecl (name, t, init) }
  | VAR; name=IDENT; COLON; t=type_expr
    { SVarDecl (name, t, EInteger 0) }
  | VAL; name=IDENT; COLON; t=type_expr; EQ; init=expr
    { SValDecl (name, t, init) }
  | lval=lvalue; assign_op; rval=expr
    { SAssign (lval, rval) }
  | name=IDENT; LPAREN; args=separated_list(COMMA, expr); RPAREN
    { SCall (name, args) }
  | IF; LPAREN; cond=expr; RPAREN; then_body=brace_block; ELSE; else_body=brace_block
    { SIf (cond, then_body, Some else_body) }
  | IF; LPAREN; cond=expr; RPAREN; then_body=brace_block
    { SIf (cond, then_body, None) }
  | WHILE; LPAREN; cond=expr; RPAREN; body=brace_block
    { SWhile (cond, body) }
  | FOR; LPAREN; var=IDENT; assign_op; start=expr; TO; stop=expr; RPAREN; body=brace_block
    { SFor (var, start, stop, body) }
  | WRITELN; LPAREN; args=separated_list(COMMA, expr); RPAREN
    { SWriteln args }
  | WRITE; LPAREN; args=separated_list(COMMA, expr); RPAREN
    { SWrite args }
  | READLN; LPAREN; vars=separated_list(COMMA, IDENT); RPAREN
    { SReadln vars }
  | RETURN; e=expr
    { SReturn (Some e) }
  | RETURN
    { SReturn None }
  | brace_block=brace_block
    { SBlock brace_block }

assign_op:
  | ASSIGN { () }
  | EQ { () }

(* lvalues for assignments *)
lvalue:
  | name=IDENT
    { EVar name }
  | e=lvalue; LBRACK; idx=expr; RBRACK
    { EArrayAccess (e, idx) }
  | e=lvalue; DOT; field=IDENT
    { ERecordAccess (e, field) }
  | e=lvalue; CARET
    { EDeref e }

(* Scala-like brace blocks with optional semicolons *)
brace_block:
  | LBRACE; stmts=statement_list_opt_semi; RBRACE
    { stmts }

statement_list_opt_semi:
  | (* empty *)
    { [] }
  | s=statement
    { [s] }
  | s=statement; SEMICOLON; rest=statement_list_opt_semi
    { s :: rest }
  | s=statement; rest=statement_list_opt_semi
    { s :: rest }

expr:
  | i=INTEGER
    { EInteger i }
  | f=REAL
    { EReal f }
  | TRUE
    { EBoolean true }
  | FALSE
    { EBoolean false }
  | c=CHAR
    { EChar c }
  | s=STRING
    { EString s }
  | name=IDENT
    { EVar name }
  | name=IDENT; LPAREN; args=separated_list(COMMA, expr); RPAREN
    { ECall (name, args) }
  | e=expr; LBRACK; idx=expr; RBRACK
    { EArrayAccess (e, idx) }
  | e=expr; DOT; field=IDENT
    { ERecordAccess (e, field) }
  | e=expr; CARET
    { EDeref e }
  | AT; e=expr
    { EAddress e }
  | NEW; LPAREN; t=type_expr; RPAREN
    { ENew t }
  | e1=expr; op=binop; e2=expr
    { EBinop (op, e1, e2) }
  | op=unop; e=expr %prec UNARY
    { EUnop (op, e) }
  | LPAREN; e=expr; RPAREN
    { e }

%inline binop:
  | PLUS   { Add }
  | MINUS  { Sub }
  | STAR   { Mul }
  | SLASH  { Div }
  | MOD    { Mod }
  | EQ     { Eq }
  | NE     { Ne }
  | LT     { Lt }
  | LE     { Le }
  | GT     { Gt }
  | GE     { Ge }
  | AND    { And }
  | OR     { Or }

%inline unop:
  | MINUS  { Neg }
  | NOT    { Not }

%%
