%{
open Ast
%}

%token <int> INTEGER
%token <float> REAL
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token TRUE FALSE
%token PROGRAM VAR TYPE FUNCTION PROCEDURE BEGIN END
%token IF THEN ELSE WHILE DO FOR TO DOWNTO OF ARRAY RECORD
%token TINTEGER TREAL TBOOLEAN TCHAR TSTRING
%token AND OR NOT DIV MOD
%token WRITELN WRITE READLN NEW RETURN
%token LPAREN RPAREN LBRACK RBRACK
%token DOT COMMA COLON SEMICOLON ASSIGN DOTDOT
%token EQ NE LT LE GT GE
%token PLUS MINUS STAR SLASH CARET AT
%token EOF

%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH DIV MOD
%nonassoc UNARY
%nonassoc CARET AT

%start <Ast.program> program

%%

program:
  | PROGRAM; name=IDENT; SEMICOLON; decls=list(declaration); DOT; EOF
    { { program_name = name; declarations = decls } }

declaration:
  | TYPE; type_decls=nonempty_list(type_declaration)
    { DType (List.hd type_decls) }  (* Simplified for single type *)
  | VAR; var_decls=nonempty_list(var_declaration)
    { DVar (List.hd var_decls) }  (* Simplified for single var *)
  | f=function_declaration
    { DFunc f }
  | p=procedure_declaration
    { DFunc p }

type_declaration:
  | name=IDENT; EQ; t=type_expr; SEMICOLON
    { { type_name = name; type_def = t } }

var_declaration:
  | name=IDENT; COLON; t=type_expr; SEMICOLON
    { { var_name = name; var_type = t; var_init = None } }
  | name=IDENT; COLON; t=type_expr; ASSIGN; init=expr; SEMICOLON
    { { var_name = name; var_type = t; var_init = Some init } }

function_declaration:
  | FUNCTION; name=IDENT; LPAREN; params=separated_list(SEMICOLON, param); RPAREN;
    COLON; ret=type_expr; SEMICOLON;
    vars=list(VAR; v=nonempty_list(var_declaration) { v });
    BEGIN; body=separated_list(SEMICOLON, statement); END; SEMICOLON
    { { func_name = name; params; return_type = Some ret;
        local_vars = List.flatten vars; body } }

procedure_declaration:
  | PROCEDURE; name=IDENT; LPAREN; params=separated_list(SEMICOLON, param); RPAREN; SEMICOLON;
    vars=list(VAR; v=nonempty_list(var_declaration) { v });
    BEGIN; body=separated_list(SEMICOLON, statement); END; SEMICOLON
    { { func_name = name; params; return_type = None;
        local_vars = List.flatten vars; body } }

param:
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
  | RECORD; fields=nonempty_list(record_field); END
    { TRecord fields }
  | name=IDENT
    { TNamed name }

record_field:
  | name=IDENT; COLON; t=type_expr; SEMICOLON
    { { field_name = name; field_type = t } }

statement:
  | lval=expr; ASSIGN; rval=expr
    { SAssign (lval, rval) }
  | name=IDENT; LPAREN; args=separated_list(COMMA, expr); RPAREN
    { SCall (name, args) }
  | IF; cond=expr; THEN; then_body=statement_block; ELSE; else_body=statement_block
    { SIf (cond, then_body, Some else_body) }
  | IF; cond=expr; THEN; then_body=statement_block
    { SIf (cond, then_body, None) }
  | WHILE; cond=expr; DO; body=statement_block
    { SWhile (cond, body) }
  | FOR; var=IDENT; ASSIGN; start=expr; TO; stop=expr; DO; body=statement_block
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
  | BEGIN; stmts=separated_list(SEMICOLON, statement); END
    { SBlock stmts }

statement_block:
  | BEGIN; stmts=separated_list(SEMICOLON, statement); END
    { stmts }
  | s=statement
    { [s] }

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
