/// Abstract Syntax Tree for Pascal-like language

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    TInteger,
    TReal,
    TBoolean,
    TChar,
    TString,
    TPointer(Box<TypeExpr>),
    TArray(Box<TypeExpr>, usize),
    TRecord(Vec<RecordField>),
    TNamed(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub field_name: Identifier,
    pub field_type: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    EInteger(i32),
    EReal(f64),
    EBoolean(bool),
    EChar(char),
    EString(String),
    EVar(Identifier),
    EBinop(BinOp, Box<Expr>, Box<Expr>),
    EUnop(UnOp, Box<Expr>),
    ECall(Identifier, Vec<Expr>),
    EArrayAccess(Box<Expr>, Box<Expr>),
    ERecordAccess(Box<Expr>, Identifier),
    EDeref(Box<Expr>),
    EAddress(Box<Expr>),
    ENew(TypeExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    SAssign(Expr, Expr),
    SCall(Identifier, Vec<Expr>),
    SIf(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    SWhile(Expr, Vec<Stmt>),
    SFor(Identifier, Expr, Expr, Vec<Stmt>),
    SWriteln(Vec<Expr>),
    SWrite(Vec<Expr>),
    SReadln(Vec<Identifier>),
    SReturn(Option<Expr>),
    SBlock(Vec<Stmt>),
    SVarDecl(Identifier, TypeExpr, Expr),
    SValDecl(Identifier, TypeExpr, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub param_name: Identifier,
    pub param_type: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub var_name: Identifier,
    pub var_type: TypeExpr,
    pub var_init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub func_name: Identifier,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub local_vars: Vec<VarDecl>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub type_name: Identifier,
    pub type_def: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    DType(TypeDecl),
    DVar(VarDecl),
    DFunc(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub program_name: Identifier,
    pub declarations: Vec<Declaration>,
}
