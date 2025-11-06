package pascal

/** Abstract Syntax Tree for Pascal-like language */

type Identifier = String

/** Type expressions */
enum TypeExpr:
  case TInteger
  case TReal
  case TBoolean
  case TChar
  case TString
  case TPointer(elementType: TypeExpr)
  case TArray(elementType: TypeExpr, size: Int)
  case TRecord(fields: List[RecordField])
  case TNamed(name: Identifier)

case class RecordField(
  fieldName: Identifier,
  fieldType: TypeExpr
)

/** Binary operators */
enum BinOp:
  case Add, Sub, Mul, Div, Mod
  case Eq, Ne, Lt, Le, Gt, Ge
  case And, Or

/** Unary operators */
enum UnOp:
  case Neg
  case Not

/** Expressions */
enum Expr:
  case EInteger(value: Int)
  case EReal(value: Double)
  case EBoolean(value: Boolean)
  case EChar(value: Char)
  case EString(value: String)
  case EVar(name: Identifier)
  case EBinop(op: BinOp, left: Expr, right: Expr)
  case EUnop(op: UnOp, operand: Expr)
  case ECall(funcName: Identifier, args: List[Expr])
  case EArrayAccess(array: Expr, index: Expr)
  case ERecordAccess(record: Expr, field: Identifier)
  case EDeref(pointer: Expr)
  case EAddress(expr: Expr)
  case ENew(typeExpr: TypeExpr)

/** Statements */
enum Stmt:
  case SAssign(lvalue: Expr, rvalue: Expr)
  case SCall(funcName: Identifier, args: List[Expr])
  case SIf(condition: Expr, thenBranch: List[Stmt], elseBranch: Option[List[Stmt]])
  case SWhile(condition: Expr, body: List[Stmt])
  case SFor(varName: Identifier, from: Expr, to: Expr, body: List[Stmt])
  case SWriteln(exprs: List[Expr])
  case SWrite(exprs: List[Expr])
  case SReadln(vars: List[Identifier])
  case SReturn(value: Option[Expr])
  case SBlock(stmts: List[Stmt])
  case SVarDecl(varName: Identifier, varType: TypeExpr, init: Expr)
  case SValDecl(varName: Identifier, varType: TypeExpr, init: Expr)

/** Variable declaration */
case class VarDecl(
  varName: Identifier,
  varType: TypeExpr,
  varInit: Option[Expr]
)

/** Function parameter */
case class Param(
  paramName: Identifier,
  paramType: TypeExpr,
  isVar: Boolean  // VAR parameter (by reference)
)

/** Function declaration */
case class FuncDecl(
  funcName: Identifier,
  params: List[Param],
  returnType: Option[TypeExpr],  // None for procedures
  localVars: List[VarDecl],
  body: List[Stmt]
)

/** Type declaration */
case class TypeDecl(
  typeName: Identifier,
  typeDef: TypeExpr
)

/** Top-level declarations */
enum Declaration:
  case DType(decl: TypeDecl)
  case DVar(decl: VarDecl)
  case DFunc(decl: FuncDecl)

/** Complete program */
case class Program(
  programName: Identifier,
  declarations: List[Declaration]
)
