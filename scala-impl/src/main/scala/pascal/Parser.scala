package pascal

import fastparse.*
import NoWhitespace.*
import TypeExpr.*
import Expr.*
import Stmt.*
import BinOp.*
import UnOp.*
import Declaration.*

object Parser:

  // Whitespace and comments
  def wsChars[$: P]: P[Unit] = P(CharsWhileIn(" \t\r\n"))
  def comment[$: P]: P[Unit] = P("(*" ~ (!"*)" ~ AnyChar).rep ~ "*)")
  def ws[$: P]: P[Unit] = P((wsChars | comment).rep)

  // Keywords
  val keywords = Set(
    "program", "begin", "end", "var", "val", "function", "procedure",
    "if", "then", "else", "while", "do", "for", "to", "downto",
    "return", "write", "writeln", "readln", "new",
    "integer", "real", "boolean", "char", "string",
    "true", "false", "and", "or", "not", "mod", "div",
    "type", "record", "array", "of", "pointer"
  )

  // Identifiers
  def letter[$: P]: P[Unit] = P(CharIn("a-zA-Z_"))
  def digit[$: P]: P[Unit] = P(CharIn("0-9"))
  def identifier[$: P]: P[Identifier] = P(
    (letter ~ (letter | digit).rep).!.filter(!keywords.contains(_))
  )

  // Literals
  def integerLit[$: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  def realLit[$: P]: P[Double] = P(
    (CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(_.toDouble)
  )
  def booleanLit[$: P]: P[Boolean] = P(
    "true".!.map(_ => true) | "false".!.map(_ => false)
  )
  def charLit[$: P]: P[Char] = P("'" ~ AnyChar ~ "'").!.map(s => s(1))
  def stringLit[$: P]: P[String] = P(
    "\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\""
  )

  // Type expressions
  def baseType[$: P]: P[TypeExpr] = P(
    "integer".!.map(_ => TInteger) |
    "real".!.map(_ => TReal) |
    "boolean".!.map(_ => TBoolean) |
    "char".!.map(_ => TChar) |
    "string".!.map(_ => TString)
  )

  def pointerType[$: P]: P[TypeExpr] = P(
    "^" ~ ws ~ typeExpr
  ).map(TPointer.apply)

  def arrayType[$: P]: P[TypeExpr] = P(
    "array" ~ ws ~ "[" ~ ws ~ integerLit ~ ws ~ "]" ~ ws ~ "of" ~ ws ~ typeExpr
  ).map((size, elemType) => TArray(elemType, size))

  def recordField[$: P]: P[RecordField] = P(
    identifier ~ ws ~ ":" ~ ws ~ typeExpr
  ).map((name, tpe) => RecordField(name, tpe))

  def recordType[$: P]: P[TypeExpr] = P(
    "record" ~ ws ~
    recordField.rep(sep = ws ~ ";" ~ ws) ~ ws ~
    "end"
  ).map(fields => TRecord(fields.toList))

  def namedType[$: P]: P[TypeExpr] = P(identifier).map(TNamed.apply)

  def typeExpr[$: P]: P[TypeExpr] = P(
    pointerType | arrayType | recordType | baseType | namedType
  )

  // Expressions (with precedence)
  def primaryExpr[$: P]: P[Expr] = P(
    realLit.map(EReal.apply) |
    integerLit.map(EInteger.apply) |
    booleanLit.map(EBoolean.apply) |
    charLit.map(EChar.apply) |
    stringLit.map(EString.apply) |
    ("new" ~ ws ~ "(" ~ ws ~ typeExpr ~ ws ~ ")").map(ENew.apply) |
    functionCall |
    ("@" ~ ws ~ identifier).map(id => EAddress(EVar(id))) |
    identifier.map(EVar.apply) |
    ("(" ~ ws ~ expr ~ ws ~ ")")
  )

  def postfixExpr[$: P]: P[Expr] = P(
    primaryExpr ~ (
      (ws ~ "^").map(_ => (e: Expr) => EDeref(e)) |
      (ws ~ "[" ~ ws ~ expr ~ ws ~ "]").map(idx => (e: Expr) => EArrayAccess(e, idx)) |
      (ws ~ "." ~ ws ~ identifier).map(field => (e: Expr) => ERecordAccess(e, field))
    ).rep
  ).map { case (base, ops) => ops.foldLeft(base)((e, op) => op(e)) }

  def unaryExpr[$: P]: P[Expr] = P(
    ("-" ~ ws ~ unaryExpr).map(e => EUnop(Neg, e)) |
    ("not" ~ ws ~ unaryExpr).map(e => EUnop(Not, e)) |
    postfixExpr
  )

  def mulExpr[$: P]: P[Expr] = P(
    unaryExpr ~ (
      (ws ~ "*" ~ ws ~ unaryExpr).map(r => (l: Expr) => EBinop(Mul, l, r)) |
      (ws ~ "div" ~ ws ~ unaryExpr).map(r => (l: Expr) => EBinop(Div, l, r)) |
      (ws ~ "mod" ~ ws ~ unaryExpr).map(r => (l: Expr) => EBinop(Mod, l, r))
    ).rep
  ).map { case (base, ops) => ops.foldLeft(base)((e, op) => op(e)) }

  def addExpr[$: P]: P[Expr] = P(
    mulExpr ~ (
      (ws ~ "+" ~ ws ~ mulExpr).map(r => (l: Expr) => EBinop(Add, l, r)) |
      (ws ~ "-" ~ ws ~ mulExpr).map(r => (l: Expr) => EBinop(Sub, l, r))
    ).rep
  ).map { case (base, ops) => ops.foldLeft(base)((e, op) => op(e)) }

  def cmpExpr[$: P]: P[Expr] = P(
    addExpr ~ (
      (ws ~ "=" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Eq, l, r)) |
      (ws ~ "<>" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Ne, l, r)) |
      (ws ~ "<=" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Le, l, r)) |
      (ws ~ ">=" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Ge, l, r)) |
      (ws ~ "<" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Lt, l, r)) |
      (ws ~ ">" ~ ws ~ addExpr).map(r => (l: Expr) => EBinop(Gt, l, r))
    ).?
  ).map {
    case (base, None) => base
    case (base, Some(op)) => op(base)
  }

  def andExpr[$: P]: P[Expr] = P(
    cmpExpr ~ (ws ~ "and" ~ ws ~ cmpExpr).rep
  ).map { case (base, rest) =>
    rest.foldLeft(base)((l, r) => EBinop(And, l, r))
  }

  def orExpr[$: P]: P[Expr] = P(
    andExpr ~ (ws ~ "or" ~ ws ~ andExpr).rep
  ).map { case (base, rest) =>
    rest.foldLeft(base)((l, r) => EBinop(Or, l, r))
  }

  def expr[$: P]: P[Expr] = P(orExpr)

  def functionCall[$: P]: P[Expr] = P(
    identifier ~ ws ~ "(" ~ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")"
  ).map((name, args) => ECall(name, args.toList))

  // Statements
  def assignStmt[$: P]: P[Stmt] = P(
    postfixExpr ~ ws ~ ":=" ~ ws ~ expr
  ).map((lhs, rhs) => SAssign(lhs, rhs))

  def callStmt[$: P]: P[Stmt] = P(
    identifier ~ ws ~ "(" ~ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")"
  ).map((name, args) => SCall(name, args.toList))

  def ifStmt[$: P]: P[Stmt] = P(
    "if" ~ ws ~ expr ~ ws ~ "then" ~ ws ~ stmtBlock ~
    (ws ~ "else" ~ ws ~ stmtBlock).?
  ).map((cond, thenBranch, elseBranch) => SIf(cond, thenBranch, elseBranch))

  def whileStmt[$: P]: P[Stmt] = P(
    "while" ~ ws ~ expr ~ ws ~ "do" ~ ws ~ stmtBlock
  ).map((cond, body) => SWhile(cond, body))

  def forStmt[$: P]: P[Stmt] = P(
    "for" ~ ws ~ identifier ~ ws ~ ":=" ~ ws ~ expr ~ ws ~ "to" ~ ws ~ expr ~ ws ~
    "do" ~ ws ~ stmtBlock
  ).map((varName, from, to, body) => SFor(varName, from, to, body))

  def writelnStmt[$: P]: P[Stmt] = P(
    "writeln" ~ ws ~ "(" ~ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")"
  ).map(exprs => SWriteln(exprs.toList))

  def writeStmt[$: P]: P[Stmt] = P(
    "write" ~ ws ~ "(" ~ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")"
  ).map(exprs => SWrite(exprs.toList))

  def readlnStmt[$: P]: P[Stmt] = P(
    "readln" ~ ws ~ "(" ~ ws ~ identifier.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")"
  ).map(ids => SReadln(ids.toList))

  def returnStmt[$: P]: P[Stmt] = P(
    "return" ~ (ws ~ expr).?
  ).map(SReturn.apply)

  def varDeclStmt[$: P]: P[Stmt] = P(
    "var" ~ ws ~ identifier ~ ws ~ ":" ~ ws ~ typeExpr ~ ws ~ ":=" ~ ws ~ expr
  ).map((name, tpe, init) => SVarDecl(name, tpe, init))

  def valDeclStmt[$: P]: P[Stmt] = P(
    "val" ~ ws ~ identifier ~ ws ~ ":" ~ ws ~ typeExpr ~ ws ~ "=" ~ ws ~ expr
  ).map((name, tpe, init) => SValDecl(name, tpe, init))

  def simpleStmt[$: P]: P[Stmt] = P(
    returnStmt | writelnStmt | writeStmt | readlnStmt |
    varDeclStmt | valDeclStmt | callStmt | assignStmt
  )

  def stmt[$: P]: P[Stmt] = P(
    ifStmt | whileStmt | forStmt |
    ("begin" ~ ws ~ stmt.rep(sep = ws ~ ";" ~ ws) ~ ws ~ "end").map(stmts => SBlock(stmts.toList)) |
    simpleStmt
  )

  def stmtBlock[$: P]: P[List[Stmt]] = P(
    ("begin" ~ ws ~ stmt.rep(sep = ws ~ ";" ~ ws) ~ ws ~ "end").map(_.toList) |
    stmt.map(List(_))
  )

  // Declarations
  def varDecl[$: P]: P[VarDecl] = P(
    identifier ~ ws ~ ":" ~ ws ~ typeExpr ~ (ws ~ ":=" ~ ws ~ expr).?
  ).map((name, tpe, init) => VarDecl(name, tpe, init))

  def param[$: P]: P[Param] = P(
    ("var" ~ ws).!.? ~ identifier ~ ws ~ ":" ~ ws ~ typeExpr
  ).map((isVar, name, tpe) => Param(name, tpe, isVar.isDefined))

  def funcDecl[$: P]: P[FuncDecl] = P(
    "function" ~ ws ~ identifier ~ ws ~
    "(" ~ ws ~ param.rep(sep = ws ~ ";" ~ ws) ~ ws ~ ")" ~ ws ~
    ":" ~ ws ~ typeExpr ~ ws ~ ";" ~ ws ~
    ((ws ~ "var" ~ ws ~ varDecl.rep(sep = ws ~ ";" ~ ws, min = 1) ~ ws ~ ";").map(_.toList) | Pass.map(_ => Nil)) ~ ws ~
    "begin" ~ ws ~ stmt.rep(sep = ws ~ ";" ~ ws) ~ ws ~ "end" ~ ws ~ ";"
  ).map((name, params, retType, localVars, body) =>
    FuncDecl(name, params.toList, Some(retType), localVars, body.toList))

  def procedureDecl[$: P]: P[FuncDecl] = P(
    "procedure" ~ ws ~ identifier ~ ws ~
    "(" ~ ws ~ param.rep(sep = ws ~ ";" ~ ws) ~ ws ~ ")" ~ ws ~ ";" ~ ws ~
    ((ws ~ "var" ~ ws ~ varDecl.rep(sep = ws ~ ";" ~ ws, min = 1) ~ ws ~ ";").map(_.toList) | Pass.map(_ => Nil)) ~ ws ~
    "begin" ~ ws ~ stmt.rep(sep = ws ~ ";" ~ ws) ~ ws ~ "end" ~ ws ~ ";"
  ).map((name, params, localVars, body) =>
    FuncDecl(name, params.toList, None, localVars, body.toList))

  def typeDecl[$: P]: P[TypeDecl] = P(
    "type" ~ ws ~ identifier ~ ws ~ "=" ~ ws ~ typeExpr
  ).map((name, tpe) => TypeDecl(name, tpe))

  def declaration[$: P]: P[Declaration] = P(
    typeDecl.map(DType.apply) |
    funcDecl.map(DFunc.apply) |
    procedureDecl.map(DFunc.apply) |
    ("var" ~ ws ~ varDecl.rep(sep = ws ~ ";" ~ ws)).map(vars =>
      vars.map(v => DVar(v)).toList
    ).flatMap(decls => Pass.map(_ => decls.head)) // Parse first, handle multiple separately
  )

  // Program with multiple var declarations
  def programVarDecls[$: P]: P[List[VarDecl]] = P(
    "var" ~ ws ~ varDecl.rep(sep = ws ~ ";" ~ ws)
  ).map(_.toList)

  def programDeclarations[$: P]: P[List[Declaration]] = P(
    (ws ~
     (programVarDecls.map(vars => vars.map(DVar.apply)) |
      typeDecl.map(td => List(DType(td))) |
      funcDecl.map(fd => List(DFunc(fd))) |
      procedureDecl.map(pd => List(DFunc(pd)))
     )
    ).rep
  ).map(_.flatten.toList)

  // Main program block (without function declarations)
  def mainBlock[$: P]: P[List[Stmt]] = P(
    "begin" ~ ws ~ stmt.rep(sep = ws ~ ";" ~ ws) ~ ws ~ "end"
  ).map(_.toList)

  def program[$: P]: P[Program] = P(
    ws ~ "program" ~ ws ~ identifier ~ ws ~ ";" ~ ws ~
    programDeclarations ~ ws ~
    (mainBlock.map(stmts =>
      // Wrap main block in a main() function
      List(DFunc(FuncDecl("main", Nil, Some(TInteger), Nil, stmts :+ SReturn(Some(EInteger(0))))))
    ) | Pass.map(_ => Nil)) ~ ws ~
    "." ~ ws
  ).map { case (name, decls, mainFunc) =>
    Program(name, decls ++ mainFunc)
  }

  def parseProgram(input: String): Either[String, Program] =
    parse(input, p => program(using p)) match
      case Parsed.Success(prog, _) => Right(prog)
      case f: Parsed.Failure => Left(f.trace().longMsg)
