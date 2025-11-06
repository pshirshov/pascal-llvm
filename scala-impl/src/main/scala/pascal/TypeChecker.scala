package pascal

import scala.collection.mutable
import TypeExpr.*
import Expr.*
import Stmt.*
import BinOp.*
import UnOp.*
import Declaration.*

case class TypeError(message: String) extends Exception(message)

case class SymbolTable(
  types: mutable.HashMap[Identifier, TypeExpr] = mutable.HashMap(),
  vars: mutable.HashMap[Identifier, TypeExpr] = mutable.HashMap(),
  funcs: mutable.HashMap[Identifier, (List[Param], Option[TypeExpr])] = mutable.HashMap(),
  vals: mutable.HashSet[Identifier] = mutable.HashSet()
):
  def copy(): SymbolTable = SymbolTable(
    types.clone(),
    vars.clone(),
    funcs,  // Share function table across scopes
    vals.clone()
  )

object TypeChecker:

  def typesEqual(t1: TypeExpr, t2: TypeExpr): Boolean = (t1, t2) match
    case (TInteger, TInteger) | (TReal, TReal) | (TBoolean, TBoolean) |
         (TChar, TChar) | (TString, TString) => true
    case (TPointer(elem1), TPointer(elem2)) => typesEqual(elem1, elem2)
    case (TArray(elem1, size1), TArray(elem2, size2)) =>
      size1 == size2 && typesEqual(elem1, elem2)
    case (TRecord(fields1), TRecord(fields2)) =>
      fields1.length == fields2.length &&
      fields1.zip(fields2).forall { case (f1, f2) =>
        f1.fieldName == f2.fieldName && typesEqual(f1.fieldType, f2.fieldType)
      }
    case (TNamed(n1), TNamed(n2)) => n1 == n2
    case _ => false

  def resolveType(symtab: SymbolTable, t: TypeExpr): TypeExpr = t match
    case TNamed(name) =>
      symtab.types.getOrElse(name,
        throw TypeError(s"Undefined type: $name"))
    case _ => t

  def checkExpr(symtab: SymbolTable, expr: Expr): TypeExpr = expr match
    case EInteger(_) => TInteger
    case EReal(_) => TReal
    case EBoolean(_) => TBoolean
    case EChar(_) => TChar
    case EString(_) => TString

    case EVar(name) =>
      symtab.vars.getOrElse(name,
        throw TypeError(s"Undefined variable: $name"))

    case EBinop(op, e1, e2) =>
      val t1 = checkExpr(symtab, e1)
      val t2 = checkExpr(symtab, e2)
      op match
        case Add | Sub | Mul | Div | Mod =>
          if !((typesEqual(t1, TInteger) && typesEqual(t2, TInteger)) ||
               (typesEqual(t1, TReal) && typesEqual(t2, TReal))) then
            throw TypeError("Arithmetic operators require numeric operands")
          t1
        case Eq | Ne | Lt | Le | Gt | Ge =>
          if !typesEqual(t1, t2) then
            throw TypeError("Comparison operators require same types")
          TBoolean
        case And | Or =>
          if !(typesEqual(t1, TBoolean) && typesEqual(t2, TBoolean)) then
            throw TypeError("Logical operators require boolean operands")
          TBoolean

    case EUnop(op, e) =>
      val t = checkExpr(symtab, e)
      op match
        case Neg =>
          if !(typesEqual(t, TInteger) || typesEqual(t, TReal)) then
            throw TypeError("Negation requires numeric operand")
          t
        case Not =>
          if !typesEqual(t, TBoolean) then
            throw TypeError("Not requires boolean operand")
          TBoolean

    case ECall(name, args) =>
      symtab.funcs.get(name) match
        case Some((params, retType)) =>
          if args.length != params.length then
            throw TypeError(s"Function $name: wrong number of arguments")
          args.zip(params).foreach { case (arg, param) =>
            val argType = checkExpr(symtab, arg)
            val paramType = resolveType(symtab, param.paramType)
            if !typesEqual(argType, paramType) then
              throw TypeError(s"Function $name: argument type mismatch")
          }
          retType.getOrElse(
            throw TypeError(s"$name is a procedure, not a function"))
        case None =>
          throw TypeError(s"Undefined function: $name")

    case EArrayAccess(arr, idx) =>
      val arrType = checkExpr(symtab, arr)
      val idxType = checkExpr(symtab, idx)
      if !typesEqual(idxType, TInteger) then
        throw TypeError("Array index must be integer")
      arrType match
        case TArray(elemType, _) => elemType
        case _ => throw TypeError("Array access on non-array type")

    case ERecordAccess(recExpr, field) =>
      val recType = checkExpr(symtab, recExpr)
      recType match
        case TRecord(fields) =>
          fields.find(_.fieldName == field) match
            case Some(f) => f.fieldType
            case None => throw TypeError(s"Record has no field: $field")
        case _ => throw TypeError("Record access on non-record type")

    case EDeref(ptr) =>
      val ptrType = checkExpr(symtab, ptr)
      ptrType match
        case TPointer(t) => t
        case _ => throw TypeError("Dereference on non-pointer type")

    case EAddress(e) =>
      val t = checkExpr(symtab, e)
      TPointer(t)

    case ENew(t) =>
      val resolved = resolveType(symtab, t)
      TPointer(resolved)

  def checkStmt(symtab: SymbolTable, returnType: Option[TypeExpr], stmt: Stmt): Unit = stmt match
    case SVarDecl(name, varType, init) =>
      val resolved = resolveType(symtab, varType)
      val initType = checkExpr(symtab, init)
      if !typesEqual(resolved, initType) then
        throw TypeError(s"Variable $name: initializer type mismatch")
      symtab.vars(name) = resolved

    case SValDecl(name, varType, init) =>
      val resolved = resolveType(symtab, varType)
      val initType = checkExpr(symtab, init)
      if !typesEqual(resolved, initType) then
        throw TypeError(s"Val $name: initializer type mismatch")
      symtab.vars(name) = resolved
      symtab.vals += name

    case SAssign(lval, rval) =>
      // Check if trying to assign to a val
      lval match
        case EVar(name) =>
          if symtab.vals.contains(name) then
            throw TypeError(s"Cannot assign to val: $name")
        case _ => ()
      val lvalType = resolveType(symtab, checkExpr(symtab, lval))
      val rvalType = resolveType(symtab, checkExpr(symtab, rval))
      if !typesEqual(lvalType, rvalType) then
        throw TypeError("Assignment type mismatch")

    case SCall(name, args) =>
      symtab.funcs.get(name) match
        case Some((params, _)) =>
          if args.length != params.length then
            throw TypeError(s"Procedure $name: wrong number of arguments")
          args.zip(params).foreach { case (arg, param) =>
            val argType = checkExpr(symtab, arg)
            if !typesEqual(argType, param.paramType) then
              throw TypeError(s"Procedure $name: argument type mismatch")
          }
        case None =>
          throw TypeError(s"Undefined procedure: $name")

    case SIf(cond, thenBranch, elseBranch) =>
      val condType = checkExpr(symtab, cond)
      if !typesEqual(condType, TBoolean) then
        throw TypeError("If condition must be boolean")
      thenBranch.foreach(checkStmt(symtab, returnType, _))
      elseBranch.foreach(_.foreach(checkStmt(symtab, returnType, _)))

    case SWhile(cond, body) =>
      val condType = checkExpr(symtab, cond)
      if !typesEqual(condType, TBoolean) then
        throw TypeError("While condition must be boolean")
      body.foreach(checkStmt(symtab, returnType, _))

    case SFor(_, start, stop, body) =>
      val startType = checkExpr(symtab, start)
      val stopType = checkExpr(symtab, stop)
      if !(typesEqual(startType, TInteger) && typesEqual(stopType, TInteger)) then
        throw TypeError("For loop bounds must be integers")
      body.foreach(checkStmt(symtab, returnType, _))

    case SWriteln(exprs) =>
      exprs.foreach(e => checkExpr(symtab, e))

    case SWrite(exprs) =>
      exprs.foreach(e => checkExpr(symtab, e))

    case SReadln(vars) =>
      vars.foreach { v =>
        if !symtab.vars.contains(v) then
          throw TypeError(s"Undefined variable: $v")
      }

    case SReturn(exprOpt) =>
      (exprOpt, returnType) match
        case (Some(e), Some(expectedType)) =>
          val actualType = checkExpr(symtab, e)
          if !typesEqual(actualType, expectedType) then
            throw TypeError("Return type mismatch")
        case (None, None) => ()
        case (Some(_), None) =>
          throw TypeError("Procedure should not return a value")
        case (None, Some(_)) =>
          throw TypeError("Function must return a value")

    case SBlock(stmts) =>
      stmts.foreach(checkStmt(symtab, returnType, _))

  def checkProgram(prog: Program): Unit =
    val symtab = SymbolTable()

    // First pass: collect type and function declarations
    prog.declarations.foreach {
      case DType(TypeDecl(typeName, typeDef)) =>
        symtab.types(typeName) = typeDef
      case DFunc(FuncDecl(funcName, params, returnType, _, _)) =>
        symtab.funcs(funcName) = (params, returnType)
      case DVar(_) => ()
    }

    // Second pass: collect global variables and check functions
    prog.declarations.foreach {
      case DVar(VarDecl(varName, varType, varInit)) =>
        val resolved = resolveType(symtab, varType)
        symtab.vars(varName) = resolved
        varInit.foreach { init =>
          val initType = checkExpr(symtab, init)
          if !typesEqual(resolved, initType) then
            throw TypeError(s"Variable $varName: initializer type mismatch")
        }

      case DFunc(func @ FuncDecl(funcName, params, returnType, localVars, body)) =>
        // Create function-local symbol table
        val localSymtab = symtab.copy()

        // Add parameters to local scope
        params.foreach { param =>
          val paramType = resolveType(symtab, param.paramType)
          localSymtab.vars(param.paramName) = paramType
        }

        // Add local variables
        localVars.foreach { v =>
          val varType = resolveType(symtab, v.varType)
          localSymtab.vars(v.varName) = varType
          v.varInit.foreach { init =>
            val initType = checkExpr(localSymtab, init)
            if !typesEqual(varType, initType) then
              throw TypeError(s"Variable ${v.varName}: initializer type mismatch")
          }
        }

        // Check function body
        val resolvedRetType = returnType.map(resolveType(symtab, _))
        body.foreach(checkStmt(localSymtab, resolvedRetType, _))

      case DType(_) => ()
    }
