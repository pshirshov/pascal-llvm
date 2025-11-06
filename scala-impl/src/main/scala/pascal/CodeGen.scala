package pascal

import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM.*
import org.bytedeco.javacpp.{BytePointer, PointerPointer}
import scala.collection.mutable
import TypeExpr.*
import Expr.*
import Stmt.*
import BinOp.*
import UnOp.*
import Declaration.*

case class CodegenError(message: String) extends Exception(message)

case class CodegenContext(
  context: LLVMContextRef,
  module: LLVMModuleRef,
  builder: LLVMBuilderRef,
  namedValues: mutable.HashMap[Identifier, LLVMValueRef] = mutable.HashMap(),
  typeCache: mutable.HashMap[TypeExpr, LLVMTypeRef] = mutable.HashMap(),
  funcTypes: mutable.HashMap[Identifier, LLVMTypeRef] = mutable.HashMap(),
  typeDefs: mutable.HashMap[Identifier, TypeExpr] = mutable.HashMap(),
  varTypes: mutable.HashMap[Identifier, TypeExpr] = mutable.HashMap()
)

object CodeGen:

  def createContext(moduleName: String): CodegenContext =
    val context = LLVMContextCreate()
    val module = LLVMModuleCreateWithNameInContext(moduleName, context)
    val builder = LLVMCreateBuilderInContext(context)
    CodegenContext(context, module, builder)

  def resolveType(ctx: CodegenContext, ty: TypeExpr): TypeExpr = ty match
    case TNamed(name) =>
      ctx.typeDefs.get(name) match
        case Some(resolved) => resolveType(ctx, resolved)
        case None => throw CodegenError(s"Undefined type: $name")
    case _ => ty

  def llTypeOfType(ctx: CodegenContext, ty: TypeExpr): LLVMTypeRef =
    ctx.typeCache.getOrElseUpdate(ty, {
      ty match
        case TInteger => LLVMInt32TypeInContext(ctx.context)
        case TReal => LLVMDoubleTypeInContext(ctx.context)
        case TBoolean => LLVMInt1TypeInContext(ctx.context)
        case TChar => LLVMInt8TypeInContext(ctx.context)
        case TString => LLVMPointerTypeInContext(ctx.context, 0)
        case TPointer(_) => LLVMPointerTypeInContext(ctx.context, 0)
        case TArray(elemType, size) =>
          LLVMArrayType2(llTypeOfType(ctx, elemType), size.toLong)
        case TRecord(fields) =>
          val fieldTypes = new PointerPointer[LLVMTypeRef](fields.size.toLong)
          fields.zipWithIndex.foreach { case (field, idx) =>
            fieldTypes.put(idx.toLong, llTypeOfType(ctx, field.fieldType))
          }
          LLVMStructTypeInContext(ctx.context, fieldTypes, fields.size, 0)
        case TNamed(name) =>
          ctx.typeDefs.get(name) match
            case Some(resolvedType) => llTypeOfType(ctx, resolvedType)
            case None => throw CodegenError(s"Unresolved named type: $name")
    })

  def getFieldIndex(fields: List[RecordField], fieldName: Identifier): Int =
    fields.indexWhere(_.fieldName == fieldName) match
      case -1 => throw CodegenError(s"Field $fieldName not found")
      case idx => idx

  def declareRuntimeFunctions(ctx: CodegenContext): (LLVMValueRef, LLVMValueRef, LLVMValueRef) =
    // printf: i32 (ptr, ...)
    val printfType = LLVMFunctionType(
      LLVMInt32TypeInContext(ctx.context),
      new PointerPointer[LLVMTypeRef](1).put(0, LLVMPointerTypeInContext(ctx.context, 0)),
      1,
      1  // vararg
    )
    ctx.funcTypes("printf") = printfType
    val printfFunc = LLVMAddFunction(ctx.module, "printf", printfType)

    // scanf: i32 (ptr, ...)
    val scanfType = LLVMFunctionType(
      LLVMInt32TypeInContext(ctx.context),
      new PointerPointer[LLVMTypeRef](1).put(0, LLVMPointerTypeInContext(ctx.context, 0)),
      1,
      1  // vararg
    )
    ctx.funcTypes("scanf") = scanfType
    val scanfFunc = LLVMAddFunction(ctx.module, "scanf", scanfType)

    // malloc: ptr (i64)
    val mallocType = LLVMFunctionType(
      LLVMPointerTypeInContext(ctx.context, 0),
      new PointerPointer[LLVMTypeRef](1).put(0, LLVMInt64TypeInContext(ctx.context)),
      1,
      0  // not vararg
    )
    ctx.funcTypes("malloc") = mallocType
    val mallocFunc = LLVMAddFunction(ctx.module, "malloc", mallocType)

    (printfFunc, scanfFunc, mallocFunc)

  var stringCounter = 0
  def buildStringLiteral(ctx: CodegenContext, str: String): LLVMValueRef =
    val strConst = LLVMConstStringInContext2(ctx.context, str, str.length.toLong, 0)
    stringCounter += 1
    val global = LLVMAddGlobal(ctx.module, LLVMTypeOf(strConst), s"str_$stringCounter")
    LLVMSetInitializer(global, strConst)
    LLVMSetGlobalConstant(global, 1)
    val zero = LLVMConstInt(LLVMInt32TypeInContext(ctx.context), 0, 0)
    val indices = new PointerPointer[LLVMValueRef](2).put(0, zero).put(1, zero)
    LLVMConstInBoundsGEP2(LLVMTypeOf(strConst), global, indices, 2)

  def codegenExpr(ctx: CodegenContext, expr: Expr): LLVMValueRef = expr match
    case EInteger(i) => LLVMConstInt(LLVMInt32TypeInContext(ctx.context), i.toLong, 0)
    case EReal(f) => LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)
    case EBoolean(b) => LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if b then 1 else 0, 0)
    case EChar(c) => LLVMConstInt(LLVMInt8TypeInContext(ctx.context), c.toLong, 0)
    case EString(s) => buildStringLiteral(ctx, s)

    case EVar(name) =>
      ctx.namedValues.get(name) match
        case Some(ptr) =>
          val varType = ctx.varTypes.getOrElse(name, TInteger)
          val resolvedType = resolveType(ctx, varType)
          LLVMBuildLoad2(ctx.builder, llTypeOfType(ctx, resolvedType), ptr, name)
        case None => throw CodegenError(s"Unknown variable: $name")

    case EBinop(op, e1, e2) =>
      val lhs = codegenExpr(ctx, e1)
      val rhs = codegenExpr(ctx, e2)
      op match
        case Add => LLVMBuildAdd(ctx.builder, lhs, rhs, "addtmp")
        case Sub => LLVMBuildSub(ctx.builder, lhs, rhs, "subtmp")
        case Mul => LLVMBuildMul(ctx.builder, lhs, rhs, "multmp")
        case Div => LLVMBuildSDiv(ctx.builder, lhs, rhs, "divtmp")
        case Mod => LLVMBuildSRem(ctx.builder, lhs, rhs, "modtmp")
        case Eq => LLVMBuildICmp(ctx.builder, LLVMIntEQ, lhs, rhs, "eqtmp")
        case Ne => LLVMBuildICmp(ctx.builder, LLVMIntNE, lhs, rhs, "netmp")
        case Lt => LLVMBuildICmp(ctx.builder, LLVMIntSLT, lhs, rhs, "lttmp")
        case Le => LLVMBuildICmp(ctx.builder, LLVMIntSLE, lhs, rhs, "letmp")
        case Gt => LLVMBuildICmp(ctx.builder, LLVMIntSGT, lhs, rhs, "gttmp")
        case Ge => LLVMBuildICmp(ctx.builder, LLVMIntSGE, lhs, rhs, "getmp")
        case And => LLVMBuildAnd(ctx.builder, lhs, rhs, "andtmp")
        case Or => LLVMBuildOr(ctx.builder, lhs, rhs, "ortmp")

    case EUnop(op, e) =>
      val value = codegenExpr(ctx, e)
      op match
        case Neg => LLVMBuildNeg(ctx.builder, value, "negtmp")
        case Not => LLVMBuildNot(ctx.builder, value, "nottmp")

    case ECall(name, args) =>
      val callee = LLVMGetNamedFunction(ctx.module, name)
      if callee == null then
        throw CodegenError(s"Unknown function: $name")
      val fnType = ctx.funcTypes.getOrElse(name,
        throw CodegenError(s"Function type not found: $name"))
      val argsArray = new PointerPointer[LLVMValueRef](args.size.toLong)
      args.zipWithIndex.foreach { case (arg, idx) =>
        argsArray.put(idx.toLong, codegenExpr(ctx, arg))
      }
      LLVMBuildCall2(ctx.builder, fnType, callee, argsArray, args.size, "calltmp")

    case EArrayAccess(arr, idx) =>
      val arrVal = codegenExpr(ctx, arr)
      val idxVal = codegenExpr(ctx, idx)
      val indices = new PointerPointer[LLVMValueRef](1).put(0, idxVal)
      val elemPtr = LLVMBuildGEP2(ctx.builder, LLVMTypeOf(arrVal), arrVal, indices, 1, "arrayptr")
      LLVMBuildLoad2(ctx.builder, LLVMInt32TypeInContext(ctx.context), elemPtr, "arrayval")

    case ERecordAccess(rec, field) =>
      val recVal = codegenExpr(ctx, rec)
      // Need to track record type to get field index
      throw CodegenError("Record access not yet implemented")

    case EDeref(ptr) =>
      val ptrVal = codegenExpr(ctx, ptr)
      LLVMBuildLoad2(ctx.builder, LLVMPointerTypeInContext(ctx.context, 0), ptrVal, "deref")

    case EAddress(e) =>
      e match
        case EVar(name) =>
          ctx.namedValues.getOrElse(name,
            throw CodegenError(s"Unknown variable: $name"))
        case _ => throw CodegenError("Can only take address of variables")

    case ENew(t) =>
      val resolved = resolveType(ctx, t)
      val llType = llTypeOfType(ctx, resolved)
      val size = LLVMSizeOf(llType)
      val mallocFunc = LLVMGetNamedFunction(ctx.module, "malloc")
      val args = new PointerPointer[LLVMValueRef](1).put(0, size)
      LLVMBuildCall2(ctx.builder, ctx.funcTypes("malloc"), mallocFunc, args, 1, "newtmp")

  def codegenStmt(ctx: CodegenContext, stmt: Stmt): Unit = stmt match
    case SVarDecl(name, varType, init) =>
      val resolved = resolveType(ctx, varType)
      val llType = llTypeOfType(ctx, resolved)
      val alloca = LLVMBuildAlloca(ctx.builder, llType, name)
      ctx.namedValues(name) = alloca
      ctx.varTypes(name) = resolved
      val initVal = codegenExpr(ctx, init)
      LLVMBuildStore(ctx.builder, initVal, alloca)

    case SValDecl(name, varType, init) =>
      // Same as var but tracked separately (already checked in type checker)
      val resolved = resolveType(ctx, varType)
      val llType = llTypeOfType(ctx, resolved)
      val alloca = LLVMBuildAlloca(ctx.builder, llType, name)
      ctx.namedValues(name) = alloca
      ctx.varTypes(name) = resolved
      val initVal = codegenExpr(ctx, init)
      LLVMBuildStore(ctx.builder, initVal, alloca)

    case SAssign(lval, rval) =>
      val ptr = lval match
        case EVar(name) =>
          ctx.namedValues.getOrElse(name,
            throw CodegenError(s"Unknown variable: $name"))
        case _ => throw CodegenError("Can only assign to variables")
      val rvalExpr = codegenExpr(ctx, rval)
      LLVMBuildStore(ctx.builder, rvalExpr, ptr)

    case SCall(name, args) =>
      codegenExpr(ctx, ECall(name, args))
      ()

    case SIf(cond, thenBranch, elseBranch) =>
      val condVal = codegenExpr(ctx, cond)
      val func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx.builder))
      val thenBB = LLVMAppendBasicBlockInContext(ctx.context, func, "then")
      val mergeBB = LLVMAppendBasicBlockInContext(ctx.context, func, "ifcont")

      elseBranch match
        case Some(stmts) =>
          val elseBB = LLVMAppendBasicBlockInContext(ctx.context, func, "else")
          LLVMBuildCondBr(ctx.builder, condVal, thenBB, elseBB)

          LLVMPositionBuilderAtEnd(ctx.builder, thenBB)
          thenBranch.foreach(codegenStmt(ctx, _))
          if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(ctx.builder)) == null then
            LLVMBuildBr(ctx.builder, mergeBB)

          LLVMPositionBuilderAtEnd(ctx.builder, elseBB)
          stmts.foreach(codegenStmt(ctx, _))
          if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(ctx.builder)) == null then
            LLVMBuildBr(ctx.builder, mergeBB)

        case None =>
          LLVMBuildCondBr(ctx.builder, condVal, thenBB, mergeBB)

          LLVMPositionBuilderAtEnd(ctx.builder, thenBB)
          thenBranch.foreach(codegenStmt(ctx, _))
          if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(ctx.builder)) == null then
            LLVMBuildBr(ctx.builder, mergeBB)

      LLVMPositionBuilderAtEnd(ctx.builder, mergeBB)

    case SWhile(cond, body) =>
      val func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx.builder))
      val condBB = LLVMAppendBasicBlockInContext(ctx.context, func, "whilecond")
      val loopBB = LLVMAppendBasicBlockInContext(ctx.context, func, "whileloop")
      val afterBB = LLVMAppendBasicBlockInContext(ctx.context, func, "afterwhile")

      LLVMBuildBr(ctx.builder, condBB)
      LLVMPositionBuilderAtEnd(ctx.builder, condBB)
      val condVal = codegenExpr(ctx, cond)
      LLVMBuildCondBr(ctx.builder, condVal, loopBB, afterBB)

      LLVMPositionBuilderAtEnd(ctx.builder, loopBB)
      body.foreach(codegenStmt(ctx, _))
      LLVMBuildBr(ctx.builder, condBB)

      LLVMPositionBuilderAtEnd(ctx.builder, afterBB)

    case SFor(varName, start, stop, body) =>
      val startVal = codegenExpr(ctx, start)
      val alloca = LLVMBuildAlloca(ctx.builder, LLVMInt32TypeInContext(ctx.context), varName)
      ctx.namedValues(varName) = alloca
      ctx.varTypes(varName) = TInteger
      LLVMBuildStore(ctx.builder, startVal, alloca)

      val func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx.builder))
      val condBB = LLVMAppendBasicBlockInContext(ctx.context, func, "forcond")
      val loopBB = LLVMAppendBasicBlockInContext(ctx.context, func, "forloop")
      val afterBB = LLVMAppendBasicBlockInContext(ctx.context, func, "afterfor")

      LLVMBuildBr(ctx.builder, condBB)
      LLVMPositionBuilderAtEnd(ctx.builder, condBB)
      val curVal = LLVMBuildLoad2(ctx.builder, LLVMInt32TypeInContext(ctx.context), alloca, varName)
      val stopVal = codegenExpr(ctx, stop)
      val condVal = LLVMBuildICmp(ctx.builder, LLVMIntSLE, curVal, stopVal, "forcond")
      LLVMBuildCondBr(ctx.builder, condVal, loopBB, afterBB)

      LLVMPositionBuilderAtEnd(ctx.builder, loopBB)
      body.foreach(codegenStmt(ctx, _))
      val nextVal = LLVMBuildAdd(ctx.builder, curVal,
        LLVMConstInt(LLVMInt32TypeInContext(ctx.context), 1, 0), "nextvar")
      LLVMBuildStore(ctx.builder, nextVal, alloca)
      LLVMBuildBr(ctx.builder, condBB)

      LLVMPositionBuilderAtEnd(ctx.builder, afterBB)

    case SWriteln(exprs) =>
      val printfFunc = LLVMGetNamedFunction(ctx.module, "printf")
      exprs.foreach { expr =>
        expr match
          case EString(s) =>
            val str = buildStringLiteral(ctx, s + "\n")
            val args = new PointerPointer[LLVMValueRef](1).put(0, str)
            LLVMBuildCall2(ctx.builder, ctx.funcTypes("printf"), printfFunc, args, 1, "")
          case _ =>
            val value = codegenExpr(ctx, expr)
            val fmt = buildStringLiteral(ctx, "%d\n")
            val args = new PointerPointer[LLVMValueRef](2).put(0, fmt).put(1, value)
            LLVMBuildCall2(ctx.builder, ctx.funcTypes("printf"), printfFunc, args, 2, "")
      }

    case SWrite(exprs) =>
      val printfFunc = LLVMGetNamedFunction(ctx.module, "printf")
      exprs.foreach { expr =>
        expr match
          case EString(s) =>
            val str = buildStringLiteral(ctx, s)
            val args = new PointerPointer[LLVMValueRef](1).put(0, str)
            LLVMBuildCall2(ctx.builder, ctx.funcTypes("printf"), printfFunc, args, 1, "")
          case _ =>
            val value = codegenExpr(ctx, expr)
            val fmt = buildStringLiteral(ctx, "%d")
            val args = new PointerPointer[LLVMValueRef](2).put(0, fmt).put(1, value)
            LLVMBuildCall2(ctx.builder, ctx.funcTypes("printf"), printfFunc, args, 2, "")
      }

    case SReadln(vars) =>
      throw CodegenError("Readln not yet implemented")

    case SReturn(exprOpt) =>
      exprOpt match
        case Some(expr) =>
          val retVal = codegenExpr(ctx, expr)
          LLVMBuildRet(ctx.builder, retVal)
        case None =>
          LLVMBuildRetVoid(ctx.builder)

    case SBlock(stmts) =>
      stmts.foreach(codegenStmt(ctx, _))

  def codegenFunction(ctx: CodegenContext, func: FuncDecl): LLVMValueRef =
    val paramTypes = new PointerPointer[LLVMTypeRef](func.params.size.toLong)
    func.params.zipWithIndex.foreach { case (param, idx) =>
      val paramType = llTypeOfType(ctx, param.paramType)
      paramTypes.put(idx.toLong, paramType)
    }

    val retType = func.returnType match
      case Some(t) => llTypeOfType(ctx, t)
      case None => LLVMVoidTypeInContext(ctx.context)

    val funcType = LLVMFunctionType(retType, paramTypes, func.params.size, 0)
    ctx.funcTypes(func.funcName) = funcType

    // Get existing function or create new one
    val function = LLVMGetNamedFunction(ctx.module, func.funcName) match
      case null => LLVMAddFunction(ctx.module, func.funcName, funcType)
      case existingFunc => existingFunc

    val entryBB = LLVMAppendBasicBlockInContext(ctx.context, function, "entry")
    LLVMPositionBuilderAtEnd(ctx.builder, entryBB)

    // Save previous named values
    val prevValues = ctx.namedValues.clone()
    val prevVarTypes = ctx.varTypes.clone()
    ctx.namedValues.clear()
    ctx.varTypes.clear()

    // Add parameters
    func.params.zipWithIndex.foreach { case (param, idx) =>
      val argVal = LLVMGetParam(function, idx)
      LLVMSetValueName2(argVal, param.paramName, param.paramName.length.toLong)
      val paramType = llTypeOfType(ctx, param.paramType)
      val alloca = LLVMBuildAlloca(ctx.builder, paramType, param.paramName)
      LLVMBuildStore(ctx.builder, argVal, alloca)
      ctx.namedValues(param.paramName) = alloca
      ctx.varTypes(param.paramName) = param.paramType
    }

    // Add local variables
    func.localVars.foreach { v =>
      val varType = llTypeOfType(ctx, v.varType)
      val alloca = LLVMBuildAlloca(ctx.builder, varType, v.varName)
      ctx.namedValues(v.varName) = alloca
      ctx.varTypes(v.varName) = v.varType
      v.varInit.foreach { init =>
        val initVal = codegenExpr(ctx, init)
        LLVMBuildStore(ctx.builder, initVal, alloca)
      }
    }

    // Generate body
    func.body.foreach(codegenStmt(ctx, _))

    // Add default return if needed
    if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(ctx.builder)) == null then
      func.returnType match
        case Some(_) =>
          // Should have been caught by type checker
          throw CodegenError(s"Function ${func.funcName} missing return")
        case None =>
          LLVMBuildRetVoid(ctx.builder)

    // Restore previous scope
    ctx.namedValues.clear()
    ctx.namedValues ++= prevValues
    ctx.varTypes.clear()
    ctx.varTypes ++= prevVarTypes

    function

  def codegenProgram(prog: Program): String =
    val ctx = createContext(prog.programName)
    declareRuntimeFunctions(ctx)

    // First pass: collect type definitions
    prog.declarations.foreach {
      case DType(TypeDecl(typeName, typeDef)) =>
        ctx.typeDefs(typeName) = typeDef
      case _ => ()
    }

    // Second pass: declare all functions
    prog.declarations.foreach {
      case DFunc(func) =>
        // Just collect function type, don't generate yet
        val paramTypes = func.params.map(p => llTypeOfType(ctx, p.paramType))
        val retType = func.returnType match
          case Some(t) => llTypeOfType(ctx, t)
          case None => LLVMVoidTypeInContext(ctx.context)
        val paramTypesPtr = new PointerPointer[LLVMTypeRef](paramTypes.size.toLong)
        paramTypes.zipWithIndex.foreach { case (t, idx) =>
          paramTypesPtr.put(idx.toLong, t)
        }
        val funcType = LLVMFunctionType(retType, paramTypesPtr, paramTypes.size, 0)
        ctx.funcTypes(func.funcName) = funcType
        LLVMAddFunction(ctx.module, func.funcName, funcType)
      case _ => ()
    }

    // Third pass: generate global variables and functions
    prog.declarations.foreach {
      case DVar(VarDecl(varName, varType, varInit)) =>
        val llType = llTypeOfType(ctx, varType)
        val global = LLVMAddGlobal(ctx.module, llType, varName)
        val initVal = varInit match
          case Some(EInteger(i)) => LLVMConstInt(llType, i.toLong, 0)
          case Some(EReal(f)) => LLVMConstReal(llType, f)
          case _ => LLVMConstNull(llType)
        LLVMSetInitializer(global, initVal)
        ctx.namedValues(varName) = global
        ctx.varTypes(varName) = varType

      case DFunc(func) =>
        codegenFunction(ctx, func)

      case _ => ()
    }

    // Verify module
    val error = new BytePointer()
    if LLVMVerifyModule(ctx.module, LLVMPrintMessageAction, error) != 0 then
      val errorMsg = error.getString
      LLVMDisposeMessage(error)
      throw CodegenError(s"Module verification failed: $errorMsg")

    // Return LLVM IR as string
    val irStr = LLVMPrintModuleToString(ctx.module).getString

    // Cleanup
    LLVMDisposeBuilder(ctx.builder)
    LLVMDisposeModule(ctx.module)
    LLVMContextDispose(ctx.context)

    irStr

  def compileToObjectFile(prog: Program, outputPath: String): Unit =
    val ctx = createContext(prog.programName)
    declareRuntimeFunctions(ctx)

    // ... (same as codegenProgram until verification)
    // First pass: collect type definitions
    prog.declarations.foreach {
      case DType(TypeDecl(typeName, typeDef)) =>
        ctx.typeDefs(typeName) = typeDef
      case _ => ()
    }

    // Second pass: declare all functions
    prog.declarations.foreach {
      case DFunc(func) =>
        val paramTypes = func.params.map(p => llTypeOfType(ctx, p.paramType))
        val retType = func.returnType match
          case Some(t) => llTypeOfType(ctx, t)
          case None => LLVMVoidTypeInContext(ctx.context)
        val paramTypesPtr = new PointerPointer[LLVMTypeRef](paramTypes.size.toLong)
        paramTypes.zipWithIndex.foreach { case (t, idx) =>
          paramTypesPtr.put(idx.toLong, t)
        }
        val funcType = LLVMFunctionType(retType, paramTypesPtr, paramTypes.size, 0)
        ctx.funcTypes(func.funcName) = funcType
        LLVMAddFunction(ctx.module, func.funcName, funcType)
      case _ => ()
    }

    // Third pass: generate global variables and functions
    prog.declarations.foreach {
      case DVar(VarDecl(varName, varType, varInit)) =>
        val llType = llTypeOfType(ctx, varType)
        val global = LLVMAddGlobal(ctx.module, llType, varName)
        val initVal = varInit match
          case Some(EInteger(i)) => LLVMConstInt(llType, i.toLong, 0)
          case Some(EReal(f)) => LLVMConstReal(llType, f)
          case _ => LLVMConstNull(llType)
        LLVMSetInitializer(global, initVal)
        ctx.namedValues(varName) = global
        ctx.varTypes(varName) = varType

      case DFunc(func) =>
        codegenFunction(ctx, func)

      case _ => ()
    }

    // Verify module
    val error = new BytePointer()
    if LLVMVerifyModule(ctx.module, LLVMPrintMessageAction, error) != 0 then
      val errorMsg = error.getString
      LLVMDisposeMessage(error)
      throw CodegenError(s"Module verification failed: $errorMsg")

    // Write to object file
    val targetTriple = LLVMGetDefaultTargetTriple()
    LLVMSetTarget(ctx.module, targetTriple)

    LLVMInitializeNativeTarget()
    LLVMInitializeNativeAsmPrinter()

    val target = new LLVMTargetRef()
    if LLVMGetTargetFromTriple(targetTriple, target, error) != 0 then
      val errorMsg = error.getString
      LLVMDisposeMessage(error)
      throw CodegenError(s"Failed to get target: $errorMsg")

    val cpu = new BytePointer("generic")
    val features = new BytePointer("")
    val targetMachine = LLVMCreateTargetMachine(
      target, targetTriple, cpu, features,
      LLVMCodeGenLevelDefault,
      LLVMRelocDefault,
      LLVMCodeModelDefault
    )

    if LLVMTargetMachineEmitToFile(targetMachine, ctx.module, new BytePointer(outputPath),
        LLVMObjectFile, error) != 0 then
      val errorMsg = error.getString
      LLVMDisposeMessage(error)
      throw CodegenError(s"Failed to emit object file: $errorMsg")

    // Cleanup
    LLVMDisposeTargetMachine(targetMachine)
    LLVMDisposeBuilder(ctx.builder)
    LLVMDisposeModule(ctx.module)
    LLVMContextDispose(ctx.context)
