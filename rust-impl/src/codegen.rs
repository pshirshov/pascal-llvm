use crate::ast::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Undefined type: {0}")]
    UndefinedType(String),
    #[error("Unknown variable: {0}")]
    UnknownVariable(String),
    #[error("Unknown function: {0}")]
    UnknownFunction(String),
    #[error("Function type not found: {0}")]
    FunctionTypeNotFound(String),
    #[error("Function {0}: incorrect number of arguments")]
    IncorrectArgCount(String),
    #[error("Record has no field: {0}")]
    NoSuchField(String),
    #[error("Record access on non-record type")]
    RecordAccessOnNonRecord,
    #[error("Complex record expressions not yet supported")]
    ComplexRecordExpr,
    #[error("Can only take address of variables")]
    AddressOfNonVariable,
    #[error("Invalid lvalue")]
    InvalidLValue,
    #[error("malloc not declared")]
    MallocNotDeclared,
    #[error("Unknown loop variable: {0}")]
    UnknownLoopVariable(String),
}

pub struct CodegenContext<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    named_values: HashMap<String, PointerValue<'ctx>>,
    type_cache: HashMap<TypeExpr, BasicTypeEnum<'ctx>>,
    func_types: HashMap<String, inkwell::types::FunctionType<'ctx>>,
    type_defs: HashMap<String, TypeExpr>,
    var_types: HashMap<String, TypeExpr>,
    printf_func: Option<FunctionValue<'ctx>>,
    scanf_func: Option<FunctionValue<'ctx>>,
    malloc_func: Option<FunctionValue<'ctx>>,
    string_counter: usize,
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut ctx = CodegenContext {
            context,
            module,
            builder,
            named_values: HashMap::new(),
            type_cache: HashMap::new(),
            func_types: HashMap::new(),
            type_defs: HashMap::new(),
            var_types: HashMap::new(),
            printf_func: None,
            scanf_func: None,
            malloc_func: None,
            string_counter: 0,
        };

        ctx.declare_runtime_functions();
        ctx
    }

    fn declare_runtime_functions(&mut self) {
        // printf: i32 (ptr, ...)
        let printf_ty = self.context.i32_type().fn_type(
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true, // var args
        );
        self.func_types.insert("printf".to_string(), printf_ty);
        self.printf_func = Some(self.module.add_function("printf", printf_ty, None));

        // scanf: i32 (ptr, ...)
        let scanf_ty = self.context.i32_type().fn_type(
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true, // var args
        );
        self.func_types.insert("scanf".to_string(), scanf_ty);
        self.scanf_func = Some(self.module.add_function("scanf", scanf_ty, None));

        // malloc: ptr (i64)
        let malloc_ty = self
            .context
            .ptr_type(AddressSpace::default())
            .fn_type(&[self.context.i64_type().into()], false);
        self.func_types.insert("malloc".to_string(), malloc_ty);
        self.malloc_func = Some(self.module.add_function("malloc", malloc_ty, None));
    }

    fn resolve_type(&self, ty: &TypeExpr) -> Result<TypeExpr, CodegenError> {
        match ty {
            TypeExpr::TNamed(name) => match self.type_defs.get(name) {
                Some(resolved) => self.resolve_type(resolved),
                None => Err(CodegenError::UndefinedType(name.clone())),
            },
            _ => Ok(ty.clone()),
        }
    }

    fn lltype_of_type(&mut self, ty: &TypeExpr) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        if let Some(llty) = self.type_cache.get(ty) {
            return Ok(*llty);
        }

        let llty = match ty {
            TypeExpr::TInteger => self.context.i32_type().as_basic_type_enum(),
            TypeExpr::TReal => self.context.f64_type().as_basic_type_enum(),
            TypeExpr::TBoolean => self.context.bool_type().as_basic_type_enum(),
            TypeExpr::TChar => self.context.i8_type().as_basic_type_enum(),
            TypeExpr::TString => self
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            TypeExpr::TPointer(_) => self
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            TypeExpr::TArray(elem_type, size) => {
                let elem_lltype = self.lltype_of_type(elem_type)?;
                elem_lltype.array_type(*size as u32).as_basic_type_enum()
            }
            TypeExpr::TRecord(fields) => {
                let field_types: Result<Vec<BasicTypeEnum<'ctx>>, CodegenError> = fields
                    .iter()
                    .map(|f| self.lltype_of_type(&f.field_type))
                    .collect();
                let field_types = field_types?;
                self.context
                    .struct_type(&field_types, false)
                    .as_basic_type_enum()
            }
            TypeExpr::TNamed(name) => {
                let resolved_type = self
                    .type_defs
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedType(name.clone()))?
                    .clone();
                return self.lltype_of_type(&resolved_type);
            }
        };

        self.type_cache.insert(ty.clone(), llty);
        Ok(llty)
    }

    fn get_field_index(fields: &[RecordField], field_name: &str) -> Option<usize> {
        fields
            .iter()
            .position(|f| f.field_name == field_name)
    }

    fn build_string_literal(&mut self, s: &str) -> PointerValue<'ctx> {
        let str_val = self.builder.build_global_string_ptr(s, &format!("str_{}", self.string_counter))
            .expect("Failed to build string literal");
        self.string_counter += 1;
        str_val.as_pointer_value()
    }

    fn codegen_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expr::EInteger(i) => Ok(self
                .context
                .i32_type()
                .const_int(*i as u64, true)
                .as_basic_value_enum()),
            Expr::EReal(f) => Ok(self
                .context
                .f64_type()
                .const_float(*f)
                .as_basic_value_enum()),
            Expr::EBoolean(b) => Ok(self
                .context
                .bool_type()
                .const_int(if *b { 1 } else { 0 }, false)
                .as_basic_value_enum()),
            Expr::EChar(c) => Ok(self
                .context
                .i8_type()
                .const_int(*c as u64, false)
                .as_basic_value_enum()),
            Expr::EString(s) => Ok(self.build_string_literal(s).as_basic_value_enum()),

            Expr::EVar(name) => {
                let ptr = *self
                    .named_values
                    .get(name)
                    .ok_or_else(|| CodegenError::UnknownVariable(name.clone()))?;
                let var_type = self
                    .var_types
                    .get(name)
                    .cloned()
                    .unwrap_or(TypeExpr::TInteger);
                let resolved = self.resolve_type(&var_type)?;
                let lltype = self.lltype_of_type(&resolved)?;
                Ok(self
                    .builder
                    .build_load(lltype, ptr, name)
                    .expect("Failed to build load"))
            }

            Expr::EBinop(op, e1, e2) => {
                let lhs = self.codegen_expr(e1)?;
                let rhs = self.codegen_expr(e2)?;
                match op {
                    BinOp::Add => Ok(self
                        .builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "addtmp")
                        .expect("Failed to build add")
                        .as_basic_value_enum()),
                    BinOp::Sub => Ok(self
                        .builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "subtmp")
                        .expect("Failed to build sub")
                        .as_basic_value_enum()),
                    BinOp::Mul => Ok(self
                        .builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "multmp")
                        .expect("Failed to build mul")
                        .as_basic_value_enum()),
                    BinOp::Div => Ok(self
                        .builder
                        .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "divtmp")
                        .expect("Failed to build div")
                        .as_basic_value_enum()),
                    BinOp::Mod => Ok(self
                        .builder
                        .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "modtmp")
                        .expect("Failed to build mod")
                        .as_basic_value_enum()),
                    BinOp::Eq => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "eqtmp",
                        )
                        .expect("Failed to build eq")
                        .as_basic_value_enum()),
                    BinOp::Ne => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "netmp",
                        )
                        .expect("Failed to build ne")
                        .as_basic_value_enum()),
                    BinOp::Lt => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "lttmp",
                        )
                        .expect("Failed to build lt")
                        .as_basic_value_enum()),
                    BinOp::Le => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "letmp",
                        )
                        .expect("Failed to build le")
                        .as_basic_value_enum()),
                    BinOp::Gt => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::SGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "gttmp",
                        )
                        .expect("Failed to build gt")
                        .as_basic_value_enum()),
                    BinOp::Ge => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::SGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "getmp",
                        )
                        .expect("Failed to build ge")
                        .as_basic_value_enum()),
                    BinOp::And => Ok(self
                        .builder
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "andtmp")
                        .expect("Failed to build and")
                        .as_basic_value_enum()),
                    BinOp::Or => Ok(self
                        .builder
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "ortmp")
                        .expect("Failed to build or")
                        .as_basic_value_enum()),
                }
            }

            Expr::EUnop(op, e) => {
                let val = self.codegen_expr(e)?;
                match op {
                    UnOp::Neg => Ok(self
                        .builder
                        .build_int_neg(val.into_int_value(), "negtmp")
                        .expect("Failed to build neg")
                        .as_basic_value_enum()),
                    UnOp::Not => Ok(self
                        .builder
                        .build_not(val.into_int_value(), "nottmp")
                        .expect("Failed to build not")
                        .as_basic_value_enum()),
                }
            }

            Expr::ECall(name, args) => {
                let callee = self
                    .module
                    .get_function(name)
                    .ok_or_else(|| CodegenError::UnknownFunction(name.clone()))?;
                let fn_type = self
                    .func_types
                    .get(name)
                    .ok_or_else(|| CodegenError::FunctionTypeNotFound(name.clone()))?;

                if !fn_type.is_var_arg() && callee.count_params() as usize != args.len() {
                    return Err(CodegenError::IncorrectArgCount(name.clone()));
                }

                let args_vals: Result<Vec<BasicMetadataValueEnum>, CodegenError> = args
                    .iter()
                    .map(|arg| Ok(self.codegen_expr(arg)?.into()))
                    .collect();
                let args_vals = args_vals?;

                Ok(self
                    .builder
                    .build_call(callee, &args_vals, "calltmp")
                    .expect("Failed to build call")
                    .try_as_basic_value()
                    .left()
                    .expect("Call should return a value"))
            }

            Expr::EArrayAccess(arr, idx) => {
                let arr_val = self.codegen_expr(arr)?;
                let idx_val = self.codegen_expr(idx)?;

                let ptr = unsafe {
                    self.builder
                        .build_gep(
                            arr_val.get_type(),
                            arr_val.into_pointer_value(),
                            &[idx_val.into_int_value()],
                            "arraytmp",
                        )
                        .expect("Failed to build GEP")
                };

                let elem_type = match arr_val.get_type() {
                    BasicTypeEnum::ArrayType(at) => at.get_element_type(),
                    _ => self.context.i32_type().as_basic_type_enum(),
                };

                Ok(self
                    .builder
                    .build_load(elem_type, ptr, "arrayload")
                    .expect("Failed to build load"))
            }

            Expr::ERecordAccess(_, _) => {
                let field_ptr = self.codegen_lvalue(expr)?;
                let field_type = self.get_expr_type(expr)?;
                let field_lltype = self.lltype_of_type(&field_type)?;
                Ok(self
                    .builder
                    .build_load(field_lltype, field_ptr, "fieldload")
                    .expect("Failed to build load"))
            }

            Expr::EDeref(ptr) => {
                let ptr_val = self.codegen_expr(ptr)?;
                // In opaque pointer mode, we load directly
                let elem_type = self.context.i32_type().as_basic_type_enum(); // Default to i32
                Ok(self
                    .builder
                    .build_load(elem_type, ptr_val.into_pointer_value(), "deref")
                    .expect("Failed to build load"))
            }

            Expr::EAddress(e) => {
                if let Expr::EVar(name) = &**e {
                    let ptr = self
                        .named_values
                        .get(name)
                        .ok_or_else(|| CodegenError::UnknownVariable(name.clone()))?;
                    Ok((*ptr).as_basic_value_enum())
                } else {
                    Err(CodegenError::AddressOfNonVariable)
                }
            }

            Expr::ENew(ty) => {
                let llty = self.lltype_of_type(ty)?;
                let size = llty.size_of().expect("Type should have size");
                let malloc_func = self.malloc_func.ok_or(CodegenError::MallocNotDeclared)?;

                let malloc_call = self
                    .builder
                    .build_call(
                        malloc_func,
                        &[size.into()],
                        "malloctmp",
                    )
                    .expect("Failed to build call");

                Ok(malloc_call
                    .try_as_basic_value()
                    .left()
                    .expect("malloc should return a value"))
            }
        }
    }

    // Helper to get the type of an expression
    fn get_expr_type(&self, expr: &Expr) -> Result<TypeExpr, CodegenError> {
        match expr {
            Expr::EVar(name) => {
                let ty = self.var_types.get(name)
                    .ok_or_else(|| CodegenError::UnknownVariable(name.clone()))?;
                self.resolve_type(ty)
            }
            Expr::ERecordAccess(rec_expr, field) => {
                let rec_type = self.resolve_type(&self.get_expr_type(rec_expr)?)?;
                if let TypeExpr::TRecord(fields) = rec_type {
                    let field_def = fields.iter()
                        .find(|f| &f.field_name == field)
                        .ok_or_else(|| CodegenError::NoSuchField(field.clone()))?;
                    self.resolve_type(&field_def.field_type)
                } else {
                    Err(CodegenError::RecordAccessOnNonRecord)
                }
            }
            Expr::EArrayAccess(arr_expr, _) => {
                let arr_type = self.resolve_type(&self.get_expr_type(arr_expr)?)?;
                if let TypeExpr::TArray(elem_type, _) = arr_type {
                    self.resolve_type(&elem_type)
                } else {
                    Err(CodegenError::InvalidLValue)
                }
            }
            _ => Err(CodegenError::InvalidLValue),
        }
    }

    fn codegen_lvalue(&mut self, expr: &Expr) -> Result<PointerValue<'ctx>, CodegenError> {
        match expr {
            Expr::EVar(name) => {
                let ptr = self
                    .named_values
                    .get(name)
                    .ok_or_else(|| CodegenError::UnknownVariable(name.clone()))?;
                Ok(*ptr)
            }
            Expr::EArrayAccess(arr, idx) => {
                let arr_ptr = self.codegen_lvalue(arr)?;
                let idx_val = self.codegen_expr(idx)?;
                let arr_type = self.resolve_type(&self.get_expr_type(arr)?)?;
                let arr_lltype = self.lltype_of_type(&arr_type)?;
                let zero = self.context.i32_type().const_int(0, false);
                Ok(unsafe {
                    self.builder
                        .build_gep(
                            arr_lltype,
                            arr_ptr,
                            &[zero, idx_val.into_int_value()],
                            "arrayptr",
                        )
                        .expect("Failed to build GEP")
                })
            }
            Expr::ERecordAccess(rec_expr, field) => {
                let rec_ptr = self.codegen_lvalue(rec_expr)?;
                let rec_type = self.resolve_type(&self.get_expr_type(rec_expr)?)?;

                if let TypeExpr::TRecord(fields) = rec_type.clone() {
                    let field_idx = Self::get_field_index(&fields, field)
                        .ok_or_else(|| CodegenError::NoSuchField(field.clone()))?;

                    let zero = self.context.i32_type().const_int(0, false);
                    let field_idx_val = self.context.i32_type().const_int(field_idx as u64, false);
                    let rec_lltype = self.lltype_of_type(&rec_type)?;

                    Ok(unsafe {
                        self.builder
                            .build_gep(
                                rec_lltype,
                                rec_ptr,
                                &[zero, field_idx_val],
                                "fieldptr",
                            )
                            .expect("Failed to build GEP")
                    })
                } else {
                    Err(CodegenError::RecordAccessOnNonRecord)
                }
            }
            Expr::EDeref(ptr) => {
                let ptr_val = self.codegen_expr(ptr)?;
                Ok(ptr_val.into_pointer_value())
            }
            _ => Err(CodegenError::InvalidLValue),
        }
    }

    fn codegen_stmt(&mut self, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::SVarDecl(name, var_type, init) | Stmt::SValDecl(name, var_type, init) => {
                let resolved = self.resolve_type(var_type)?;
                let lltype = self.lltype_of_type(&resolved)?;
                let alloca = self
                    .builder
                    .build_alloca(lltype, name)
                    .expect("Failed to build alloca");
                self.named_values.insert(name.clone(), alloca);
                self.var_types.insert(name.clone(), resolved);
                let init_val = self.codegen_expr(init)?;
                self.builder
                    .build_store(alloca, init_val)
                    .expect("Failed to build store");
                Ok(())
            }

            Stmt::SAssign(lval, rval) => {
                let lval_ptr = self.codegen_lvalue(lval)?;
                let rval_val = self.codegen_expr(rval)?;
                self.builder
                    .build_store(lval_ptr, rval_val)
                    .expect("Failed to build store");
                Ok(())
            }

            Stmt::SCall(name, args) => {
                self.codegen_expr(&Expr::ECall(name.clone(), args.clone()))?;
                Ok(())
            }

            Stmt::SIf(cond, then_stmts, else_stmts) => {
                let cond_val = self.codegen_expr(cond)?;
                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .expect("No insert block")
                    .get_parent()
                    .expect("Block has no parent");

                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "ifcont");

                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb)
                    .expect("Failed to build conditional branch");

                // Then block
                self.builder.position_at_end(then_bb);
                for stmt in then_stmts {
                    self.codegen_stmt(stmt)?;
                }
                let then_bb_end = self.builder.get_insert_block().expect("No insert block");
                let then_terminated = then_bb_end.get_terminator().is_some();
                if !then_terminated {
                    self.builder
                        .build_unconditional_branch(merge_bb)
                        .expect("Failed to build branch");
                }

                // Else block
                self.builder.position_at_end(else_bb);
                if let Some(stmts) = else_stmts {
                    for stmt in stmts {
                        self.codegen_stmt(stmt)?;
                    }
                }
                let else_bb_end = self.builder.get_insert_block().expect("No insert block");
                let else_terminated = else_bb_end.get_terminator().is_some();
                if !else_terminated {
                    self.builder
                        .build_unconditional_branch(merge_bb)
                        .expect("Failed to build branch");
                }

                // Merge block
                self.builder.position_at_end(merge_bb);
                // If both branches are terminated, this block is unreachable
                if then_terminated && else_terminated {
                    self.builder.build_unreachable().expect("Failed to build unreachable");
                }
                Ok(())
            }

            Stmt::SWhile(cond, body) => {
                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .expect("No insert block")
                    .get_parent()
                    .expect("Block has no parent");

                let loop_bb = self.context.append_basic_block(parent_fn, "loop");
                let body_bb = self.context.append_basic_block(parent_fn, "body");
                let after_bb = self.context.append_basic_block(parent_fn, "afterloop");

                self.builder
                    .build_unconditional_branch(loop_bb)
                    .expect("Failed to build branch");

                // Loop condition
                self.builder.position_at_end(loop_bb);
                let cond_val = self.codegen_expr(cond)?;
                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), body_bb, after_bb)
                    .expect("Failed to build conditional branch");

                // Loop body
                self.builder.position_at_end(body_bb);
                for stmt in body {
                    self.codegen_stmt(stmt)?;
                }
                self.builder
                    .build_unconditional_branch(loop_bb)
                    .expect("Failed to build branch");

                // After loop
                self.builder.position_at_end(after_bb);
                Ok(())
            }

            Stmt::SFor(var, start, stop, body) => {
                let start_val = self.codegen_expr(start)?;
                // Auto-allocate loop variable if it doesn't exist
                let var_ptr = if let Some(ptr) = self.named_values.get(var) {
                    *ptr
                } else {
                    let alloca = self
                        .builder
                        .build_alloca(self.context.i32_type(), var)
                        .expect("Failed to build alloca");
                    self.named_values.insert(var.clone(), alloca);
                    self.var_types.insert(var.clone(), TypeExpr::TInteger);
                    alloca
                };
                self.builder
                    .build_store(var_ptr, start_val)
                    .expect("Failed to build store");

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .expect("No insert block")
                    .get_parent()
                    .expect("Block has no parent");

                let loop_bb = self.context.append_basic_block(parent_fn, "forloop");
                let body_bb = self.context.append_basic_block(parent_fn, "forbody");
                let after_bb = self.context.append_basic_block(parent_fn, "afterfor");

                self.builder
                    .build_unconditional_branch(loop_bb)
                    .expect("Failed to build branch");

                // Loop condition
                self.builder.position_at_end(loop_bb);
                let var_val = self
                    .builder
                    .build_load(self.context.i32_type(), var_ptr, var)
                    .expect("Failed to build load");
                let stop_val = self.codegen_expr(stop)?;
                let cond_val = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLE,
                        var_val.into_int_value(),
                        stop_val.into_int_value(),
                        "forcond",
                    )
                    .expect("Failed to build compare");

                self.builder
                    .build_conditional_branch(cond_val, body_bb, after_bb)
                    .expect("Failed to build conditional branch");

                // Loop body
                self.builder.position_at_end(body_bb);
                for stmt in body {
                    self.codegen_stmt(stmt)?;
                }

                let next_var = self
                    .builder
                    .build_int_add(
                        var_val.into_int_value(),
                        self.context.i32_type().const_int(1, false),
                        "nextvar",
                    )
                    .expect("Failed to build add");
                self.builder
                    .build_store(var_ptr, next_var)
                    .expect("Failed to build store");
                self.builder
                    .build_unconditional_branch(loop_bb)
                    .expect("Failed to build branch");

                // After loop
                self.builder.position_at_end(after_bb);
                Ok(())
            }

            Stmt::SWriteln(args) | Stmt::SWrite(args) => {
                let printf_func = self.printf_func.ok_or(CodegenError::UnknownFunction("printf".to_string()))?;

                for arg in args {
                    let arg_val = self.codegen_expr(arg)?;
                    let fmt = match arg_val.get_type() {
                        BasicTypeEnum::IntType(int_ty) => {
                            if int_ty.get_bit_width() == 1 {
                                self.build_string_literal("%d")
                            } else {
                                self.build_string_literal("%d")
                            }
                        }
                        BasicTypeEnum::FloatType(_) => self.build_string_literal("%f"),
                        BasicTypeEnum::PointerType(_) => self.build_string_literal("%s"),
                        _ => self.build_string_literal("%d"),
                    };

                    self.builder
                        .build_call(
                            printf_func,
                            &[fmt.into(), arg_val.into()],
                            "printfcall",
                        )
                        .expect("Failed to build call");
                }

                if matches!(stmt, Stmt::SWriteln(_)) {
                    let newline = self.build_string_literal("\n");
                    self.builder
                        .build_call(printf_func, &[newline.into()], "printfcall")
                        .expect("Failed to build call");
                }
                Ok(())
            }

            Stmt::SReadln(vars) => {
                let scanf_func = self.scanf_func.ok_or(CodegenError::UnknownFunction("scanf".to_string()))?;

                for var in vars {
                    let var_ptr = *self
                        .named_values
                        .get(var)
                        .ok_or_else(|| CodegenError::UnknownVariable(var.clone()))?;
                    let fmt = self.build_string_literal("%d");

                    self.builder
                        .build_call(
                            scanf_func,
                            &[fmt.into(), var_ptr.into()],
                            "scanfcall",
                        )
                        .expect("Failed to build call");
                }
                Ok(())
            }

            Stmt::SReturn(expr_opt) => {
                match expr_opt {
                    Some(e) => {
                        let ret_val = self.codegen_expr(e)?;
                        self.builder
                            .build_return(Some(&ret_val))
                            .expect("Failed to build return");
                    }
                    None => {
                        self.builder
                            .build_return(None)
                            .expect("Failed to build return");
                    }
                }
                Ok(())
            }

            Stmt::SBlock(stmts) => {
                for stmt in stmts {
                    self.codegen_stmt(stmt)?;
                }
                Ok(())
            }
        }
    }

    fn codegen_function(&mut self, func: &FuncDecl) -> Result<FunctionValue<'ctx>, CodegenError> {
        let param_types: Result<Vec<BasicMetadataTypeEnum<'ctx>>, CodegenError> = func
            .params
            .iter()
            .map(|p| Ok(self.lltype_of_type(&p.param_type)?.into()))
            .collect();
        let param_types = param_types?;

        let ret_type = match &func.return_type {
            Some(t) => self.lltype_of_type(t)?,
            None => self.context.i32_type().as_basic_type_enum(), // Use i32 as void placeholder
        };

        let func_type = if func.return_type.is_some() {
            ret_type.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        self.func_types.insert(func.func_name.clone(), func_type);
        let the_function = self.module.add_function(&func.func_name, func_type, None);

        // Create entry block
        let bb = self.context.append_basic_block(the_function, "entry");
        self.builder.position_at_end(bb);

        // Save old named values and types
        let old_values = self.named_values.clone();
        let old_types = self.var_types.clone();
        self.named_values.clear();
        self.var_types.clear();

        // Allocate parameters
        for (i, param) in the_function.get_param_iter().enumerate() {
            let param_info = &func.params[i];
            let param_name = &param_info.param_name;
            let param_type = self.lltype_of_type(&param_info.param_type)?;
            let alloca = self
                .builder
                .build_alloca(param_type, param_name)
                .expect("Failed to build alloca");
            self.builder
                .build_store(alloca, param)
                .expect("Failed to build store");
            self.named_values.insert(param_name.clone(), alloca);
            self.var_types
                .insert(param_name.clone(), param_info.param_type.clone());
        }

        // Allocate local variables
        for var in &func.local_vars {
            let var_type = self.lltype_of_type(&var.var_type)?;
            let alloca = self
                .builder
                .build_alloca(var_type, &var.var_name)
                .expect("Failed to build alloca");
            self.named_values.insert(var.var_name.clone(), alloca);
            self.var_types.insert(var.var_name.clone(), var.var_type.clone());
            if let Some(init) = &var.var_init {
                let init_val = self.codegen_expr(init)?;
                self.builder
                    .build_store(alloca, init_val)
                    .expect("Failed to build store");
            }
        }

        // Generate function body
        for stmt in &func.body {
            self.codegen_stmt(stmt)?;
        }

        // Add default return if needed
        if self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_none()
        {
            if func.return_type.is_none() {
                self.builder
                    .build_return(None)
                    .expect("Failed to build return");
            }
        }

        // Restore named values and types
        self.named_values = old_values;
        self.var_types = old_types;

        Ok(the_function)
    }

    pub fn codegen_program(&mut self, prog: &Program) -> Result<(), CodegenError> {
        // Process type declarations
        for decl in &prog.declarations {
            if let Declaration::DType(type_decl) = decl {
                self.type_defs
                    .insert(type_decl.type_name.clone(), type_decl.type_def.clone());
            }
        }

        // Process global variables
        for decl in &prog.declarations {
            if let Declaration::DVar(var) = decl {
                let var_type = self.lltype_of_type(&var.var_type)?;
                let global = self.module.add_global(var_type, None, &var.var_name);
                global.set_initializer(&var_type.const_zero());
                self.named_values
                    .insert(var.var_name.clone(), global.as_pointer_value());
                self.var_types.insert(var.var_name.clone(), var.var_type.clone());
            }
        }

        // Process functions
        for decl in &prog.declarations {
            if let Declaration::DFunc(func) = decl {
                self.codegen_function(func)?;
            }
        }

        Ok(())
    }

    pub fn get_module(self) -> Module<'ctx> {
        self.module
    }
}

pub fn generate_code(prog: &Program) -> Result<String, CodegenError> {
    let context = Context::create();
    let mut codegen = CodegenContext::new(&context, &prog.program_name);
    codegen.codegen_program(prog)?;
    let result = codegen.get_module().to_string();
    Ok(result)
}
