/// Type checker for Pascal-like language
use crate::ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
#[error("Type error: {0}")]
pub struct TypeError(pub String);

pub struct SymbolTable {
    types: HashMap<Identifier, TypeExpr>,
    vars: HashMap<Identifier, TypeExpr>,
    funcs: HashMap<Identifier, (Vec<Param>, Option<TypeExpr>)>,
    vals: HashMap<Identifier, ()>, // Track immutable variables
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            types: HashMap::new(),
            vars: HashMap::new(),
            funcs: HashMap::new(),
            vals: HashMap::new(),
        }
    }

    fn copy_for_function(&self) -> Self {
        SymbolTable {
            types: self.types.clone(),
            vars: self.vars.clone(),
            funcs: self.funcs.clone(),
            vals: self.vals.clone(),
        }
    }
}

fn types_equal(t1: &TypeExpr, t2: &TypeExpr) -> bool {
    match (t1, t2) {
        (TypeExpr::TInteger, TypeExpr::TInteger)
        | (TypeExpr::TReal, TypeExpr::TReal)
        | (TypeExpr::TBoolean, TypeExpr::TBoolean)
        | (TypeExpr::TChar, TypeExpr::TChar)
        | (TypeExpr::TString, TypeExpr::TString) => true,
        (TypeExpr::TPointer(t1), TypeExpr::TPointer(t2)) => types_equal(t1, t2),
        (TypeExpr::TArray(t1, s1), TypeExpr::TArray(t2, s2)) => s1 == s2 && types_equal(t1, t2),
        (TypeExpr::TRecord(fields1), TypeExpr::TRecord(fields2)) => {
            fields1.len() == fields2.len()
                && fields1.iter().zip(fields2.iter()).all(|(f1, f2)| {
                    f1.field_name == f2.field_name && types_equal(&f1.field_type, &f2.field_type)
                })
        }
        (TypeExpr::TNamed(n1), TypeExpr::TNamed(n2)) => n1 == n2,
        _ => false,
    }
}

fn resolve_type(symtab: &SymbolTable, type_expr: &TypeExpr) -> Result<TypeExpr, TypeError> {
    match type_expr {
        TypeExpr::TNamed(name) => symtab
            .types
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError(format!("Undefined type: {}", name))),
        other => Ok(other.clone()),
    }
}

fn check_expr(symtab: &SymbolTable, expr: &Expr) -> Result<TypeExpr, TypeError> {
    match expr {
        Expr::EInteger(_) => Ok(TypeExpr::TInteger),
        Expr::EReal(_) => Ok(TypeExpr::TReal),
        Expr::EBoolean(_) => Ok(TypeExpr::TBoolean),
        Expr::EChar(_) => Ok(TypeExpr::TChar),
        Expr::EString(_) => Ok(TypeExpr::TString),

        Expr::EVar(name) => symtab
            .vars
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError(format!("Undefined variable: {}", name))),

        Expr::EBinop(op, e1, e2) => {
            let t1 = check_expr(symtab, e1)?;
            let t2 = check_expr(symtab, e2)?;

            match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    if (types_equal(&t1, &TypeExpr::TInteger)
                        && types_equal(&t2, &TypeExpr::TInteger))
                        || (types_equal(&t1, &TypeExpr::TReal)
                            && types_equal(&t2, &TypeExpr::TReal))
                    {
                        Ok(t1)
                    } else {
                        Err(TypeError(
                            "Arithmetic operators require numeric operands".to_string(),
                        ))
                    }
                }
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                    if types_equal(&t1, &t2) {
                        Ok(TypeExpr::TBoolean)
                    } else {
                        Err(TypeError(
                            "Comparison operators require same types".to_string(),
                        ))
                    }
                }
                BinOp::And | BinOp::Or => {
                    if types_equal(&t1, &TypeExpr::TBoolean)
                        && types_equal(&t2, &TypeExpr::TBoolean)
                    {
                        Ok(TypeExpr::TBoolean)
                    } else {
                        Err(TypeError(
                            "Logical operators require boolean operands".to_string(),
                        ))
                    }
                }
            }
        }

        Expr::EUnop(op, e) => {
            let t = check_expr(symtab, e)?;
            match op {
                UnOp::Neg => {
                    if types_equal(&t, &TypeExpr::TInteger) || types_equal(&t, &TypeExpr::TReal) {
                        Ok(t)
                    } else {
                        Err(TypeError("Negation requires numeric operand".to_string()))
                    }
                }
                UnOp::Not => {
                    if types_equal(&t, &TypeExpr::TBoolean) {
                        Ok(TypeExpr::TBoolean)
                    } else {
                        Err(TypeError("Not requires boolean operand".to_string()))
                    }
                }
            }
        }

        Expr::ECall(name, args) => {
            let (params, ret_type) = symtab
                .funcs
                .get(name)
                .ok_or_else(|| TypeError(format!("Undefined function: {}", name)))?;

            if args.len() != params.len() {
                return Err(TypeError(format!(
                    "Function {}: wrong number of arguments",
                    name
                )));
            }

            for (arg, param) in args.iter().zip(params.iter()) {
                let arg_type = check_expr(symtab, arg)?;
                let param_type = resolve_type(symtab, &param.param_type)?;
                if !types_equal(&arg_type, &param_type) {
                    return Err(TypeError(format!(
                        "Function {}: argument type mismatch",
                        name
                    )));
                }
            }

            ret_type
                .clone()
                .ok_or_else(|| TypeError(format!("{} is a procedure, not a function", name)))
        }

        Expr::EArrayAccess(arr, idx) => {
            let arr_type = check_expr(symtab, arr)?;
            let idx_type = check_expr(symtab, idx)?;

            if !types_equal(&idx_type, &TypeExpr::TInteger) {
                return Err(TypeError("Array index must be integer".to_string()));
            }

            match arr_type {
                TypeExpr::TArray(elem_type, _) => Ok(*elem_type),
                _ => Err(TypeError("Array access on non-array type".to_string())),
            }
        }

        Expr::ERecordAccess(rec_expr, field) => {
            let rec_type = check_expr(symtab, rec_expr)?;
            match rec_type {
                TypeExpr::TRecord(fields) => fields
                    .iter()
                    .find(|f| f.field_name == *field)
                    .map(|f| f.field_type.clone())
                    .ok_or_else(|| TypeError(format!("Record has no field: {}", field))),
                _ => Err(TypeError("Record access on non-record type".to_string())),
            }
        }

        Expr::EDeref(ptr) => {
            let ptr_type = check_expr(symtab, ptr)?;
            match ptr_type {
                TypeExpr::TPointer(t) => Ok(*t),
                _ => Err(TypeError("Dereference on non-pointer type".to_string())),
            }
        }

        Expr::EAddress(e) => {
            let t = check_expr(symtab, e)?;
            Ok(TypeExpr::TPointer(Box::new(t)))
        }

        Expr::ENew(t) => {
            let resolved = resolve_type(symtab, t)?;
            Ok(TypeExpr::TPointer(Box::new(resolved)))
        }
    }
}

fn check_stmt(
    symtab: &mut SymbolTable,
    return_type: &Option<TypeExpr>,
    stmt: &Stmt,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::SVarDecl(name, var_type, init) => {
            let resolved = resolve_type(symtab, var_type)?;
            let init_type = check_expr(symtab, init)?;
            if !types_equal(&resolved, &init_type) {
                return Err(TypeError(format!(
                    "Variable {}: initializer type mismatch",
                    name
                )));
            }
            symtab.vars.insert(name.clone(), resolved);
            Ok(())
        }

        Stmt::SValDecl(name, var_type, init) => {
            let resolved = resolve_type(symtab, var_type)?;
            let init_type = check_expr(symtab, init)?;
            if !types_equal(&resolved, &init_type) {
                return Err(TypeError(format!(
                    "Val {}: initializer type mismatch",
                    name
                )));
            }
            symtab.vars.insert(name.clone(), resolved);
            symtab.vals.insert(name.clone(), ());
            Ok(())
        }

        Stmt::SAssign(lval, rval) => {
            // Check if trying to assign to a val
            if let Expr::EVar(name) = lval {
                if symtab.vals.contains_key(name) {
                    return Err(TypeError(format!("Cannot assign to val: {}", name)));
                }
            }

            let lval_type = resolve_type(symtab, &check_expr(symtab, lval)?)?;
            let rval_type = resolve_type(symtab, &check_expr(symtab, rval)?)?;
            if !types_equal(&lval_type, &rval_type) {
                return Err(TypeError("Assignment type mismatch".to_string()));
            }
            Ok(())
        }

        Stmt::SCall(name, args) => {
            let (params, _) = symtab
                .funcs
                .get(name)
                .ok_or_else(|| TypeError(format!("Undefined procedure: {}", name)))?;

            if args.len() != params.len() {
                return Err(TypeError(format!(
                    "Procedure {}: wrong number of arguments",
                    name
                )));
            }

            for (arg, param) in args.iter().zip(params.iter()) {
                let arg_type = check_expr(symtab, arg)?;
                if !types_equal(&arg_type, &param.param_type) {
                    return Err(TypeError(format!(
                        "Procedure {}: argument type mismatch",
                        name
                    )));
                }
            }
            Ok(())
        }

        Stmt::SIf(cond, then_stmts, else_stmts) => {
            let cond_type = check_expr(symtab, cond)?;
            if !types_equal(&cond_type, &TypeExpr::TBoolean) {
                return Err(TypeError("If condition must be boolean".to_string()));
            }
            for stmt in then_stmts {
                check_stmt(symtab, return_type, stmt)?;
            }
            if let Some(stmts) = else_stmts {
                for stmt in stmts {
                    check_stmt(symtab, return_type, stmt)?;
                }
            }
            Ok(())
        }

        Stmt::SWhile(cond, body) => {
            let cond_type = check_expr(symtab, cond)?;
            if !types_equal(&cond_type, &TypeExpr::TBoolean) {
                return Err(TypeError("While condition must be boolean".to_string()));
            }
            for stmt in body {
                check_stmt(symtab, return_type, stmt)?;
            }
            Ok(())
        }

        Stmt::SFor(_var, start, stop, body) => {
            let start_type = check_expr(symtab, start)?;
            let stop_type = check_expr(symtab, stop)?;
            if !types_equal(&start_type, &TypeExpr::TInteger)
                || !types_equal(&stop_type, &TypeExpr::TInteger)
            {
                return Err(TypeError("For loop bounds must be integers".to_string()));
            }
            for stmt in body {
                check_stmt(symtab, return_type, stmt)?;
            }
            Ok(())
        }

        Stmt::SWriteln(exprs) | Stmt::SWrite(exprs) => {
            for e in exprs {
                check_expr(symtab, e)?;
            }
            Ok(())
        }

        Stmt::SReadln(vars) => {
            for var in vars {
                if !symtab.vars.contains_key(var) {
                    return Err(TypeError(format!("Undefined variable: {}", var)));
                }
            }
            Ok(())
        }

        Stmt::SReturn(expr_opt) => match (expr_opt, return_type) {
            (Some(e), Some(expected_type)) => {
                let actual_type = check_expr(symtab, e)?;
                if !types_equal(&actual_type, expected_type) {
                    return Err(TypeError("Return type mismatch".to_string()));
                }
                Ok(())
            }
            (None, None) => Ok(()),
            (Some(_), None) => Err(TypeError("Procedure should not return a value".to_string())),
            (None, Some(_)) => Err(TypeError("Function must return a value".to_string())),
        },

        Stmt::SBlock(stmts) => {
            for stmt in stmts {
                check_stmt(symtab, return_type, stmt)?;
            }
            Ok(())
        }
    }
}

pub fn check_program(prog: &Program) -> Result<(), TypeError> {
    let mut symtab = SymbolTable::new();

    // First pass: collect type and function declarations
    for decl in &prog.declarations {
        match decl {
            Declaration::DType(type_decl) => {
                symtab
                    .types
                    .insert(type_decl.type_name.clone(), type_decl.type_def.clone());
            }
            Declaration::DFunc(func) => {
                symtab.funcs.insert(
                    func.func_name.clone(),
                    (func.params.clone(), func.return_type.clone()),
                );
            }
            Declaration::DVar(_) => {}
        }
    }

    // Second pass: collect global variables and check functions
    for decl in &prog.declarations {
        match decl {
            Declaration::DVar(var) => {
                let resolved = resolve_type(&symtab, &var.var_type)?;
                symtab.vars.insert(var.var_name.clone(), resolved.clone());
                if let Some(init) = &var.var_init {
                    let init_type = check_expr(&symtab, init)?;
                    if !types_equal(&resolved, &init_type) {
                        return Err(TypeError(format!(
                            "Variable {}: initializer type mismatch",
                            var.var_name
                        )));
                    }
                }
            }

            Declaration::DFunc(func) => {
                // Create function-local symbol table
                let mut local_symtab = symtab.copy_for_function();

                // Add parameters to local scope
                for param in &func.params {
                    let resolved = resolve_type(&local_symtab, &param.param_type)?;
                    local_symtab
                        .vars
                        .insert(param.param_name.clone(), resolved);
                }

                // Add local variables
                for var in &func.local_vars {
                    let resolved = resolve_type(&local_symtab, &var.var_type)?;
                    local_symtab
                        .vars
                        .insert(var.var_name.clone(), resolved.clone());
                    if let Some(init) = &var.var_init {
                        let init_type = check_expr(&local_symtab, init)?;
                        if !types_equal(&resolved, &init_type) {
                            return Err(TypeError(format!(
                                "Variable {}: initializer type mismatch",
                                var.var_name
                            )));
                        }
                    }
                }

                // Check function body
                let return_type = if let Some(t) = &func.return_type {
                    Some(resolve_type(&local_symtab, t)?)
                } else {
                    None
                };

                for stmt in &func.body {
                    check_stmt(&mut local_symtab, &return_type, stmt)?;
                }
            }

            Declaration::DType(_) => {}
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_type_check() {
        // program Test; var x: integer; .
        let prog = Program {
            program_name: "Test".to_string(),
            declarations: vec![Declaration::DVar(VarDecl {
                var_name: "x".to_string(),
                var_type: TypeExpr::TInteger,
                var_init: None,
            })],
        };
        assert!(check_program(&prog).is_ok());
    }

    #[test]
    fn test_undefined_variable() {
        // program Test; function main(): integer; begin return x end; .
        let prog = Program {
            program_name: "Test".to_string(),
            declarations: vec![Declaration::DFunc(FuncDecl {
                func_name: "main".to_string(),
                params: vec![],
                return_type: Some(TypeExpr::TInteger),
                local_vars: vec![],
                body: vec![Stmt::SReturn(Some(Expr::EVar("x".to_string())))],
            })],
        };
        assert!(check_program(&prog).is_err());
    }

    #[test]
    fn test_val_immutability() {
        // program Test; function main(): integer; begin val x: integer = 10; x := 20; return x end; .
        let prog = Program {
            program_name: "Test".to_string(),
            declarations: vec![Declaration::DFunc(FuncDecl {
                func_name: "main".to_string(),
                params: vec![],
                return_type: Some(TypeExpr::TInteger),
                local_vars: vec![],
                body: vec![
                    Stmt::SValDecl(
                        "x".to_string(),
                        TypeExpr::TInteger,
                        Expr::EInteger(10),
                    ),
                    Stmt::SAssign(Expr::EVar("x".to_string()), Expr::EInteger(20)),
                    Stmt::SReturn(Some(Expr::EVar("x".to_string()))),
                ],
            })],
        };
        let result = check_program(&prog);
        assert!(result.is_err());
        assert!(result.unwrap_err().0.contains("Cannot assign to val"));
    }
}
