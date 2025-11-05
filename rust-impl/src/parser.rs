/// Parser for Pascal-like language
use crate::ast::*;
use crate::lexer::{Lexer, Token};
use std::fmt;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        if self.current_token == expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError {
                message: format!("Expected {:?}, got {:?}", expected, self.current_token),
            })
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.current_token.clone() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError {
                message: format!("Expected identifier, got {:?}", self.current_token),
            }),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        self.expect(Token::Program)?;
        let program_name = self.expect_ident()?;
        self.expect(Token::Semicolon)?;

        let mut declarations = Vec::new();
        while self.current_token != Token::Dot {
            declarations.push(self.parse_declaration()?);
        }

        self.expect(Token::Dot)?;

        Ok(Program {
            program_name,
            declarations,
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        match &self.current_token {
            Token::Type => self.parse_type_decl(),
            Token::Var => self.parse_var_decl(),
            Token::Function | Token::Procedure => self.parse_func_decl(),
            _ => Err(ParseError {
                message: format!("Expected declaration, got {:?}", self.current_token),
            }),
        }
    }

    fn parse_type_decl(&mut self) -> Result<Declaration, ParseError> {
        self.expect(Token::Type)?;
        let type_name = self.expect_ident()?;
        self.expect(Token::Eq)?;
        let type_def = self.parse_type_expr()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::DType(TypeDecl {
            type_name,
            type_def,
        }))
    }

    fn parse_var_decl(&mut self) -> Result<Declaration, ParseError> {
        self.expect(Token::Var)?;
        let var_name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let var_type = self.parse_type_expr()?;

        let var_init = if self.current_token == Token::Assign {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(Declaration::DVar(VarDecl {
            var_name,
            var_type,
            var_init,
        }))
    }

    fn parse_func_decl(&mut self) -> Result<Declaration, ParseError> {
        let is_function = self.current_token == Token::Function;
        self.advance();

        let func_name = self.expect_ident()?;
        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        if self.current_token != Token::RParen {
            loop {
                let param_name = self.expect_ident()?;
                self.expect(Token::Colon)?;
                let param_type = self.parse_type_expr()?;
                params.push(Param {
                    param_name,
                    param_type,
                });

                if self.current_token == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(Token::RParen)?;

        let return_type = if is_function {
            self.expect(Token::Colon)?;
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        // Parse local variable declarations
        let mut local_vars = Vec::new();
        while self.current_token == Token::Var {
            if let Declaration::DVar(var_decl) = self.parse_var_decl()? {
                local_vars.push(var_decl);
            }
        }

        // Parse body
        self.expect(Token::Begin)?;
        let body = self.parse_stmt_list()?;
        self.expect(Token::End)?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::DFunc(FuncDecl {
            func_name,
            params,
            return_type,
            local_vars,
            body,
        }))
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        match &self.current_token {
            Token::TInteger => {
                self.advance();
                Ok(TypeExpr::TInteger)
            }
            Token::TReal => {
                self.advance();
                Ok(TypeExpr::TReal)
            }
            Token::TBoolean => {
                self.advance();
                Ok(TypeExpr::TBoolean)
            }
            Token::TChar => {
                self.advance();
                Ok(TypeExpr::TChar)
            }
            Token::TString => {
                self.advance();
                Ok(TypeExpr::TString)
            }
            Token::Caret => {
                self.advance();
                let inner = self.parse_type_expr()?;
                Ok(TypeExpr::TPointer(Box::new(inner)))
            }
            Token::Array => {
                self.advance();
                self.expect(Token::LBrack)?;
                let size = match self.current_token {
                    Token::Integer(n) => {
                        self.advance();
                        n as usize
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected array size".to_string(),
                        })
                    }
                };
                self.expect(Token::RBrack)?;
                self.expect(Token::Of)?;
                let elem_type = self.parse_type_expr()?;
                Ok(TypeExpr::TArray(Box::new(elem_type), size))
            }
            Token::Record => {
                self.advance();
                let mut fields = Vec::new();
                while self.current_token != Token::End {
                    let field_name = self.expect_ident()?;
                    self.expect(Token::Colon)?;
                    let field_type = self.parse_type_expr()?;
                    self.expect(Token::Semicolon)?;
                    fields.push(RecordField {
                        field_name,
                        field_type,
                    });
                }
                self.expect(Token::End)?;
                Ok(TypeExpr::TRecord(fields))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(TypeExpr::TNamed(name))
            }
            _ => Err(ParseError {
                message: format!("Expected type expression, got {:?}", self.current_token),
            }),
        }
    }

    fn parse_stmt_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !matches!(
            self.current_token,
            Token::End | Token::Else | Token::Eof
        ) {
            stmts.push(self.parse_stmt()?);
            if self.current_token == Token::Semicolon {
                self.advance();
            }
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match &self.current_token {
            Token::Var => self.parse_var_stmt(),
            Token::Val => self.parse_val_stmt(),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::Writeln => self.parse_writeln_stmt(),
            Token::Write => self.parse_write_stmt(),
            Token::Readln => self.parse_readln_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Begin => self.parse_block_stmt(),
            _ => self.parse_assign_or_call_stmt(),
        }
    }

    fn parse_var_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Var)?;
        let name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let var_type = self.parse_type_expr()?;
        self.expect(Token::Assign)?;
        let init = self.parse_expr()?;
        Ok(Stmt::SVarDecl(name, var_type, init))
    }

    fn parse_val_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Val)?;
        let name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let var_type = self.parse_type_expr()?;
        self.expect(Token::Eq)?;
        let init = self.parse_expr()?;
        Ok(Stmt::SValDecl(name, var_type, init))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::If)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Then)?;

        let mut then_stmts = Vec::new();
        if self.current_token == Token::Begin {
            self.advance();
            then_stmts = self.parse_stmt_list()?;
            self.expect(Token::End)?;
        } else {
            then_stmts.push(self.parse_stmt()?);
        }

        let else_stmts = if self.current_token == Token::Else {
            self.advance();
            let mut stmts = Vec::new();
            if self.current_token == Token::Begin {
                self.advance();
                stmts = self.parse_stmt_list()?;
                self.expect(Token::End)?;
            } else {
                stmts.push(self.parse_stmt()?);
            }
            Some(stmts)
        } else {
            None
        };

        Ok(Stmt::SIf(cond, then_stmts, else_stmts))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::While)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Do)?;

        let mut body = Vec::new();
        if self.current_token == Token::Begin {
            self.advance();
            body = self.parse_stmt_list()?;
            self.expect(Token::End)?;
        } else {
            body.push(self.parse_stmt()?);
        }

        Ok(Stmt::SWhile(cond, body))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::For)?;
        let var = self.expect_ident()?;
        self.expect(Token::Assign)?;
        let start = self.parse_expr()?;

        let ascending = match self.current_token {
            Token::To => {
                self.advance();
                true
            }
            Token::Downto => {
                self.advance();
                false
            }
            _ => {
                return Err(ParseError {
                    message: "Expected 'to' or 'downto'".to_string(),
                })
            }
        };

        let end = self.parse_expr()?;
        self.expect(Token::Do)?;

        let mut body = Vec::new();
        if self.current_token == Token::Begin {
            self.advance();
            body = self.parse_stmt_list()?;
            self.expect(Token::End)?;
        } else {
            body.push(self.parse_stmt()?);
        }

        // For downto loops, we'll store the end expr as-is and handle it in codegen
        Ok(Stmt::SFor(var, start, end, body))
    }

    fn parse_writeln_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Writeln)?;
        self.expect(Token::LParen)?;
        let mut exprs = Vec::new();
        if self.current_token != Token::RParen {
            loop {
                exprs.push(self.parse_expr()?);
                if self.current_token == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;
        Ok(Stmt::SWriteln(exprs))
    }

    fn parse_write_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Write)?;
        self.expect(Token::LParen)?;
        let mut exprs = Vec::new();
        if self.current_token != Token::RParen {
            loop {
                exprs.push(self.parse_expr()?);
                if self.current_token == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;
        Ok(Stmt::SWrite(exprs))
    }

    fn parse_readln_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Readln)?;
        self.expect(Token::LParen)?;
        let mut idents = Vec::new();
        if self.current_token != Token::RParen {
            loop {
                idents.push(self.expect_ident()?);
                if self.current_token == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;
        Ok(Stmt::SReadln(idents))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Return)?;
        if matches!(
            self.current_token,
            Token::Semicolon | Token::End | Token::Eof
        ) {
            Ok(Stmt::SReturn(None))
        } else {
            Ok(Stmt::SReturn(Some(self.parse_expr()?)))
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Begin)?;
        let stmts = self.parse_stmt_list()?;
        self.expect(Token::End)?;
        Ok(Stmt::SBlock(stmts))
    }

    fn parse_assign_or_call_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;

        // Check if this is an assignment
        if self.current_token == Token::Assign {
            self.advance();
            let rval = self.parse_expr()?;
            Ok(Stmt::SAssign(expr, rval))
        } else if let Expr::ECall(name, args) = expr {
            // It's a procedure call
            Ok(Stmt::SCall(name, args))
        } else {
            Err(ParseError {
                message: "Expected assignment or procedure call".to_string(),
            })
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;

        while self.current_token == Token::Or {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::EBinop(BinOp::Or, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison_expr()?;

        while self.current_token == Token::And {
            self.advance();
            let right = self.parse_comparison_expr()?;
            left = Expr::EBinop(BinOp::And, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_comparison_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_additive_expr()?;

        if let Some(op) = match self.current_token {
            Token::Eq => Some(BinOp::Eq),
            Token::Ne => Some(BinOp::Ne),
            Token::Lt => Some(BinOp::Lt),
            Token::Le => Some(BinOp::Le),
            Token::Gt => Some(BinOp::Gt),
            Token::Ge => Some(BinOp::Ge),
            _ => None,
        } {
            self.advance();
            let right = self.parse_additive_expr()?;
            left = Expr::EBinop(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_additive_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative_expr()?;

        while let Some(op) = match self.current_token {
            Token::Plus => Some(BinOp::Add),
            Token::Minus => Some(BinOp::Sub),
            _ => None,
        } {
            self.advance();
            let right = self.parse_multiplicative_expr()?;
            left = Expr::EBinop(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary_expr()?;

        while let Some(op) = match self.current_token {
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            Token::Div => Some(BinOp::Div),
            Token::Mod => Some(BinOp::Mod),
            _ => None,
        } {
            self.advance();
            let right = self.parse_unary_expr()?;
            left = Expr::EBinop(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.current_token {
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::EUnop(UnOp::Neg, Box::new(expr)))
            }
            Token::Not => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::EUnop(UnOp::Not, Box::new(expr)))
            }
            Token::At => {
                self.advance();
                let expr = self.parse_postfix_expr()?;
                Ok(Expr::EAddress(Box::new(expr)))
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            match self.current_token {
                Token::LBrack => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(Token::RBrack)?;
                    expr = Expr::EArrayAccess(Box::new(expr), Box::new(index));
                }
                Token::Dot => {
                    self.advance();
                    let field = self.expect_ident()?;
                    expr = Expr::ERecordAccess(Box::new(expr), field);
                }
                Token::Caret => {
                    self.advance();
                    expr = Expr::EDeref(Box::new(expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        match &self.current_token.clone() {
            Token::Integer(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::EInteger(n))
            }
            Token::Real(r) => {
                let r = *r;
                self.advance();
                Ok(Expr::EReal(r))
            }
            Token::Char(c) => {
                let c = *c;
                self.advance();
                Ok(Expr::EChar(c))
            }
            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::EString(s))
            }
            Token::True => {
                self.advance();
                Ok(Expr::EBoolean(true))
            }
            Token::False => {
                self.advance();
                Ok(Expr::EBoolean(false))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();

                // Check if it's a function call
                if self.current_token == Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    if self.current_token != Token::RParen {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.current_token == Token::Comma {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    Ok(Expr::ECall(name, args))
                } else {
                    Ok(Expr::EVar(name))
                }
            }
            Token::New => {
                self.advance();
                self.expect(Token::LParen)?;
                let type_expr = self.parse_type_expr()?;
                self.expect(Token::RParen)?;
                Ok(Expr::ENew(type_expr))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", self.current_token),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_program() {
        let input = r#"
program Test;
var x: integer;
function main(): integer;
begin
  x := 42;
  return 0
end;
.
"#;
        let mut parser = Parser::new(input);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.program_name, "Test");
        assert_eq!(program.declarations.len(), 2);
    }

    #[test]
    fn test_inline_declarations() {
        let input = r#"
program Test;
function main(): integer;
begin
  var x: integer := 10;
  val y: integer = 20;
  return x + y
end;
.
"#;
        let mut parser = Parser::new(input);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.program_name, "Test");
    }

    #[test]
    fn test_record_type() {
        let input = r#"
program Test;
type Point = record
  x: integer;
  y: integer;
end;
.
"#;
        let mut parser = Parser::new(input);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.program_name, "Test");
        if let Declaration::DType(type_decl) = &program.declarations[0] {
            assert_eq!(type_decl.type_name, "Point");
            if let TypeExpr::TRecord(fields) = &type_decl.type_def {
                assert_eq!(fields.len(), 2);
            } else {
                panic!("Expected record type");
            }
        } else {
            panic!("Expected type declaration");
        }
    }
}
