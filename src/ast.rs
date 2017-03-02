use std::iter::Iterator;
use std::slice::Iter;
use std::iter::Peekable;
use std::collections::hash_map::HashMap;

use parser::TokenIterator;
use parser::Token;
use parser::ValueType;
use parser::TokenType;
use parser;

pub struct AST<'a>  {
    iter: Peekable<Iter<'a, Token<'a>>>,
    variables: HashMap<&'a str, Token<'a>>
}

pub struct Expression {

}

impl<'a> AST<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> AST<'a> {
        AST{
            iter: tokens.iter().peekable(),
            variables: HashMap::new()
        }
    }

    pub fn interpret(&mut self) -> bool {
        self.statements();
        true
    }

    fn statements(&mut self) {
        while self.iter.peek().is_some() {
            self.statement();
        }
    }

    /*
    stmt    -> var id : type [:= expr]              {var}
    stmt    -> id := expr                           {id}
    stmt    -> for id in expr .. do stmts end for   {for}
    stmt    -> read id                              {read}
    stmt    -> print expr                           {print}
    stmt    -> assert ( expr )                      {assert}
    */
    fn statement(&mut self) {
        let tt = self.iter.peek().unwrap().clone();
        match tt.token_type {
            TokenType::VarKeyword => self.varDecl(),
            TokenType::Identifier => self.identifier(),
            //TokenType::For => self.forLoop(),
            //TokenType::Read => self.read(),
            //TokenType::Print => self.print(),
            //TokenType::Assert => self.assertStmt(),
            _ => {}
        };
        println!("variables {:?}", self.variables);
        self.expectNext(TokenType::StatementEnd);

    }

    fn varDecl(&mut self) {
        let keyword: &Token = self.expectNext(TokenType::VarKeyword);
        let id: &Token = self.expectNext(TokenType::Identifier);
        let typeassign: &Token = self.expectNext(TokenType::TypeDeclaration);
        let tokentype: &Token = self.expectNext(TokenType::Type(ValueType::Any));

        self.setVariableValue(id, *tokentype);
        if self.expectPeek(TokenType::ValueDefinition) {
            self.assign(id);
        }

    }

    fn setVariableValue(&mut self, name :&'a Token<'a>, token: Token<'a>) {
        self.variables.insert(name.lexeme.lexeme, token);
    }

    fn getVariableValue(&mut self, name :&'a Token<'a>) -> Token<'a> {
        let tt = self.variables.get(name.lexeme.lexeme).unwrap();
        *tt
    }

    fn assign(&mut self, id: &'a Token<'a>) {
        self.expectNext(TokenType::ValueDefinition);
        println!("{:?}", self.iter.clone());
        let expressionValue: Token<'a> = self.expression();
        self.setVariableValue(id, expressionValue);
    }

    fn shuntingyard(&mut self) -> Vec<Token<'a>> {
        let mut tokens: Vec<Token<'a>> = vec!();

        let token: &Token = self.iter.next().unwrap();
        let tt: TokenType = token.token_type;
        match tt {
            TokenType::Addition |
            TokenType::Subtraction |
            TokenType::Multiplication |
            TokenType::Division => {},
            TokenType::LParen => {},
            TokenType::RParen => {},
            _ => {tokens.push(*token)}
        }
        tokens
    }

/*
 <expr>   ::=  <opnd> <op> <opnd>
           |   [ <unary_op> ] <opnd>

 <opnd>   ::=  <int>
           |   <string>
           |   <var_ident>
           |   "(" expr ")"
*/
    fn expression(&mut self) -> Token<'a> {
        let next: &Token<'a> = self.iter.next().unwrap();
        let leftOperand: Token<'a> = match next.token_type {
            TokenType::LParen => { //handle nested expression
                let res = self.expression();
                self.expectNext(TokenType::RParen);
                res
            },
            TokenType::IntegerValue(_) => *next,
            TokenType::StringLiteral => *next,
            TokenType::Identifier => {self.getVariableValue(next)},
            _ => panic!("Unexpected token {:?}", next)
        };


        let operation = *(self.iter.peek().unwrap());
        let singleoperand = match operation.token_type {
            TokenType::Addition |
            TokenType::Subtraction |
            TokenType::Multiplication |
            TokenType::Division => false,
            _ => true
        };
        println!("Is single operand: {} {:?}", singleoperand, operation);
        if !singleoperand {
            let operation = *(self.iter.next().unwrap());
            let rightOperand: Token<'a> = match next.token_type {
                TokenType::LParen => { //handle nested expression
                    let res = self.expression();
                    self.expectNext(TokenType::RParen);
                    res
                },
                TokenType::IntegerValue(_) => *next,
                TokenType::StringLiteral => *next,
                TokenType::Identifier => self.getVariableValue(next),
                _ => panic!("Unexpected token {:?}", next)
            };
            let result = self.operation(leftOperand, operation, rightOperand);
            return result;
        } else {
            return leftOperand;
        }
    }

    fn operation(&mut self, l: Token<'a>, op: Token<'a>, r: Token<'a>) -> Token<'a> {
        let result = match op.token_type {
            TokenType::Addition => {},
            TokenType::Subtraction => {},
            TokenType::Multiplication => {},
            TokenType::Division => {},
            _ => unimplemented!()
        };
        result
    }

    fn handleAddition(&self, l: Token<'a>, r: Token<'a>) -> Token<'a> {

    }


    fn identifier(&mut self) {
        self.expectNext(TokenType::Identifier);

    }

    fn forLoop(&mut self) -> bool {
        false
    }

    fn read(&mut self) -> bool {
        false
    }

    fn print(&mut self) {
        self.expectNext(TokenType::Print);
        let varId = self.expectNext(TokenType::Identifier);
        println!("{:?}={:?}",varId.lexeme.lexeme,self.getVariableValue(varId))
    }

    fn assertStmt(&mut self) -> bool {
        false
    }

    fn expectPeek(&mut self, tt: TokenType) -> bool {
        let maybetoken = self.iter.peek();
        if let Some(token) = maybetoken {
            return token.token_type == tt
        }
        false
    }

    fn expectNext(&mut self, tt: TokenType) -> &'a Token<'a> {
        let maybetoken = self.iter.next();
        if let Some(token) = maybetoken {
            if token.token_type == tt {
                return token
            } else {
                panic!("Unexpected token {:?}, expected type {:?}", token, tt)
            }
        }
        panic!("Expected token but iterator was empty")
    }
}
/*
 <prog>   ::=  <stmts>
 <stmts>  ::=  <stmt> ";" ( <stmt> ";" )*
 <stmt>   ::=  "var" <var_ident> ":" <type> [ ":=" <expr> ]
           |   <var_ident> ":=" <expr>
           |   "for" <var_ident> "in" <expr> ".." <expr> "do"
                <stmts> "end" "for"
           |   "read" <var_ident>
           |   "print" <expr>
           |   "assert" "(" <expr> ")"

 <expr>   ::=  <opnd> <op> <opnd>
           |   [ <unary_op> ] <opnd>

 <opnd>   ::=  <int>
           |   <string>
           |   <var_ident>
           |   "(" expr ")"

 <type>   ::=  "int" | "string" | "bool"[unary_op]
 <var_ident> ::= <ident>

 <reserved keyword> ::=
              "var" | "for" | "end" | "in" | "do" | "read" |
              "print" | "int" | "string" | "bool" | "assert"


predict set

prog    -> stmts "$$"                           {var, id, for, read, print, assert, $$}

stmts   -> stmt stmts                           {var, id, for, read, print, assert}
stmts   -> e

stmt    -> var id : type [:= expr]              {var}

stmt    -> var id : type var_tail
var_tail -> := expr | e                         {:=}

stmt    -> id := expr                           {id}
stmt    -> for id in expr .. do stmts end for   {for}
stmt    -> read id                              {read}
stmt    -> print expr                           {print}
stmt    -> assert ( expr )                      {assert}

expr    -> opnd op opnd
expr    -> [unary_op] opnd                        {!}

opnd    -> int                                  {1,2,3,4,5,6,7,8,9}
opnd    -> string
opnd    -> id                                   {a-zA-Z}
opnd    -> ( expr )                             {(}

op      -> + | - | * | / | < | = | & | !        {+, -, *, /, <, =, &, !}

type    -> integer                              {integer}
type    -> string                               {string}
type    -> bool                                 {bool}

id      -> regex                                {a-zA-Z}
*/

