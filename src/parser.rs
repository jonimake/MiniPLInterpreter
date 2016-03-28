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

 <type>   ::=  "int" | "string" | "bool"
 <var_ident> ::= <ident>

 <reserved keyword> ::=
              "var" | "for" | "end" | "in" | "do" | "read" |
              "print" | "int" | "string" | "bool" | "assert"
*/
use std::iter::Iterator;

pub enum PlOperand {

}
//'+' | '-' | '*' | '/' | '<' | '=' | '&' | '!'
pub enum PlOperation {
    plus,
    minus,
    mul,
    div,
    lessthan,
    assign,
    and,
    not
}

pub enum Expr {
    unary,
    binary(PlOperand)
}

pub enum Syntax_tree {
    left(Expr, Box<Syntax_tree>),
    right(Expr, Box<Syntax_tree>),
    leaf(Expr),
    empty,
}

struct Parser {
    num_tokens: i32,
    syntax_tree_root: Box<Syntax_tree>
}

/*
struct Parser<'a> {
    asd: &'a mut Iterator<Item=&'a str>
}
*/
impl Parser {
    fn new() -> Parser {
        Parser{num_tokens: 0, syntax_tree_root: Box::new(Syntax_tree::empty)}
    }

    fn add_token(&mut self, token: &str) -> bool {
        match token {
            "+" => {add_root(token)},
            _ => {add_leaf(token)},
        };
        true
    }

}


/*

(print

*/

#[test]
fn test_add_token() -> () {
    let mut parser = Parser::new();
    parser.add_token("223");
    parser.add_token("+");
    parser.add_token("12");

}

