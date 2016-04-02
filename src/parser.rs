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

#[derive(Clone, Copy, PartialEq, Hash, Eq, Debug)]
pub enum ParseToken<'a> {
    var_decl,
    id(&'a str),
    type_decl,      //:
    assignment,     //:=
    equal,     //=

    integer_type,
    string_type,
    boolean_type,
    integer(i32),
    string(&'a str),
    boolean(bool),
    read,
    print,
    assert,

    for_loop_begin,     //for
    for_loop_in,
    for_loop_do,
    for_loop_end,   //end

    statement_end,      //;
    lparen,
    rparen,
    add,
    sub,
    mul,
    div,
    not,

    range,
    undefined
}


enum PredictSet {
    Integer
}

#[derive(Clone, Copy, PartialEq, Hash, Eq, Debug)]
struct TypeVal<'a> {
    val_type: Option<ParseToken<'a>>,
    val: Option<ParseToken<'a>>,
}

use std::iter::Iterator;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::collections::HashMap;
use token::Token;
use token::TokenType;
use std::boxed::Box;
use ast::Ast;

pub struct Parser<'a> {
    token_stack: Vec<ParseToken<'a>>,
    variables: HashMap<ParseToken<'a>, Option<TypeVal<'a>>>,
    operand_stack: Vec<ParseToken<'a>>,
    operator_stack: Vec<ParseToken<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        let mut refined_tokens: Vec<ParseToken<'a>> = tokens.into_iter().map(refine_type).collect();
        refined_tokens.reverse();
        Parser {
            token_stack: refined_tokens,
            variables: HashMap::new(),
            operand_stack: Vec::new(),
            operator_stack: Vec::new()
        }
    }


    pub fn interpret (&mut self) {
        self.program();
    }

    fn program(&mut self) {
        let mut statements: Vec<Ast<'a>> = Vec::new();
        while(!self.token_stack.is_empty()) {
            let ast: Ast<'a> = self.statement();
            statements.push(ast);
            println!("Statements: {:?}", statements);
            let statement_end = self.token_stack.pop();
            if(statement_end != Some(ParseToken::statement_end)) {panic!("Unexpected end of statement")}
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
    fn statement(&mut self) -> Ast<'a> {
        let t = self.token_stack.last().cloned();
        let mut ntype = ParseToken::undefined;
        let node: Ast<'a> = match t {
            Some(ParseToken::var_decl) => {self.var_decl()},
            Some(ParseToken::id(var_id)) => {self.var_assignment()},
            Some(ParseToken::read) => {self.read()},
            Some(ParseToken::print) => {self.print()},
            Some(ParseToken::assert) => {panic!("todo assert")},
            _ => panic!("unexpected token {:?}", t)
        };
        //Box::new(Ast{node_type: ntype, lhs: None, rhs: None, value: None})
        node
    }

    fn read(&mut self) -> Ast<'a> {
        let read = self.token_stack.pop();
        let id = self.token_stack.pop();
        let id_box = Box::new(Ast{node_type: id.unwrap(), lhs: None, rhs: None, value: id.unwrap(), value_type: id.unwrap()});
        Ast{node_type: read.unwrap(), lhs: Some(id_box), value_type: ParseToken::undefined, rhs: None, value: read.unwrap()}
    }

    fn print(&mut self) -> Ast<'a> {
        let print = self.token_stack.pop();
        let expr = self.expression();

        Ast{node_type: print.unwrap(), lhs: Some(Box::new(expr)), value_type: ParseToken::print, rhs: None, value: ParseToken::undefined}
    }

    fn var_decl(&mut self) -> Ast<'a> {

        if(self.token_stack.pop() != Some(ParseToken::var_decl)) {panic!("syntax error, no var")}
        let id: Option<ParseToken<'a>> = self.token_stack.pop();

        let id = match id {
            Some(ParseToken::id(_)) => {id.unwrap()},
            _ => panic!("no")
        };
        if( self.token_stack.pop() != Some(ParseToken::type_decl)) {panic!("syntax error, ':' required")}

        let ttype = self.token_stack.pop();
        let ttype = match ttype {
            Some(ParseToken::integer_type) => {ttype.unwrap()},
            Some(ParseToken::string_type) => {ttype.unwrap()},
            Some(ParseToken::boolean_type) => {ttype.unwrap()},
            _ => panic!("Not a valid type {:?}", ttype)
        };

        let var_node = Ast{node_type: ParseToken::var_decl, lhs: None, value_type: ttype, rhs: None, value: id};

        if (self.token_stack.last().cloned() == Some(ParseToken::assignment)) {
            let assignment = self.token_stack.pop();
            let expr_node = self.expression();
            let assignment_node = Ast{
                node_type: ParseToken::assignment,
                lhs: Some(Box::new(var_node)),
                rhs: Some(Box::new(expr_node)),
                value: ParseToken::undefined,
                value_type: ParseToken::undefined};
            println!("parsed expression");
            assignment_node
        } else {
            var_node
        }
    }

    fn var_assignment(&mut self) -> Ast<'a> {
        let var_id = self.token_stack.pop();
        let assignment = self.token_stack.pop();
        let expr = self.expression();

        Ast{node_type: assignment.unwrap(),
            lhs: Some(Box::new(
                Ast{
                    node_type: var_id.unwrap(),
                    value: var_id.unwrap(),
                    value_type: var_id.unwrap(),
                    rhs: None,
                    lhs: None
                })),
            rhs: Some(Box::new(expr)),
            value: ParseToken::undefined,
            value_type: ParseToken::undefined}
    }

    //<expr>   ::=  <opnd> <op> <opnd>
    //          |   [ <unary_op> ] <opnd>

    fn expression(&mut self) -> Ast<'a> {

        println!("stack: {:?}", self.token_stack);
        let token = self.token_stack.last().cloned();
        //let mut ast = Ast{node_type: ParseToken::undefined, lhs: None, rhs: None, value: ParseToken::undefined, value_type: ParseToken::undefined};
        println!("Expression: {:?}", token);
        let ast: Ast<'a> = match token {
            Some(ParseToken::not) => { //handle unary op case
                self.expression_tail()
            },
            Some(_) => {
                let lhs = self.operand();
                if(self.token_stack.last().cloned() != Some(ParseToken::statement_end)) {
                    let mut tail = self.expression_tail();
                    tail.lhs = Some(Box::new(lhs));
                    //let op = self.token_stack.pop();
                    //let rhs = self.operand();
                    tail
                } else {
                    lhs
                }
            }
            _ => panic!("expression parse failure {:?}", token)
        };
        ast
    }

    fn expression_tail(&mut self) -> Ast<'a> {
        println!("stack: {:?}", self.token_stack);
        let t = self.token_stack.pop();
        println!("Expr tail {:?}", t);
        match t {
            Some(ParseToken::not) => {
                let lhs = self.expression();
                Ast{node_type: t.unwrap(), lhs: Some(Box::new(lhs)), rhs: None, value: ParseToken::undefined, value_type: ParseToken::undefined}
            }
            Some(_) => { //some binary op
                let rhs = self.operand();
                Ast{node_type: t.unwrap(), lhs: None, rhs: Some(Box::new(rhs)), value: ParseToken::undefined, value_type: ParseToken::undefined}
            }
            _ => panic!("expression tail parse failure {:?}", t)
        }
    }


    fn operand(&mut self) -> Ast<'a> {
        let oprnd = self.token_stack.pop();
        println!("Operand {:?}", oprnd);
        let ast = match oprnd {
            Some(ParseToken::lparen) => {
                let expr = self.expression();
                let rparen = self.token_stack.pop();
                match rparen {
                    Some(ParseToken::rparen) => {},
                    _ => panic!("Missing right parenthesis")
                }
                expr
            },
            Some(ParseToken::integer(i)) => {
                Ast{node_type: oprnd.unwrap(), lhs: None, rhs: None, value: oprnd.unwrap(), value_type: ParseToken::integer_type}
            },
            Some(ParseToken::string(str)) => {
                Ast{node_type: oprnd.unwrap(), lhs: None, rhs: None, value: oprnd.unwrap(), value_type: ParseToken::string_type}
            },
            Some(ParseToken::boolean(bool)) => {
                 Ast{node_type: oprnd.unwrap(), lhs: None, rhs: None, value: oprnd.unwrap(), value_type: ParseToken::boolean_type}
             },
             Some(ParseToken::id(id)) => {
                Ast{node_type: oprnd.unwrap(), lhs: None, rhs: None, value: oprnd.unwrap(), value_type: oprnd.unwrap()}
            }
            _ => panic!("Operand parse error {:?}", oprnd)
        };
        ast
    }

}

//let single_char_token = Regex::new(r"\+|-|\*|/|<|=|&|!|\(|\)|;|\.|:").unwrap();
fn get_single_char_type(token: Token) -> ParseToken {
    let lexeme = token.lexeme;
    match lexeme {
        ":" => ParseToken::type_decl,
        "+" => ParseToken::add,
        "-" => ParseToken::sub,
        "*" => ParseToken::mul,
        "/" => ParseToken::div,
        ";" => ParseToken::statement_end,
        "(" => ParseToken::lparen,
        ")" => ParseToken::rparen,
        "=" => ParseToken::equal,
        "!" => ParseToken::not,
        _   => panic!("Parse error while mapping token {:?}, line {}, col {}", lexeme, token.line, token.column)
    }
}

fn get_two_char_type(token: Token) -> ParseToken {
    let lexeme = token.lexeme;
    match lexeme {
        ":=" => ParseToken::assignment,
        ".." => ParseToken::range,
        _    => panic!("Parse error while mapping token {:?}, line {}, col {}", lexeme, token.line, token.column)
    }
}

fn get_keyword_type(token: Token) -> ParseToken {
    let lexeme = token.lexeme;
    match lexeme {
        "for"       => ParseToken::for_loop_begin,
        "var"       => ParseToken::var_decl,
        "read"      => ParseToken::read,
        "print"     => ParseToken::print,
        "assert"    => ParseToken::assert,
        "int"       => ParseToken::integer_type,
        "bool"      => ParseToken::boolean_type,
        "string"    => ParseToken::string_type,
        "in"        => ParseToken::for_loop_in,
        "do"        => ParseToken::for_loop_do,
        "end"       => ParseToken::for_loop_end,
        _           => panic!("Parse error while mapping token {:?}, line {}, col {}", lexeme, token.line, token.column)
    }
}

fn get_integer_type(token: Token) -> ParseToken {
    let lexeme = token.lexeme;
    let number: i32 = lexeme.parse().unwrap();
    ParseToken::integer(number)
}

fn get_identifier_type(token: Token) -> ParseToken {
    ParseToken::id(token.lexeme)
}

fn get_string_type(token: Token) -> ParseToken {
    ParseToken::string(token.lexeme.trim_left_matches('"').trim_right_matches('"'))
}

fn refine_type(token: Token) -> ParseToken {
    let parsetoken = match token.token_type {
        TokenType::single_char      => get_single_char_type(token),
        TokenType::two_char         => get_two_char_type(token),
        TokenType::keyword          => get_keyword_type(token),
        TokenType::integer          => get_integer_type(token),
        TokenType::string_literal   => get_string_type(token),
        TokenType::identifier       => get_identifier_type(token),
        _ => {
            panic!("Error in on line {} col {}", token.line, token.column);
        }
    };
    parsetoken
}
//var i : int := 1 + 2
#[test]
fn test_parse_expression() -> () {
    let tokens = vec!(
        Token{line: 1, column: 1, lexeme: "var", token_type:TokenType::keyword},
        Token{line: 1, column: 5, lexeme: "i", token_type:TokenType::identifier},
        Token{line: 1, column: 7, lexeme: ":", token_type:TokenType::single_char},
        Token{line: 1, column: 9, lexeme: "int", token_type:TokenType::keyword},
        Token{line: 1, column: 17-4, lexeme: ":=", token_type:TokenType::two_char},
        Token{line: 1, column: 20-4, lexeme: "1", token_type:TokenType::integer},
        Token{line: 1, column: 22-4, lexeme: "+", token_type:TokenType::single_char},
        Token{line: 1, column: 24-4, lexeme: "2", token_type:TokenType::integer},
        Token{line: 1, column: 25-4, lexeme: ";", token_type:TokenType::single_char}
    );
    let mut p = Parser::new(tokens);
    p.interpret();
}
/*
pub line: usize,
pub column: usize,
pub lexeme: &'a str,
pub token_type: TokenType
*/