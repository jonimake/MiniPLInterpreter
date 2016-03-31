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


predict set

prog    -> stmts "$$"                           {var, id, for, read, print, assert, $$}

stmts   -> stmt stmts                           {var, id, for, read, print, assert}
stmts   -> e

stmt    -> var id : type [:= expr]              {var}
stmt    -> id := expr                           {id}
stmt    -> for id in expr .. do stmts end for   {for}
stmt    -> read id                              {read}
stmt    -> print expr                           {print}
stmt    -> assert ( expr )                      {assert}

expr    -> opnd op opnd
expr    -> unary_op opnd                        {!}

opnd    -> int                                  {1,2,3,4,5,6,7,8,9}
opnd    -> string
opnd    -> id
opnd    -> ( expr )                             {(}

type    -> integer                              {integer}
type    -> string                               {string}
type    -> bool                                 {bool}

id      -> regex                                {a-zA-Z}

*/

#[derive(Clone, Copy, PartialEq, Debug)]
enum ParseToken<'a> {
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

    range,
    NA
}


enum PredictSet {
    Integer
}

use std::iter::Iterator;
use token::Token;
use token::TokenType;
use std::boxed::Box;

struct Parser<'a> {
    tokens: Iterator<Item = ParseToken<'a>>
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


pub fn interpret<'a, I> (mut tokens: I) -> ()
    where I: Iterator< Item = Token<'a> > {
    let tokens = tokens.map(refine_type);
    //let mut variables = HashMap::new();
    for t in tokens {

        println!("{:?}", t);
    }


    /*
    while let Some(token) = tokens.next() {
        //refine token type
        let parsetoken = refine_type(token);
        println!("{:?}", parsetoken);
    }*/

    println!("Done");
}


pub fn  get_ast<'a>(tokens: &mut Iterator< Item = Token>) -> ()  {

    while let Some(token) = tokens.next() {
        //refine token type
        let parsetoken = refine_type(token);
        println!("{:?}", parsetoken);
    }

}

#[test]
fn test_add_token() -> () {
}

