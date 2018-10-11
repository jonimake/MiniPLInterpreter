#![feature(uniform_paths, tool_lints)]
#![feature(try_from)]
#![feature(nll)]
#![feature(str_escape)]

#![allow(unknown_lints)]
#![warn(clippy::all)]
#![allow(dead_code)]



pub mod lexer;
pub mod parser;
pub mod ast;
pub mod ast_visualizer;

#[macro_use]
extern crate log;
use simplelog;
use structopt;
#[macro_use]
extern crate structopt_derive;


use simplelog::Config;
use simplelog::LogLevelFilter;
use simplelog::TermLogger;
use structopt::StructOpt;

use crate::lexer::lexeme_iterator::LexemeIterator;
use crate::parser::interpreter::Interpreter;
use crate::parser::interpreter::InterpreterState;
use crate::parser::token::Token;
use crate::parser::token_iterator::TokenIterator;
use crate::ast::Ast;
use crate::parser::ast_parser;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;

use ast_visualizer::DotGraphNode;
use ast_parser::AstParser;

pub fn parse_string(line: &str) -> Result<Ast, String> {
    let lexeme_it = LexemeIterator::new(line);
    let mut token_iterator = TokenIterator::new(lexeme_it);
    let mut ast_parser = AstParser::new(&mut token_iterator);
    ast_parser.build_ast()
}

pub fn parse_into_dot(txt: &str) -> Result<String, String> {
    let dot_str: Ast = parse_string(&txt)?;
    let dotnodes = dot_str.convert();

    let mut out: Vec<String> = dotnodes.iter().flat_map(|e| e.get_dot_output()).collect::<Vec<String>>();
    out.insert(0, "digraph G {".to_owned());
    out.push("}".to_owned());
    let lines = out.join("\n");
    Ok(lines)
}


#[test]
fn sample1_var_definition_expression_print() {
    let code = r#"
var X : int := 4 + (6 * 2);
print X;
"#;
    let lexeme_it = LexemeIterator::new(code);
    let mut token_iterator: TokenIterator<LexemeIterator<'_>> = TokenIterator::new(lexeme_it);

    let mut ast_parser = ast_parser::AstParser::new(&mut token_iterator);
    let result = ast_parser.build_ast().unwrap();
    let nodes = result.convert();
    let out = nodes.iter().flat_map(|e| e.get_dot_output()).collect::<Vec<String>>();
    for s in out {
        println!("{}", s);
    }
    unimplemented!()
    //let viztree: Tree<i32, String> = result.into();
}

#[test]
fn sample2_loop_print() {
    let code = r#"
var nTimes : int := 0;
print nTimes;
print "How many times?";
read nTimes;
print nTimes;
var x : int;
for x in 0..nTimes-1 do
	print x;
	print " : Hello, World!\n";
end for;
print x;
print nTimes;
assert (x = (nTimes-1));
"#;
    let lexeme_it = LexemeIterator::new(code);
    let mut token_iterator: TokenIterator<LexemeIterator<'_>> = TokenIterator::new(lexeme_it);

    let mut ast_parser = ast_parser::AstParser::new(&mut token_iterator);
    let result = ast_parser.build_ast().unwrap();
    let nodes = result.convert();
    let out = nodes.iter().flat_map(|e| e.get_dot_output()).collect::<Vec<String>>();
    for s in out {
        println!("{}", s);
    }
}
/*
#[test]
fn sample4_decl_assign_print() {
    let code = r#"
var X : int;
X := 15;
print X;
"#;
    parse_file(code).unwrap();
}

#[test]
fn test_decl_assign_boolean() {
    let code = r#"
var X : bool;
X := 15;
print X;
"#;
    parse_file(code).unwrap();
}
*/