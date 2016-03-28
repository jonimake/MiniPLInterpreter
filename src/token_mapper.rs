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
use token_iterator::TokenIterator;

pub struct TokenMapper<'a>  {
	tokenIterator: &'a TokenIterator<'a>,
}

impl<'a>  TokenMapper<'a>  {
	pub fn new(it: &'a TokenIterator<'a>) -> TokenMapper<'a> {
		TokenMapper{tokenIterator: it}
	}
}