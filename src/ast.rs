use std::boxed::Box;
use parser::ParseToken;

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub struct Ast<'a> {
    pub node_type:  ParseToken<'a>,
    pub value:      ParseToken<'a>,
    pub value_type: ParseToken<'a>,

    // Box type below is basically a reference to a heap stored
    // value that is needed for recursive struct references
    pub lhs: Option<Box<Ast<'a>>>,
    pub rhs: Option<Box<Ast<'a>>>
}


impl<'a> Ast<'a> {

}