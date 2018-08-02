/*
stmt    -> var id : type [:= expr]              {var}
stmt    -> id := expr                           {id}
stmt    -> for id in expr .. do stmts end for   {for}
stmt    -> read id                              {read}
stmt    -> print expr                           {print}
stmt    -> assert ( expr )                      {assert}
*/

pub enum Ast {
    VarDecl{id: String, value_type: Type, value: Box<Ast>}
}

pub enum Type {
    String,
    Boolean,
    Integer
}