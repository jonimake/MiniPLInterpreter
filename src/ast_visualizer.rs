use crate::ast::{Ast, Statement, Expression, Operator, Id, Scalar, Type};

use std::hash::Hash;
use std::hash::Hasher;
use std::collections::hash_map::DefaultHasher;
use std::cmp::Ordering;
use std::borrow::Cow;

static mut INDEX: usize = 0;


type NodeId = usize;

fn get_tree_ptr() -> usize {
    unsafe {
        let ptr = INDEX;
        INDEX += 1;
        return ptr;
    }
    panic!("Failed to get tree ptr")
}

#[derive(Clone)]
pub struct GraphNode {
    pub id: NodeId,
    pub label: String,
    pub children: Vec<NodeId>
}

impl GraphNode {
    pub fn get_dot_output(&self) -> Vec<String> {
        let node_str = format!("{}[label=\"{}\"];", self.id, self.label);
        let mut edge_strings = self.children.iter().map(|e| {
            format!("{} -> {}[label=\"\"]", self.id, e)
        }).collect::<Vec<String>>();
        let mut out = vec![node_str];
        out.append(&mut edge_strings);
        out
    }
}

impl PartialEq for GraphNode {
    fn eq(&self, other: &'_ Self) -> bool {
        self.id == other.id
    }
}

impl PartialOrd for GraphNode {
    fn partial_cmp(&self, other: &'_ Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

pub trait DotGraphNode<T> {
    fn convert(&self) -> T;
}


impl DotGraphNode<GraphNode> for Operator {
    fn convert(&self) -> GraphNode {
        GraphNode {
            id: get_tree_ptr(),
            label: format!("{:?}", self),
            children: Vec::new(),
        }
    }
}


impl DotGraphNode<Vec<GraphNode>> for Scalar {
    fn convert(&self) -> Vec<GraphNode> {
        let id = get_tree_ptr();
        let (label, mut children) = match self {
            Scalar::Var(id) => {
                let child = id.convert();
                ("Var".to_owned(), vec![child])
            },
            Scalar::Str(s) => (s.to_owned().replace("\"", "\\\""), Vec::default()),
            Scalar::Int(i) => (i.to_string(), Vec::default()),
            Scalar::Bool(b) => (b.to_string(), Vec::default())
        };

        let n = GraphNode {
            id,
            label,
            children: children.iter().map(|e| e.id).collect(),
        };

        let mut v = vec![n];
        v.append(&mut children);
        v
    }
}

impl DotGraphNode<Vec<GraphNode>> for Expression {
    fn convert(&self) -> Vec<GraphNode> {
        let id = get_tree_ptr();
        let mut immediate_children: Vec<GraphNode> = Vec::default();
        let label;
        let mut child_nodes = match self {
            Expression::ScalarExpression(scalar) => {
                let graphnodes = scalar.convert();
                label = "Scalar expression";
                immediate_children.push(graphnodes.first().unwrap().clone());
                graphnodes
            },
            Expression::Unary(left, right) => {
                let mut left_node = vec![left.convert()];
                label = "Unary expression";
                let mut right_nodes = right.convert();

                immediate_children.push(left_node.first().unwrap().clone());
                immediate_children.push(right_nodes.first().unwrap().clone());

                left_node.append(&mut right_nodes);
                left_node
            },
            Expression::Binary(left, op, right) => {
                label = "Binary expression";
                let center_node = op.convert();
                let mut left_nodes = left.convert();
                let mut right_nodes = right.convert();

                immediate_children.push(center_node.clone());
                immediate_children.push(left_nodes.first().unwrap().clone());
                immediate_children.push(right_nodes.first().unwrap().clone());

                left_nodes.push(center_node);
                left_nodes.append(&mut right_nodes);
                left_nodes
            },
        };

        let children = immediate_children.iter().map(|e| e.id).collect::<Vec<NodeId>>();
        let node = GraphNode {
            id,
            label: label.to_owned(),
            children,
        };

        let mut graph_nodes = vec![node];
        graph_nodes.append(&mut child_nodes);
        graph_nodes
    }
}


impl DotGraphNode<Vec<GraphNode>> for Ast {
    fn convert(&self) -> Vec<GraphNode> {
        let id = get_tree_ptr();
        let mut children: Vec<NodeId> = Vec::new();
        let mut all_nodes: Vec<GraphNode> = Vec::new();

        match self {
            Ast::Statements(statements) => {
                for statement in statements {
                    let mut statement_nodes = statement.convert();
                    if let Some(first) = statement_nodes.first() {
                        children.push(first.id);
                    }
                    all_nodes.append(&mut statement_nodes);
                }
            },
            _ => unreachable!()
        };


        let node = GraphNode {
            id,
            label: "AST".to_owned(),
            children,
        };
        let mut ast_nodes = vec![node];
        ast_nodes.append(&mut all_nodes);
        ast_nodes
    }
}

impl DotGraphNode<Vec<GraphNode>> for Statement {
    fn convert(&self) -> Vec<GraphNode> {
        let id = get_tree_ptr();
        let label;
        let mut immediate_children: Vec<GraphNode> = Vec::default();
        let mut all_nodes = match self {
            Statement::VarDecl(id, typ) => {
                label = "Var declaration";
                let id_node = id.convert();
                let type_node = typ.convert();
                immediate_children.push(id_node.clone());
                immediate_children.push(type_node.clone());
                vec![id_node, type_node]
            },
            Statement::VarDefn(id, typ, expr) => {
                label = "Var definition";
                let id_node = id.convert();
                let type_node = typ.convert();
                let mut expr_nodes = expr.convert();

                immediate_children.push(id_node.clone());
                immediate_children.push(type_node.clone());
                immediate_children.push(expr_nodes.first().unwrap().clone());

                let mut r = vec![id_node, type_node];
                r.append(&mut expr_nodes);
                r
            },
            Statement::IdAssign(id, expr) => {
                label = "Id assign";
                let id_node = id.convert();
                let mut expr_nodes = expr.convert();
                immediate_children.push(id_node.clone());
                immediate_children.push(expr_nodes.first().unwrap().clone());

                let mut r = vec![id_node];
                r.append(&mut expr_nodes);
                r
            },
            Statement::ForLoop(id, start_expr, end_expr, ast) => {
                label = "For loop";
                let id_node = id.convert();
                let mut start_expr_nodes = start_expr.convert();
                let mut end_expr_nodes = end_expr.convert();
                let mut ast_nodes = ast.convert();

                immediate_children.push(id_node.clone());
                immediate_children.push(start_expr_nodes.first().unwrap().clone());
                immediate_children.push(end_expr_nodes.first().unwrap().clone());
                immediate_children.push(ast_nodes.first().unwrap().clone());

                let mut r = vec![id_node];
                r.append(&mut start_expr_nodes);
                r.append(&mut end_expr_nodes);
                r.append(&mut ast_nodes);
                r
            },
            Statement::Read(id) => {
                label = "Read";
                let id_node = id.convert();
                immediate_children.push(id_node.clone());
                vec![id_node]
            },
            Statement::Print(expr) => {
                label = "Print";
                let expr_nodes = expr.convert();
                immediate_children.push(expr_nodes.first().unwrap().clone());
                expr_nodes
            },
            Statement::Assert(expr) => {
                label = "Assert";
                let expr_nodes = expr.convert();
                immediate_children.push(expr_nodes.first().unwrap().clone());
                expr_nodes
            },
        };

        let children = immediate_children.iter().map(|c| c.id).collect::<Vec<_>>();

        let node = GraphNode {
            id,
            label: label.to_owned(),
            children
        };
        let mut ret = vec![node];
        ret.append(&mut all_nodes);
        ret
    }
}

impl DotGraphNode<GraphNode> for Id {
    fn convert(&self) -> GraphNode {
        let id = get_tree_ptr();
        GraphNode {
            id,
            label: self.0.clone(),
            children: Vec::new(),
        }
    }
}

impl DotGraphNode<GraphNode> for Type {
    fn convert(&self) -> GraphNode {
        let id = get_tree_ptr();
        let label = format!("{:?}", self);
        GraphNode {
            id,
            label,
            children: Vec::new(),
        }
    }
}

#[test]
fn visualize_var_definition() {
    use crate::parser::token::Token;
    use crate::parser::token_type::TokenType;
    use crate::parser::ast_parser::AstParser;
    use crate::ast::Ast;
    use crate::ast_visualizer::DotGraphNode;


    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut ast_parser = AstParser::new(&mut iter);
    let ast = ast_parser.build_ast().unwrap();

    let nodes = ast.convert();
    let out = nodes.iter().flat_map(|e| e.get_dot_output()).collect::<Vec<String>>();
    for s in out {
        println!("{}", s);
    }
    assert_eq!(false, true);
}