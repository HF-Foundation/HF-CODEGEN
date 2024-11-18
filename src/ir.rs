// TODO: Implement optimisation passes

use alloc::string::String;
use alloc::vec::Vec;

use hf_parser_rust::ast::{AstNode, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Span {
    pub location: (usize, usize),
    pub length: usize, // We only need the length as we can calculate the rest
}

impl Span {
    pub fn from_location(location: (usize, usize)) -> Self {
        Self {
            location,
            length: 1,
        }
    }

    pub fn extend(&self, length: usize) -> Self {
        Self {
            location: self.location,
            length: self.length + length,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrOp {
    Add(usize),
    Subtract(usize),
    MoveRight(usize),
    MoveLeft(usize),
    StackPush,
    StackPop,
    Function(String, Vec<IrNode>),
    FunctionCall(String),
    Asm(String),
    Condition(Vec<IrNode>),
}

impl IrOp {
    fn equals_extend(&mut self, op: &SyntaxNode) -> bool {
        match self {
            Self::Add(n) => {
                if op == &SyntaxNode::Add {
                    *n += 1;
                    true
                } else {
                    false
                }
            }
            Self::Subtract(n) => {
                if op == &SyntaxNode::Subtract {
                    *n += 1;
                    true
                } else {
                    false
                }
            }
            Self::MoveRight(n) => {
                if op == &SyntaxNode::MoveRight {
                    *n += 1;
                    true
                } else {
                    false
                }
            }
            Self::MoveLeft(n) => {
                if op == &SyntaxNode::MoveLeft {
                    *n += 1;
                    true
                } else {
                    false
                }
            }
            _ => false, // These cannot be extended regardless
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct IrNode {
    pub node: IrOp,
    pub span: Span,
}

impl core::fmt::Debug for IrNode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if f.alternate()
            && matches!(self.node, IrOp::Function(_, _))
            || matches!(self.node, IrOp::Condition(_))
        {
            write!(
                f,
                "{:#?} \x1b[90m({}:{})\x1b[0m",
                self.node,
                self.span.location.0 + 1,
                self.span.location.1 + 1
            )
        } else {
            write!(
                f,
                "{:?} \x1b[90m({}:{})\x1b[0m",
                self.node,
                self.span.location.0 + 1,
                self.span.location.1 + 1
            )
        }
    }
}

pub struct IrBuilder {
    pub ir_nodes: Vec<IrNode>,
    pub current: Option<IrNode>,
}

impl IrBuilder {
    fn new() -> Self {
        Self {
            ir_nodes: Vec::new(),
            current: None,
        }
    }

    fn extend(&mut self, ast: AstNode) {
        if let Some(mut current) = self.current.take() {
            if current.node.equals_extend(&ast.node) {
                current.span = current.span.extend(1);
                self.current = Some(current);
                return;
            }

            self.ir_nodes.push(current);
        }

        self.current = Some(IrNode {
            node: match ast.node {
                SyntaxNode::Add => IrOp::Add(1),
                SyntaxNode::Subtract => IrOp::Subtract(1),
                SyntaxNode::MoveRight => IrOp::MoveRight(1),
                SyntaxNode::MoveLeft => IrOp::MoveLeft(1),
                SyntaxNode::StackPush => IrOp::StackPush,
                SyntaxNode::StackPop => IrOp::StackPop,
                SyntaxNode::Function(name, args) => IrOp::Function(name, from_ast(args)),
                SyntaxNode::FuncCall(name) => IrOp::FunctionCall(name),
                SyntaxNode::Asm(code) => IrOp::Asm(code),
                SyntaxNode::Condition(conditions) => IrOp::Condition(from_ast(conditions)),
            },
            span: Span::from_location(ast.location),
        });
    }

    fn build(mut self) -> Vec<IrNode> {
        if let Some(current) = self.current.take() {
            self.ir_nodes.push(current);
        }
        self.ir_nodes
    }
}

pub fn from_ast(ast: Vec<AstNode>) -> Vec<IrNode> {
    let mut ir = IrBuilder::new();

    for node in ast {
        ir.extend(node);
    }

    ir.build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;
    use hf_parser_rust::ast::{AstNode, SyntaxNode};

    #[test]
    fn test_from_ast_with_empty_vec() {
        let ast = Vec::new();
        let ir = from_ast(ast);
        assert!(ir.is_empty());
    }

    #[test]
    fn test_from_ast_with_single_add_node() {
        let ast = vec![AstNode {
            node: SyntaxNode::Add,
            location: (0, 0),
        }];
        let ir = from_ast(ast);
        assert_eq!(
            ir,
            vec![IrNode {
                node: IrOp::Add(1),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            }]
        );
    }

    #[test]
    fn test_from_ast_with_multiple_add_nodes() {
        let ast = vec![
            AstNode {
                node: SyntaxNode::Add,
                location: (0, 0),
            },
            AstNode {
                node: SyntaxNode::Add,
                location: (0, 1),
            },
        ];
        let ir = from_ast(ast);
        assert_eq!(
            ir,
            vec![IrNode {
                node: IrOp::Add(2),
                span: Span {
                    location: (0, 0),
                    length: 2,
                },
            }]
        );
    }

    #[test]
    fn test_from_ast_with_different_nodes() {
        let ast = vec![
            AstNode {
                node: SyntaxNode::Add,
                location: (0, 0),
            },
            AstNode {
                node: SyntaxNode::Subtract,
                location: (0, 1),
            },
        ];
        let ir = from_ast(ast);
        assert_eq!(
            ir,
            vec![
                IrNode {
                    node: IrOp::Add(1),
                    span: Span {
                        location: (0, 0),
                        length: 1,
                    },
                },
                IrNode {
                    node: IrOp::Subtract(1),
                    span: Span {
                        location: (0, 1),
                        length: 1,
                    },
                },
            ]
        );
    }

    #[test]
    fn test_from_ast_with_function_node() {
        let ast = vec![AstNode {
            node: SyntaxNode::Function("test_func".to_string(), vec![]),
            location: (0, 0),
        }];
        let ir = from_ast(ast);
        assert_eq!(
            ir,
            vec![IrNode {
                node: IrOp::Function("test_func".to_string(), vec![]),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            }]
        );
    }
}