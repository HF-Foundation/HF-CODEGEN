use alloc::string::ToString;
use alloc::vec::Vec;

use iced_x86::code_asm::*;

use super::CompilerError;
use crate::ir::{IrNode, IrOp};

pub struct Compiler {
    code_asm: CodeAssembler,
}

impl Compiler {
    pub fn new(bitness: u32) -> Self {
        Self {
            code_asm: CodeAssembler::new(bitness).unwrap(),
        }
    }

    /// Translates an IR node to x86 assembly and pushes it to the code assembler.
    ///
    /// # Registers
    ///
    /// R8: address of the current cell
    ///     access it via `byte_ptr(r8)` aka `byte ptr[r8]`
    fn translate_ir_node(&mut self, ir_node: IrNode) -> Result<(), CompilerError> {
        match ir_node.node {
            IrOp::Add(n) => {
                self.code_asm
                    .add(byte_ptr(r8), n as u32)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::Subtract(n) => {
                self.code_asm
                    .sub(byte_ptr(r8), n as u32)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::MoveRight(n) => {
                self.code_asm
                    .lea(r8, byte_ptr(r8 + n as u32))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::MoveLeft(n) => {
                self.code_asm
                    .lea(r8, byte_ptr(r8 - n as u32))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}

impl super::CompilerTrait for Compiler {
    fn compile_to_bytecode(&mut self, ast: Vec<IrNode>) -> Result<Vec<u8>, CompilerError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::Span;

    #[test]
    fn test_translate_ir_node_add() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::Add(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // add byte ptr[r8b], 1
            vec![0x41, 0x80, 0x00, 0x01]
        );
    }

    #[test]
    fn test_translate_ir_node_add_with_span_64() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::Add(5),
            span: Span {
                location: (0, 0),
                length: 5,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // add byte ptr[r8], 5
            vec![0x41, 0x80, 0x00, 0x05]
        );
    }

    #[test]
    fn test_translate_sub_64() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::Subtract(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // sub byte ptr[r8], 1
            vec![0x41, 0x80, 0x28, 0x01]
        );
    }

    #[test]
    fn test_translate_multiple_nodes() {
        let mut compiler = Compiler::new(64);
        let ir_nodes = vec![
            IrNode {
                node: IrOp::Add(1),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            },
            IrNode {
                node: IrOp::Subtract(2),
                span: Span {
                    location: (0, 1),
                    length: 2,
                },
            },
        ];
        for ir_node in ir_nodes {
            compiler
                .translate_ir_node(ir_node)
                .expect("Failed to translate IR node");
        }
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0x41, 0x80, 0x00, 0x01, // add byte ptr[r8b], 1
                0x41, 0x80, 0x28, 0x02, // sub byte ptr[r8b], 2
            ]
        );
    }

    #[test]
    fn test_translate_ir_node_move_right() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::MoveRight(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // add r8, 1
            vec![0x49, 0x83, 0xC0, 0x01]
        );
    }

    #[test]
    fn test_translate_ir_node_move_left() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::MoveLeft(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // sub r8, 1
            vec![0x49, 0x83, 0xE8, 0x01]
        );
    }
}
