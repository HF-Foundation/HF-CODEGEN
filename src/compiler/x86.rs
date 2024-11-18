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
                    // add byte ptr[r8], n
                    .add(byte_ptr(r8), n as u32)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::Subtract(n) => {
                self.code_asm
                    // sub byte ptr[r8], n
                    .sub(byte_ptr(r8), n as u32)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::MoveRight(n) => {
                if n > 0x7FFFFFFF {
                    return Err(CompilerError {
                        kind: super::CompilerErrorKind::MoveTooLarge(n as u32),
                        span: Some(ir_node.span),
                    });
                }
                self.code_asm
                    // lea r8, [r8 + n]
                    .lea(r8, dword_ptr(r8 + n as u32))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            IrOp::MoveLeft(n) => {
                if n > 0x7FFFFFFF {
                    return Err(CompilerError {
                        kind: super::CompilerErrorKind::MoveTooLarge(n as u32),
                        span: Some(ir_node.span),
                    });
                }
                self.code_asm
                    // lea r8, [r8 - n]
                    .lea(r8, dword_ptr(r8 - n as u32))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            // push byte ptr[r8]
            //
            // doesn't really exist, but we can do:
            //
            // lea  rsp,       [rsp-1]
            // mov  al,        byte[r8]
            // mov  byte[rsp], al
            IrOp::StackPush => {
                self.code_asm
                    .lea(rsp, dword_ptr(rsp - 1))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm
                    .mov(al, byte_ptr(r8))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm
                    .mov(byte_ptr(rsp), al)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
            }
            // pop byte ptr[r8]
            //
            // doesn't really exist, but we can do:
            //
            // mov  al,        byte[rsp]
            // mov  byte[r8],  al
            // lea  rsp,       [rsp+1]
            IrOp::StackPop => {
                self.code_asm
                    .mov(al, byte_ptr(rsp))
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm
                    .mov(byte_ptr(r8), al)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm
                    .lea(rsp, dword_ptr(rsp + 1))
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
    fn test_translate_ir_node_add_with_span() {
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
    fn test_translate_sub() {
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
            // lea r8, [r8 + 1]
            vec![0x4d, 0x8d, 0x40, 0x01]
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
            // lea r8, [r8 - 1]
            vec![0x4d, 0x8d, 0x40, 0xff]
        );
    }

    #[test]
    fn test_translate_ir_node_move_right_32_bit_operand() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::MoveRight(0x7fff_ffff),
            span: Span {
                location: (0, 0),
                length: 0x7fff_ffff,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // lea r8, [r8 + 0x7FFFFFFF]
            vec![0x4d, 0x8d, 0x80, 0xff, 0xff, 0xff, 0x7f]
        );
    }

    #[test]
    fn test_translate_push() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::StackPush,
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0x48, 0x8d, 0x64, 0x24, 0xff, // lea rsp, [rsp-1]
                0x41, 0x8a, 0x00,             // mov al, byte ptr [r8]
                0x88, 0x04, 0x24,             // mov byte ptr [rsp], al
            ]
        );
    }

    #[test]
    fn test_translate_pop() {
        let mut compiler = Compiler::new(64);
        let ir_node = IrNode {
            node: IrOp::StackPop,
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(ir_node)
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0x8a, 0x04, 0x24,             // mov al, byte ptr [rsp]
                0x41, 0x88, 0x00,             // mov byte ptr [r8], al
                0x48, 0x8d, 0x64, 0x24, 0x01, // lea rsp, [rsp+1]
            ]
        );
    }
}
