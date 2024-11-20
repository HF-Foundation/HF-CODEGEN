use alloc::string::{String, ToString};
use alloc::vec::Vec;

use hashbrown::HashMap;
use iced_x86::code_asm::{CodeLabel, *};

use super::{CompilerError, CompilerSettings};
use crate::ir::{IrNode, IrOp};
use crate::target::CallingConvention;

pub struct Compiler {
    code_asm: CodeAssembler,
    calling_convention: CallingConvention,
    settings: CompilerSettings,
}

impl Compiler {
    pub fn new(
        bitness: u32,
        compiler_settings: CompilerSettings,
        calling_convention: CallingConvention,
    ) -> Self {
        Self {
            code_asm: CodeAssembler::new(bitness).unwrap(),
            calling_convention,
            settings: compiler_settings,
        }
    }

    /// Translates an IR node to x86 assembly and pushes it to the code assembler.
    ///
    /// # Registers
    ///
    /// R8: address of the current cell
    ///     access it via `byte_ptr(r8)` aka `byte ptr[r8]`
    ///
    /// TODO: we might wanna return the hashmap here
    fn translate_ir_node(&mut self, ir_node: Vec<IrNode>) -> Result<(), CompilerError> {
        let mut functions = HashMap::new();
        for node in ir_node {
            self.translate_ir_node_impl(node, &mut functions)?;
        }
        Ok(())
    }

    fn generate_memory_alloc_syscall(&mut self, size: u32) -> Result<(), IcedError> {
        self.code_asm.mov(r9d, 4)?;
        self.code_asm.mov(r8d, 0x00001000 | 0x00002000)?;
        self.code_asm.mov(edx, size)?;
        self.code_asm.xor(ecx, ecx)?;
        // TODO: call VirtualAlloc
        // TODO: `rax` has the result ptr or some error (ignore error)
        Ok(())
    }

    fn translate_ir_node_impl(
        &mut self,
        ir_node: IrNode,
        functions: &mut HashMap<String, CodeLabel>,
    ) -> Result<(), CompilerError> {
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
            // equivalent:
            //
            // while *r8 != 0 {
            //    // code
            // }
            //
            // or in assembly:
            //
            // start_label:
            //    cmp byte ptr[r8], 0
            //    je end_label
            //    ... ; code
            //    jmp start_label
            // end_label:
            //
            IrOp::Condition(cond_ir_nodes) => {
                let mut start_label = self.code_asm.create_label();
                let mut end_label = self.code_asm.create_label();

                // phantom instruction so we have an address
                self.code_asm.zero_bytes().map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;

                self.code_asm
                    .set_label(&mut start_label)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;

                self.code_asm
                    .cmp(byte_ptr(r8), 0)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm.je(end_label).map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;

                let mut scope_functions = functions.clone();
                for cond_ir_node in cond_ir_nodes {
                    self.translate_ir_node_impl(cond_ir_node, &mut scope_functions)?;
                }

                self.code_asm.jmp(start_label).map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;

                self.code_asm
                    .set_label(&mut end_label)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;

                // phantom instruction so we have an address
                self.code_asm.zero_bytes().map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;
            }
            IrOp::Function(name, fn_ir_nodes) => {
                let mut fn_label = self.code_asm.create_label();
                let mut skip_fn_label = self.code_asm.create_label();

                self.code_asm
                    .jmp(skip_fn_label)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm
                    .set_label(&mut fn_label)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                // we dont care if the function already exists, if it does, tough luck,
                // we're overwriting it and we're not going to check if it's the same
                functions.insert(name.clone(), fn_label);
                let mut scope_functions = functions.clone();
                for fn_ir_node in fn_ir_nodes {
                    self.translate_ir_node_impl(fn_ir_node, &mut scope_functions)?;
                }
                self.code_asm.ret().map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;
                self.code_asm
                    .set_label(&mut skip_fn_label)
                    .map_err(|e| CompilerError {
                        kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                        span: Some(ir_node.span),
                    })?;
                self.code_asm.zero_bytes().map_err(|e| CompilerError {
                    kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                    span: Some(ir_node.span),
                })?;
            }
            IrOp::FunctionCall(name) => {
                let fn_label = functions.get(&name).ok_or(CompilerError {
                    kind: super::CompilerErrorKind::FunctionNotFound(name.clone()),
                    span: Some(ir_node.span),
                })?;
                self.code_asm.call(*fn_label).map_err(|e| CompilerError {
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
        /* Doesn't work rn
        self.generate_memory_alloc_syscall(1024)
            .map_err(|e| CompilerError {
                kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                span: None,
            })?;
        */
        self.translate_ir_node(ast)?;
        self.code_asm
            .assemble(self.settings.base_address)
            .map_err(|e| CompilerError {
                kind: super::CompilerErrorKind::AssemblerError(e.to_string()),
                span: None,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir::Span, target::Target};

    fn get_compiler() -> Compiler {
        Compiler::new(
            64,
            CompilerSettings::default(),
            Target::native().calling_convention,
        )
    }

    #[test]
    fn test_translate_ir_node_add() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Add(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // add byte ptr[r8b], 1
            vec![0x41, 0x80, 0x00, 0x01]
        );
    }

    #[test]
    fn test_translate_ir_node_add_with_span() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Add(5),
            span: Span {
                location: (0, 0),
                length: 5,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // add byte ptr[r8], 5
            vec![0x41, 0x80, 0x00, 0x05]
        );
    }

    #[test]
    fn test_function_call_only() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::FunctionCall("test".to_string()),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        let mut functions = HashMap::new();
        functions.insert("test".to_string(), compiler.code_asm.create_label());
        compiler
            .translate_ir_node_impl(ir_node, &mut functions)
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // call test
            vec![0xe8, 0xfb, 0xff, 0xff, 0xff]
        );
    }

    #[test]
    fn test_translate_sub() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Subtract(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // sub byte ptr[r8], 1
            vec![0x41, 0x80, 0x28, 0x01]
        );
    }

    #[test]
    fn test_translate_multiple_nodes() {
        let mut compiler = get_compiler();
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
                .translate_ir_node(vec![ir_node])
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
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::MoveRight(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // lea r8, [r8 + 1]
            vec![0x4d, 0x8d, 0x40, 0x01]
        );
    }

    #[test]
    fn test_translate_ir_node_move_left() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::MoveLeft(1),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // lea r8, [r8 - 1]
            vec![0x4d, 0x8d, 0x40, 0xff]
        );
    }

    #[test]
    fn test_translate_ir_node_move_right_32_bit_operand() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::MoveRight(0x7fff_ffff),
            span: Span {
                location: (0, 0),
                length: 0x7fff_ffff,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            // lea r8, [r8 + 0x7FFFFFFF]
            vec![0x4d, 0x8d, 0x80, 0xff, 0xff, 0xff, 0x7f]
        );
    }

    #[test]
    fn test_translate_push() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::StackPush,
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
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
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::StackPop,
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
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

    #[test]
    fn test_translate_condition_without_body() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Condition(vec![]),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0x41, 0x80, 0x38, 0x00, // cmp byte ptr [r8], 0
                0x74, 0x02,             // je 0x2 (+2)
                0xeb, 0xf8              // jmp 0xf8 (-8)
            ]
        );
    }

    #[test]
    fn test_translate_condition_with_body() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Condition(vec![IrNode {
                node: IrOp::Add(1),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            }]),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0x41, 0x80, 0x38, 0x00, // cmp byte ptr [r8], 0
                0x74, 0x06,             // je 0x6 (+6)
                0x41, 0x80, 0x00, 0x01, // add byte ptr[r8b], 1
                0xeb, 0xf4              // jmp 0xf4 (-12)
            ]
        );
    }

    #[test]
    fn test_translate_function() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Function("hello world".to_string(), vec![]),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                // jmp 0x1 (+1)
                0xeb, 0x01,
                // ret
                0xc3
            ]
        );
    }

    #[test]
    fn test_translate_function_with_body() {
        let mut compiler = get_compiler();
        let ir_node = IrNode {
            node: IrOp::Function(
                "hello world".to_string(),
                vec![IrNode {
                    node: IrOp::Add(1),
                    span: Span {
                        location: (0, 0),
                        length: 1,
                    },
                }],
            ),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        };
        compiler
            .translate_ir_node(vec![ir_node])
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0xeb, 0x05,             // jmp 0x5 (+5)
                0x41, 0x80, 0x00, 0x01, // add byte ptr[r8b], 1
                0xc3, // ret
            ]
        );
    }

    #[test]
    fn test_translate_function_call() {
        let mut compiler = get_compiler();
        let ir_nodes = vec![
            IrNode {
                node: IrOp::Function(
                    "hello_world".to_string(),
                    vec![IrNode {
                        node: IrOp::Add(5),
                        span: Span {
                            location: (1, 4),
                            length: 5,
                        },
                    }],
                ),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            },
            IrNode {
                node: IrOp::FunctionCall("hello_world".to_string()),
                span: Span {
                    location: (2, 0),
                    length: 1,
                },
            },
        ];
        compiler
            .translate_ir_node(ir_nodes)
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0xeb, 0x05,                    // jmp 0x5 (+5)
                0x41, 0x80, 0x00, 0x05,        // add byte ptr[r8b], 5
                0xc3,                          // ret
                0xe8, 0xf6, 0xff, 0xff, 0xff,  // call 0 <hello_world>
            ]
        );
    }

    #[test]
    fn test_translate_multiple_functions_with_the_same_name() {
        let mut compiler = get_compiler();
        let ir_nodes = vec![
            IrNode {
                node: IrOp::Function(
                    "hello_world".to_string(),
                    vec![IrNode {
                        node: IrOp::Add(5),
                        span: Span {
                            location: (1, 4),
                            length: 5,
                        },
                    }],
                ),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            },
            IrNode {
                node: IrOp::Function(
                    "hello_world".to_string(),
                    vec![IrNode {
                        node: IrOp::Add(10),
                        span: Span {
                            location: (1, 4),
                            length: 5,
                        },
                    }],
                ),
                span: Span {
                    location: (0, 0),
                    length: 1,
                },
            },
            IrNode {
                node: IrOp::FunctionCall("hello_world".to_string()),
                span: Span {
                    location: (2, 0),
                    length: 1,
                },
            },
        ];
        compiler
            .translate_ir_node(ir_nodes)
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0xeb, 0x05,                    // jmp 0x5 (+5)
                0x41, 0x80, 0x00, 0x05,        // add byte ptr[r8b], 5
                0xc3,                          // ret
                0xeb, 0x05,                    // jmp 0x5 (+5)
                0x41, 0x80, 0x00, 0x0a,        // add byte ptr[r8b], 10
                0xc3,                          // ret
                0xe8, 0xf6, 0xff, 0xff, 0xff,  // call <hello_world>
            ]
        );
    }

    #[test]
    fn test_translate_nested_empty_functions() {
        let mut compiler = get_compiler();
        let ir_nodes = vec![IrNode {
            node: IrOp::Function(
                "hello_world".to_string(),
                vec![IrNode {
                    node: IrOp::Function(
                        "hello_world".to_string(),
                        vec![IrNode {
                            node: IrOp::Function("hello_world".to_string(), vec![]),
                            span: Span {
                                location: (0, 0),
                                length: 1,
                            },
                        }],
                    ),
                    span: Span {
                        location: (0, 0),
                        length: 1,
                    },
                }],
            ),
            span: Span {
                location: (0, 0),
                length: 1,
            },
        }];
        compiler
            .translate_ir_node(ir_nodes)
            .expect("Failed to translate IR node");
        #[rustfmt::skip]
        assert_eq!(
            compiler.code_asm.assemble(1).unwrap(),
            vec![
                0xeb, 0x07, // jmp 0x7 (+7)
                0xeb, 0x04, // jmp 0x4 (+4)
                0xeb, 0x01, // jmp 0x1 (+1)
                0xc3, // ret
                0xc3, // ret
                0xc3, // ret
            ]
        );
    }
}
