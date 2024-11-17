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
