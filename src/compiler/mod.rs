use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use thiserror_no_std::Error;

use crate::ir::IrNode;
use crate::target::{Arch, Target};

mod x86;

#[derive(Debug, Error)]
pub struct CompilerError {
    pub kind: CompilerErrorKind,
    pub span: Option<crate::ir::Span>,
}

#[derive(Debug, Error)]
pub enum CompilerErrorKind {
    #[error("unknown error: {0}")]
    Unknown(String),
    #[error("assembler error: {0}")]
    AssemblerError(String),
}

pub(crate) trait CompilerTrait {
    fn compile_to_bytecode(&mut self, ast: Vec<IrNode>) -> Result<Vec<u8>, CompilerError>;
}

pub struct HfCompiler {
    compiler: Box<dyn CompilerTrait>,
}

impl HfCompiler {
    pub fn from_target(target: Target) -> Self {
        let compiler = match target.arch {
            Arch::X86 => Box::new(x86::Compiler::new(32)),
            Arch::X86_64 => Box::new(x86::Compiler::new(64)),
            _ => unimplemented!(),
        };

        Self { compiler }
    }

    pub fn compile_to_bytecode(&mut self, ast: Vec<IrNode>) -> Result<Vec<u8>, CompilerError> {
        self.compiler.compile_to_bytecode(ast)
    }
}
