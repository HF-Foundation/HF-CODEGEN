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
    #[error("move left/right too large, can at most move 0x7FFFFFFF bytes at a time: {0:x}")]
    MoveTooLarge(u32),
    #[error("function not found: '{0}'")]
    FunctionNotFound(String),
}

pub(crate) trait CompilerTrait {
    fn compile_to_bytecode(&mut self, ast: Vec<IrNode>) -> Result<Vec<u8>, CompilerError>;
}

pub struct HfCompiler {
    compiler: Box<dyn CompilerTrait>,
}

impl HfCompiler {
    pub fn new(target: Target, compiler_settings: CompilerSettings) -> Self {
        let compiler = match target.arch {
            Arch::X86 => Box::new(x86::Compiler::new(32, compiler_settings, target.calling_convention)),
            Arch::X86_64 => Box::new(x86::Compiler::new(64, compiler_settings, target.calling_convention)),
            _ => unimplemented!(),
        };

        Self { compiler }
    }

    pub fn compile_to_bytecode(&mut self, ast: Vec<IrNode>) -> Result<Vec<u8>, CompilerError> {
        self.compiler.compile_to_bytecode(ast)
    }
}

pub struct CompilerSettings {
    pub optimization_level: u8,
    pub base_address: u64,
}

impl Default for CompilerSettings {
    fn default() -> Self {
        Self {
            optimization_level: 0,
            base_address: 0xffff,
        }
    }
}
