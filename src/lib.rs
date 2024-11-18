#![no_std]
#[macro_use]
extern crate alloc;

use alloc::vec::Vec;

use hf_parser_rust::ast::AstNode;

pub mod compiler;
pub mod ir;
pub mod target;

pub use compiler::{CompilerError, CompilerErrorKind};
use target::*;

pub fn compile_to_bytecode(target: Target, ast: Vec<AstNode>) -> Result<Vec<u8>, CompilerError> {
    let mut compiler = compiler::HfCompiler::from_target(target);

    let ir = ir::from_ast(ast);

    compiler.compile_to_bytecode(ir)
}

#[cfg(test)]
mod tests {
    use super::*;
}
