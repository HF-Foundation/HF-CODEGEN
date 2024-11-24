#![no_std]
#[macro_use]
extern crate alloc;

pub mod compiler;
pub mod ir;
pub mod target;
pub mod scope;

pub use compiler::{CompilerError, CompilerErrorKind};
