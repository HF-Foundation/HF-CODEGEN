use alloc::vec::Vec;
use assert_hex::assert_eq_hex;
use hf_parser_rust::{ast, token};

use super::{x86::*, CompilerSettings, CompilerTrait};
use crate::{ir::IrNode, target::Target};

fn get_compiler() -> Compiler {
    Compiler::new(
        64,
        CompilerSettings::default(),
        Target::native().calling_convention,
    )
}

fn compile_to_ir(source: &str) -> Vec<IrNode> {
    let tokens = token::tokenize(source).expect("source code did not tokenize");
    let ast = ast::build_ast(tokens).expect("source code did not build into an AST");
    crate::ir::from_ast(ast)
}

fn compile_to_bytecode(source: &str) -> Vec<u8> {
    let ir = compile_to_ir(source);
    let mut compiler = get_compiler();
    compiler
        .compile_to_bytecode(ir)
        .expect("failed to compile to bytecode")
}

#[test]
fn test_emit_nothing() {
    assert_eq_hex!(compile_to_bytecode(""), vec![]);
}

#[test]
fn test_emit_add() {
    assert_eq_hex!(compile_to_bytecode("+"), vec![0x41, 0x80, 0x00, 0x01]);
    assert_eq_hex!(compile_to_bytecode("++"), vec![0x41, 0x80, 0x00, 0x02]);
    assert_eq_hex!(compile_to_bytecode("++++++"), vec![0x41, 0x80, 0x00, 0x06]);
}

#[test]
fn test_emit_sub() {
    assert_eq_hex!(compile_to_bytecode("-"), vec![0x41, 0x80, 0x28, 0x01]);
    assert_eq_hex!(compile_to_bytecode("--"), vec![0x41, 0x80, 0x28, 0x02]);
    assert_eq_hex!(compile_to_bytecode("------"), vec![0x41, 0x80, 0x28, 0x06]);
}

#[test]
fn test_emit_function_call() {
    assert_eq_hex!(
        compile_to_bytecode(":test{}@test;"),
        vec![0xc3, 0xe8, 0xfa, 0xff, 0xff, 0xff,]
    )
}

#[test]
fn test_emit_add_sub_mix() {
    assert_eq_hex!(
        compile_to_bytecode("+-++--++"),
        vec![
            0x41, 0x80, 0x0, 0x1, 0x41, 0x80, 0x28, 0x1, 0x41, 0x80, 0x0, 0x2, 0x41, 0x80, 0x28,
            0x2, 0x41, 0x80, 0x0, 0x2,
        ]
    )
}

#[test]
fn test_move_right() {
    assert_eq_hex!(compile_to_bytecode(">"), vec![0x4d, 0x8d, 0x40, 0x1,]);
    assert_eq_hex!(compile_to_bytecode(">>>>"), vec![0x4d, 0x8d, 0x40, 0x4,]);
}

#[test]
fn test_move_left() {
    assert_eq_hex!(compile_to_bytecode("<"), vec![0x4d, 0x8d, 0x40, 0xff,]);
    assert_eq_hex!(compile_to_bytecode("<<<<"), vec![0x4d, 0x8d, 0x40, 0xfc,]);
}

#[test]
fn test_arith_limits() {
    assert_eq_hex!(
        compile_to_bytecode(&"+".repeat(255)),
        vec![0x41, 0x80, 0x0, 0xff,],
    );
    assert_eq_hex!(
        compile_to_bytecode(&"-".repeat(255)),
        vec![0x41, 0x80, 0x28, 0xff,],
    );
}

#[test]
fn test_emit_loop() {
    assert_eq_hex!(
        compile_to_bytecode("[+[-]]"),
        vec![
            0x41, 0x80, 0x38, 0x0, 0x74, 0x12, 0x41, 0x80, 0x0, 0x1, 0x41, 0x80, 0x38, 0x0, 0x74,
            0x6, 0x41, 0x80, 0x28, 0x1, 0xeb, 0xf4, 0xeb, 0xe8,
        ]
    );
}

#[test]
fn test_emit_condition_empty() {
    assert_eq_hex!(
        compile_to_bytecode("[]"),
        vec![0x41, 0x80, 0x38, 0x0, 0x74, 0x2, 0xeb, 0xf8,]
    );
}

#[test]
fn test_emit_push() {
    assert_eq_hex!(
        compile_to_bytecode("."),
        vec![0x4d, 0x8d, 0x49, 0x1, 0x41, 0x8a, 0x0, 0x41, 0x88, 0x1,],
    );
    assert_eq_hex!(
        compile_to_bytecode(".."),
        vec![
            0x4d, 0x8d, 0x49, 0x1, 0x41, 0x8a, 0x0, 0x41, 0x88, 0x1, 0x4d, 0x8d, 0x49, 0x1, 0x41,
            0x8a, 0x0, 0x41, 0x88, 0x1,
        ],
    );
}

#[test]
fn test_emit_pop() {
    assert_eq_hex!(
        compile_to_bytecode(","),
        vec![0x41, 0x8a, 0x1, 0x41, 0x88, 0x0, 0x4d, 0x8d, 0x49, 0xff],
    );
    assert_eq_hex!(
        compile_to_bytecode(",,"),
        vec![
            0x41, 0x8a, 0x1, 0x41, 0x88, 0x0, 0x4d, 0x8d, 0x49, 0xff, 0x41, 0x8a, 0x1, 0x41, 0x88,
            0x0, 0x4d, 0x8d, 0x49, 0xff,
        ],
    );
}

#[test]
fn test_fn_with_body() {
    assert_eq_hex!(
        compile_to_bytecode(":test{++++[-]}"),
        vec![
            0x41, 0x80, 0x0, 0x4, 0x41, 0x80, 0x38, 0x0, 0x74, 0x6, 0x41, 0x80, 0x28, 0x1, 0xeb,
            0xf4, 0xc3,
        ]
    )
}

#[test]
fn test_fns_with_same_name() {
    assert_eq_hex!(
        compile_to_bytecode(":test{}@test;:test{}@test;"),
        vec![0xc3, 0xe8, 0xfa, 0xff, 0xff, 0xff, 0xc3, 0xe8, 0xfa, 0xff, 0xff, 0xff,],
    )
}

#[test]
fn test_long_add() {
    assert_eq_hex!(
        compile_to_bytecode(&"+".repeat(300)),
        vec![
            0x41, 0x80, 0x0, 0xff, // 255
            0x41, 0x80, 0x0, 0x2d, // 45
        ]
    );
}

#[test]
fn test_long_code() {
    assert_eq_hex!(
        compile_to_bytecode(&format!("{}{}{}", ":test{}", "+".repeat(1024), "@test;")),
        vec![
            0xc3, 0x41, 0x80, 0x0, 0xff, 0x41, 0x80, 0x0, 0xff, 0x41, 0x80, 0x0, 0xff, 0x41, 0x80,
            0x0, 0xff, 0x41, 0x80, 0x0, 0x4, 0xe8, 0xe6, 0xff, 0xff, 0xff,
        ]
    )
}
