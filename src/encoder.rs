#![allow(unused)]

use crate::ast;
use crate::ast::{DataSize, Instruction, Operand, Operands, Register};
use crate::lexer::Span;

#[derive(Debug)]
pub enum EncodeError {
    InvalidOperands(Span),
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidOperands(_) => write!(f, "Invalid operands"),
        }
    }
}

type Encoder = fn(&ast::Instruction) -> Result<Vec<u8>, EncodeError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum OperandSize {
    Unknown,
    Byte,
    Word,
    MaybeWord,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum OperandType {
    None,
    Immediate,        // 0x10
    Accumulator,      // AL/AX
    Counter,          // CL/CX
    Register,         // AX/BX/CL/DH
    RegisterOrMemory, // AX/BX/CL/DH/[data]/[BX + 10]
    Segment,          // DS/CS
    Memory,           // [data]/[BX + 10]
}

struct EncodeData {
    op_code: u8,
    operation: ast::Operation,
    destination: OperandType,
    destination_size: OperandSize,
    source: OperandType,
    source_size: OperandSize,
    encoder: Encoder,
}

macro_rules! ed {
    ($op_code:literal, $operation:ident, $destination:ident, $destination_size:ident, $source:ident, $source_size:ident, $encoder:ident) => {{
        EncodeData {
            op_code: $op_code,
            operation: ast::Operation::$operation,
            destination: OperandType::$destination,
            destination_size: OperandSize::$destination_size,
            source: OperandType::$source,
            source_size: OperandSize::$source_size,
            encoder: $encoder,
        }
    }};
}

fn encode_register_or_memory_and_register<'a>(
    instruction: &ast::Instruction<'a>,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

#[rustfmt::skip]
const TABLE: &[EncodeData] = &[
    // 04 ib         ADD AL, imm8          Add imm8 to AL
    ed!(0x04, ADD, Accumulator, Byte, Immediate, Byte, encode_register_or_memory_and_register),

    // 05 iw         ADD AX, imm16         Add imm16 to AX
    ed!(0x05, ADD, Accumulator, Word, Immediate, Word, encode_register_or_memory_and_register),

    // 00 /r         ADD r/m8, r8          Add r8 to r/m8
    ed!(0x00, ADD, RegisterOrMemory, Byte, Register, Byte, encode_register_or_memory_and_register),
    
    // 01 /r         ADD r/m16, r16        Add r16 to r/m16
    ed!(0x01, ADD, RegisterOrMemory, Word, Register, Word, encode_register_or_memory_and_register),

    // 02 /r         ADD r8, r/m8          Add r/m8 to r8
    ed!(0x02, ADD, Register, Byte, RegisterOrMemory, Byte, encode_register_or_memory_and_register),

    // 03 /r         ADD r16, r/m16        Add r/m16 to r16
    ed!(0x03, ADD, Register, Word, RegisterOrMemory, Word, encode_register_or_memory_and_register),

    // 80 /0 ib      ADD r/m8, imm8        Add imm8 to r/m8
    // 81 /0 iw      ADD r/m16, imm16      Add imm16 to r/m16
    // 83 /0 ib      ADD r/m16, imm8       Add sign-extended imm8 to r/m16
];

impl<'a> PartialEq<EncodeData> for ast::Instruction<'a> {
    fn eq(&self, other: &EncodeData) -> bool {
        if self.operation != other.operation {
            return false;
        }

        match &self.operands {
            Operands::None(_) => return true,

            Operands::Destination(_, destination) => {
                if destination.operand_type() == other.destination
                    && destination.operand_size() == other.destination_size
                {
                    return true;
                }
            }

            Operands::DestinationAndSource(_, destination, source) => {
                if destination.operand_type() == other.destination
                    && destination.operand_size() == other.destination_size
                    && source.operand_type() == other.source
                    && source.operand_size() == other.source_size
                {
                    return true;
                }
            }
        }

        false
    }
}

fn find_encode_data<'a>(instruction: &ast::Instruction<'a>) -> Option<&'static EncodeData> {
    for entry in TABLE {
        if instruction == entry {
            return Some(entry);
        }
    }

    None
}

impl<'a> ast::Operand<'a> {
    fn operand_type(&self) -> OperandType {
        match self {
            ast::Operand::Immediate(_) => OperandType::Immediate,
            ast::Operand::Address(_, _, _) => OperandType::Memory,
            ast::Operand::Register(register) => match register {
                ast::Register::Byte(register) => match register {
                    ast::ByteRegister::AL => OperandType::Accumulator,
                    ast::ByteRegister::CL => OperandType::Counter,
                    _ => OperandType::Register,
                },
                ast::Register::Word(register) => match register {
                    ast::WordRegister::AX => OperandType::Accumulator,
                    ast::WordRegister::CX => OperandType::Counter,
                    _ => OperandType::Register,
                },
            },
            ast::Operand::Segment(_) => OperandType::Segment,
        }
    }

    fn operand_size(&self) -> OperandSize {
        match self {
            ast::Operand::Immediate(_) => OperandSize::Unknown,

            ast::Operand::Address(Some(data_size), _, _) => match data_size {
                DataSize::Byte => OperandSize::Byte,
                DataSize::Word => OperandSize::Word,
            },

            ast::Operand::Register(register) => match register {
                Register::Byte(_) => OperandSize::Byte,
                Register::Word(_) => OperandSize::Word,
            },

            ast::Operand::Segment(_) => OperandSize::Word,

            _ => OperandSize::Unknown,
        }
    }
}

pub fn encode<'a>(instruction: &ast::Instruction<'a>) -> Result<Vec<u8>, EncodeError> {
    let encode_data = match find_encode_data(instruction) {
        Some(encode_data) => encode_data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    dbg!(
        encode_data.operation,
        encode_data.destination,
        encode_data.destination_size,
        encode_data.source,
        encode_data.source_size
    );

    (encode_data.encoder)(instruction)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn immediate<'a>(value: i32) -> ast::Operand<'a> {
        ast::Operand::Immediate(Box::new(ast::Expression::Term(ast::Value::Constant(value))))
    }

    fn byte_register<'a>(register: ast::ByteRegister) -> ast::Operand<'a> {
        ast::Operand::Register(ast::Register::Byte(register))
    }

    fn word_register<'a>(register: ast::WordRegister) -> ast::Operand<'a> {
        ast::Operand::Register(ast::Register::Word(register))
    }

    fn memory<'a>() -> ast::Operand<'a> {
        ast::Operand::Address(
            None,
            Box::new(ast::Expression::Term(ast::Value::Constant(0))),
            None,
        )
    }

    #[test]
    fn operand_to_operand_type() {
        assert_eq!(OperandType::Immediate, immediate(10).operand_type());

        assert_eq!(
            OperandType::Accumulator,
            word_register(ast::WordRegister::AX).operand_type()
        );
        assert_eq!(
            OperandType::Accumulator,
            byte_register(ast::ByteRegister::AL).operand_type()
        );
        assert_eq!(
            OperandType::Counter,
            word_register(ast::WordRegister::CX).operand_type()
        );
        assert_eq!(
            OperandType::Counter,
            byte_register(ast::ByteRegister::CL).operand_type()
        );
        assert_eq!(
            OperandType::Register,
            word_register(ast::WordRegister::SP).operand_type()
        );
        assert_eq!(
            OperandType::Register,
            byte_register(ast::ByteRegister::DH).operand_type()
        );

        assert_eq!(OperandType::Memory, memory().operand_type());
    }
}
