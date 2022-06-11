#![allow(unused)]

use crate::ast;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum OperandSize {
    Byte,
    Word,
    MaybeWord,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum OperandType {
    None,
    Immediate,   // 0x10
    Accumulator, // AL/AX
    Counter,     // CL/CX
    Register,    // AX/BX/CL/DH
    Segment,     // DS/CS
    Memory,      // [data]/[BX + 10]
}

struct EncodeData {
    op_code: u8,
    destination: OperandType,
    source: OperandType,
}

impl EncodeData {
    const fn new(op_code: u8, destination: OperandType, source: OperandType) -> Self {
        Self {
            op_code,
            destination,
            source,
        }
    }
}

const TABLE: &[EncodeData] = &[
    EncodeData::new(0x00, OperandType::Register, OperandType::Register),
    EncodeData::new(0x01, OperandType::Register, OperandType::Immediate),
];

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
            ast::Operand::Immediate(_) => todo!(),
            ast::Operand::Address(_, _, _) => todo!(),
            ast::Operand::Register(_) => todo!(),
            ast::Operand::Segment(_) => todo!(),
        }
    }
}

fn encode<'a>(instruction: &ast::Instruction<'a>) {
    for entry in TABLE {
        let operands = match &instruction.operands {
            ast::Operands::None(_) => (OperandType::None, OperandType::None),
            ast::Operands::Destination(_, destination) => {
                (destination.operand_type(), OperandType::None)
            }
            ast::Operands::DestinationAndSource(_, destination, source) => {
                (destination.operand_type(), source.operand_type())
            }
        };
    }
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
