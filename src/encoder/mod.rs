mod instructions;

use crate::ast;
use crate::ast::{ByteRegister, Operand, Operands, Register, WordRegister};
use crate::lexer::Span;

pub use instructions::{str_to_operation, Operation};

#[derive(Debug)]
pub enum EncodeError {
    InvalidOperands(Span),
}

impl EncodeError {
    pub fn span(&self) -> &Span {
        match self {
            EncodeError::InvalidOperands(span) => span,
        }
    }
}

type Encoder = fn(&ast::Instruction, &InstructionData) -> Result<Vec<u8>, EncodeError>;

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidOperands(_) => write!(f, "Invalid operands"),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Eq, PartialEq)]
pub enum OperandType {
    none,
    imm_8,
    imm_16,
    al,
    ax,
    reg_8,
    reg_16,
    reg_mem_8,
    reg_mem_16,
    disp_8,
    disp_16,
}

pub struct InstructionData {
    pub operation: Operation,
    pub op_code: u8,
    pub destination: OperandType,
    pub source: OperandType,
    pub encoder: Encoder,
}

fn encode_al_and_imm_8(
    instruction: &ast::Instruction,
    data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    match &instruction.operands {
        Operands::DestinationAndSource(
            _,
            Operand::Register(Register::Byte(ByteRegister::AL)),
            Operand::Immediate(expr),
        ) => {
            let value = expr.value();
            let [lo, _, _, _] = value.to_le_bytes();
            Ok(vec![data.op_code, lo])
        }
        _ => Err(EncodeError::InvalidOperands(0..0)),
    }
}

fn encode_ax_and_imm_16(
    instruction: &ast::Instruction,
    data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    match &instruction.operands {
        Operands::DestinationAndSource(
            _,
            Operand::Register(Register::Word(WordRegister::AX)),
            Operand::Immediate(expr),
        ) => {
            let value = expr.value();
            let [lo, hi, _, _] = value.to_le_bytes();
            Ok(vec![data.op_code, lo, hi])
        }
        _ => Err(EncodeError::InvalidOperands(0..0)),
    }
}

fn encode_none_and_none(
    _instruction: &ast::Instruction,
    data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    Ok(vec![data.op_code])
}

fn encode_reg_mem_8_and_reg_8(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn encode_reg_mem_16_and_reg_16(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn encode_reg_8_and_reg_mem_8(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn encode_reg_16_and_reg_mem_16(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn encode_disp_8_and_none(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn encode_disp_16_and_none(
    _instruction: &ast::Instruction,
    _data: &InstructionData,
) -> Result<Vec<u8>, EncodeError> {
    todo!()
}

fn operand_to_type(operand: &ast::Operand) -> OperandType {
    match operand {
        Operand::Immediate(expr) => {
            if expr.value() < 256 {
                OperandType::imm_8
            } else {
                OperandType::imm_16
            }
        }
        Operand::Address(_, _, _) => todo!(),
        Operand::Register(register) => match register {
            Register::Byte(byte_register) => match byte_register {
                ast::ByteRegister::AL => OperandType::al,
                _ => OperandType::reg_8,
            },
            Register::Word(word_register) => match word_register {
                ast::WordRegister::AX => OperandType::ax,
                _ => OperandType::reg_16,
            },
        },
        Operand::Segment(_) => todo!(),
    }
}

fn coerce_operand_types(
    destination: OperandType,
    source: OperandType,
) -> (OperandType, OperandType) {
    use OperandType::*;

    if matches!(destination, imm_16 | ax | reg_16 | reg_mem_16 if source == imm_8) {
        return (destination, imm_16);
    }

    (destination, source)
}

fn find_instruction_data_for(instruction: &ast::Instruction) -> Option<&'static InstructionData> {
    let (destination, source) = match &instruction.operands {
        Operands::None(_) => (OperandType::none, OperandType::none),
        Operands::Destination(_, destination) => (operand_to_type(destination), OperandType::none),
        Operands::DestinationAndSource(_, destination, source) => {
            (operand_to_type(destination), operand_to_type(source))
        }
    };

    let (destination, source) = coerce_operand_types(destination, source);

    dbg!("searching_for", &destination, &source);

    for data in instructions::DATA {
        if data.operation == instruction.operation
            && data.destination == destination
            && data.source == source
        {
            return Some(data);
        }
    }

    None
}

pub fn size_in_bytes<'a>(instruction: &ast::Instruction<'a>) -> Result<u32, EncodeError> {
    let data = match find_instruction_data_for(instruction) {
        Some(data) => data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    Ok(match (&data.destination, &data.source) {
        (OperandType::none, OperandType::none) => 1,
        (OperandType::al, OperandType::imm_8) => 2,
        (OperandType::ax, OperandType::imm_16) => 3,
        _ => todo!("{:?}, {:?}", &data.destination, &data.source),
    })
}

pub fn encode<'a>(instruction: &ast::Instruction<'a>) -> Result<Vec<u8>, EncodeError> {
    let data = match find_instruction_data_for(instruction) {
        Some(data) => data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    dbg!(
        "found data",
        &data.operation,
        &data.op_code,
        &data.destination,
        &data.source
    );

    let encoder = data.encoder;
    encoder(instruction, data)
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
