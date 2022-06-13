mod instructions;

use crate::ast;
use crate::lexer::Span;
use ast::Instruction;

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

type Encoder = fn(&Instruction, &InstructionData) -> Result<Vec<u8>, EncodeError>;
type Sizer = fn(&Instruction, &InstructionData) -> Result<u32, EncodeError>;

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

    one,

    imm,
    imm8,
    imm16,

    sbyteword,
    sbyteword16,

    al,
    ax,
    cl,
    cx,
    dx,
    reg8,
    reg16,

    es,
    cs,
    ss,
    ds,
    seg,

    mem,
    mem8,
    mem16,

    rm8,
    rm16,

    disp8,
    disp16,

    seg_off,
}

pub struct InstructionData {
    pub operation: Operation,
    pub op_code: u8,
    pub destination: OperandType,
    pub source: OperandType,
    pub encoder: Encoder,
    pub sizer: Sizer,
}

trait EncoderTrait {
    fn encode(instruction: &Instruction, data: &InstructionData) -> Result<Vec<u8>, EncodeError>;

    fn size(instruction: &Instruction, data: &InstructionData) -> Result<u32, EncodeError>;
}

macro_rules! encoder {
    ($name:ident, $encode:expr, $size:expr) => {
        #[allow(non_camel_case_types)]
        pub struct $name;

        impl EncoderTrait for $name {
            #[inline]
            fn encode(
                instruction: &Instruction,
                data: &InstructionData,
            ) -> Result<Vec<u8>, EncodeError> {
                $encode(instruction, data)
            }

            #[inline]
            fn size(instruction: &Instruction, data: &InstructionData) -> Result<u32, EncodeError> {
                $size(instruction, data)
            }
        }
    };
}

// fn encode(
//     _instruction: &Instruction,
//     _data: &InstructionData,
// ) -> Result<Vec<u8>, EncodeError> $encode
//
// fn size(
//     _instruction: &Instruction,
//     _data: &InstructionData,
// ) -> Result<u32, EncodeError> $size

encoder!(al_and_dx, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(al_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(al_and_mem, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ax_and_dx, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ax_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ax_and_mem, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ax_and_reg16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ax_and_sbyteword, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(cs_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ds_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(dx_and_al, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(dx_and_ax, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(es_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(imm16_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(imm_and_al, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(imm_and_ax, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(imm_and_cx, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(imm_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem16_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_al, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_ax, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_imm16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_imm8, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_reg16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_reg8, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_sbyteword16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(mem_and_seg, |_, _| { todo!() }, |_, _| { todo!() });

encoder!(
    none_and_none,
    |_, d: &InstructionData| { Ok(vec![d.op_code]) },
    |_, _| { Ok(1) }
);

encoder!(reg16_and_ax, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg16_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg16_and_mem, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg16_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg16_and_reg16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg16_and_seg, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg8_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg8_and_mem, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(reg8_and_reg8, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_cl, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_imm8, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_one, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm16_and_sbyteword, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm8_and_cl, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm8_and_imm, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm8_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(rm8_and_one, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(seg_and_mem, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(seg_and_reg16, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(seg_off_and_none, |_, _| { todo!() }, |_, _| { todo!() });
encoder!(ss_and_none, |_, _| { todo!() }, |_, _| { todo!() });

fn operand_to_type(operand: &ast::Operand) -> OperandType {
    match operand {
        ast::Operand::Immediate(expr) => {
            if expr.value() < 256 {
                OperandType::imm8
            } else {
                OperandType::imm16
            }
        }
        ast::Operand::Address(_, _, _) => todo!(),
        ast::Operand::Register(register) => match register {
            ast::Register::Byte(byte_register) => match byte_register {
                ast::ByteRegister::AL => OperandType::al,
                _ => OperandType::reg8,
            },
            ast::Register::Word(word_register) => match word_register {
                ast::WordRegister::AX => OperandType::ax,
                _ => OperandType::reg16,
            },
        },
        ast::Operand::Segment(_) => todo!(),
    }
}

fn coerce_operand_types(
    destination: OperandType,
    source: OperandType,
) -> (OperandType, OperandType) {
    use OperandType::*;

    if matches!(destination, imm16 | ax | reg16 | mem16 if source == imm8) {
        return (destination, imm16);
    }

    (destination, source)
}

fn find_instruction_data_for(instruction: &Instruction) -> Option<&'static InstructionData> {
    let (destination, source) = match &instruction.operands {
        ast::Operands::None(_) => (OperandType::none, OperandType::none),
        ast::Operands::Destination(_, destination) => {
            (operand_to_type(destination), OperandType::none)
        }
        ast::Operands::DestinationAndSource(_, destination, source) => {
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

pub fn size_in_bytes<'a>(instruction: &Instruction<'a>) -> Result<u32, EncodeError> {
    let data = match find_instruction_data_for(instruction) {
        Some(data) => data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    Ok(match (&data.destination, &data.source) {
        (OperandType::none, OperandType::none) => 1,
        (OperandType::al, OperandType::imm8) => 2,
        (OperandType::ax, OperandType::imm16) => 3,
        _ => todo!("{:?}, {:?}", &data.destination, &data.source),
    })
}

pub fn encode<'a>(instruction: &Instruction<'a>) -> Result<Vec<u8>, EncodeError> {
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

    #[test]
    fn operand_types() {
        assert_eq!(
            OperandType::imm_8,
            operand_to_type(&ast::Operand::Immediate(Box::new(ast::Expression::Term(
                ast::Value::Constant(10)
            ))))
        );

        assert_eq!(
            OperandType::imm_16,
            operand_to_type(&ast::Operand::Immediate(Box::new(ast::Expression::Term(
                ast::Value::Constant(256)
            ))))
        );

        assert_eq!(
            OperandType::reg_al,
            operand_to_type(&ast::Operand::Register(ast::Register::Byte(
                ast::ByteRegister::AL
            )))
        );

        assert_eq!(
            OperandType::reg_ax,
            operand_to_type(&ast::Operand::Register(ast::Register::Word(
                ast::WordRegister::AX
            )))
        );

        assert_eq!(
            OperandType::reg_8,
            operand_to_type(&ast::Operand::Register(ast::Register::Byte(
                ast::ByteRegister::DL
            )))
        );

        assert_eq!(
            OperandType::reg_16,
            operand_to_type(&ast::Operand::Register(ast::Register::Word(
                ast::WordRegister::DX
            )))
        );
    }
}
