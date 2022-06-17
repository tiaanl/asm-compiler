use crate::ast;
use crate::instructions::Operation;
use crate::lexer::Span;

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Code {
    ib,
    ibs,
    iw,
    iwd,
    seg,
    rel,
    repe,
    rel8,
    jmp8,
    wait,

    byte(u8),
    plus_reg(u8),
    mrm,
    encoding(u8),
}

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
type Sizer = fn(&ast::Instruction, &InstructionData) -> Result<u32, EncodeError>;

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidOperands(_) => write!(f, "Invalid operands"),
        }
    }
}

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperandType {
    none = 1,

    one = 2,

    imm = 3,
    imm8 = 4,
    imm16 = 5,

    sbyteword = 6,
    sbyteword16 = 7,

    al = 8,
    ax = 9,
    cl = 10,
    cx = 11,
    dx = 12,
    reg8 = 13,
    reg16 = 14,

    es = 15,
    cs = 16,
    ss = 17,
    ds = 18,
    seg = 19,

    mem = 20,
    _mem8 = 21,
    mem16 = 22,

    rm8 = 23,
    rm16 = 24,

    _disp8 = 25,
    _disp16 = 26,

    seg_off = 27,
}

pub struct InstructionData {
    pub operation: Operation,
    pub destination: OperandType,
    pub source: OperandType,
    pub encoder: Encoder,
    pub sizer: Sizer,
    pub codes: &'static [Code],
}

#[must_use]
fn mod_reg_rm(encoding: u8, register: u8, mem: u8) -> u8 {
    (encoding << 6) | (register << 3) | mem
}

fn code_common(code: &Code, instruction: &ast::Instruction, output: &mut Vec<u8>) -> bool {
    match code {
        Code::byte(byte) => {
            output.push(*byte);
            true
        }

        Code::plus_reg(byte) => match &instruction.operands {
            ast::Operands::Destination(_, ast::Operand::Register(register)) => {
                output.push(byte.wrapping_add(register.encoding()));
                true
            }

            _ => false,
        },

        Code::mrm => match &instruction.operands {
            ast::Operands::DestinationAndSource(
                _,
                ast::Operand::Register(reg),
                ast::Operand::Register(mem),
            ) => {
                output.push(mod_reg_rm(0b11, reg.encoding(), mem.encoding()));
                true
            }

            ast::Operands::DestinationAndSource(
                _,
                ast::Operand::Register(reg),
                ast::Operand::Segment(mem),
            ) => {
                output.push(mod_reg_rm(0b11, reg.encoding(), mem.encoding()));
                true
            }

            ast::Operands::DestinationAndSource(
                _,
                ast::Operand::Segment(reg),
                ast::Operand::Register(mem),
            ) => {
                output.push(mod_reg_rm(0b11, reg.encoding(), mem.encoding()));
                true
            }

            _ => false,
        },

        _ => false,
    }
}

pub mod funcs {
    use super::*;

    pub trait EncoderTrait {
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError>;

        fn size(instruction: &ast::Instruction, data: &InstructionData)
            -> Result<u32, EncodeError>;
    }

    pub mod bytes_only {
        use super::*;

        pub fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            todo!()
        }

        pub fn size(
            _instruction: &ast::Instruction,
            _data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            todo!()
        }
    }

    #[allow(non_camel_case_types)]
    pub struct immediate;
    impl EncoderTrait for immediate {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct memory_and_register;
    impl EncoderTrait for memory_and_register {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| {
                let mut output = vec![];

                for code in data.codes {
                    if !code_common(code, instruction, &mut output) {
                        match code {
                            Code::mrm => match &instruction.operands {
                                ast::Operands::DestinationAndSource(
                                    _,
                                    ast::Operand::Address(_, _, _),
                                    ast::Operand::Register(_),
                                ) => {
                                    todo!("mem reg")
                                }

                                _ => unreachable!("{:?}", instruction),
                            },

                            _ => todo!("{:?}", code),
                        }
                    }
                }

                Ok(output)
            })(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct register_and_memory;
    impl EncoderTrait for register_and_memory {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| {
                let mut output = vec![];

                for code in data.codes {
                    if !code_common(code, instruction, &mut output) {
                        match code {
                            Code::byte(byte) => output.push(*byte),
                            Code::mrm => match &instruction.operands {
                                ast::Operands::DestinationAndSource(
                                    _,
                                    ast::Operand::Address(_, _, _),
                                    ast::Operand::Register(_),
                                ) => {
                                    todo!("mem reg")
                                }

                                _ => unreachable!("{:?}", instruction),
                            },
                            _ => todo!("{:?}", code),
                        }
                    }
                }

                Ok(output)
            })(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    pub mod memory_immediate {
        use super::*;

        pub fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            let mut output = vec![];

            dbg!(data.codes);

            for code in data.codes {
                match code {
                    Code::byte(byte) => output.push(*byte),
                    Code::encoding(_encoding) => match &instruction.operands {
                        ast::Operands::DestinationAndSource(
                            _,
                            ast::Operand::Address(_, _, _),
                            ast::Operand::Immediate(_),
                        ) => {
                            todo!()
                        }
                        _ => panic!("Invalid operands: {:?}", instruction.operands),
                    },
                    _ => panic!("invalid code"),
                }
            }

            Ok(output)
        }

        pub fn size(
            _instruction: &ast::Instruction,
            _data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            todo!()
        }
    }

    #[allow(non_camel_case_types)]
    pub struct immediate_source;

    impl EncoderTrait for immediate_source {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!("{:?}", instruction))(
                instruction,
                data,
            )
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct jump_immediate;
    impl EncoderTrait for jump_immediate {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct memory;
    impl EncoderTrait for memory {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct register;
    impl EncoderTrait for register {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct register_immediate;
    impl EncoderTrait for register_immediate {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }

    #[allow(non_camel_case_types)]
    pub struct register_source;
    impl EncoderTrait for register_source {
        #[inline]
        fn encode(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<Vec<u8>, EncodeError> {
            (|instruction: &ast::Instruction, data: &InstructionData| todo!())(instruction, data)
        }

        #[inline]
        fn size(
            instruction: &ast::Instruction,
            data: &InstructionData,
        ) -> Result<u32, EncodeError> {
            (|_, _| todo!())(instruction, data)
        }
    }
}

impl<'a> ast::Operand<'a> {
    fn matches_operand_type(&self, operand_type: OperandType) -> bool {
        match self {
            ast::Operand::Immediate(_) => match operand_type {
                OperandType::one
                | OperandType::imm
                | OperandType::imm8
                | OperandType::imm16
                | OperandType::sbyteword
                | OperandType::sbyteword16 => true,
                _ => false,
            },
            ast::Operand::Address(_, _, _) => match operand_type {
                OperandType::mem
                | OperandType::_mem8
                | OperandType::mem16
                | OperandType::rm8
                | OperandType::rm16 => true,
                _ => false,
            },
            ast::Operand::Register(_) => match operand_type {
                OperandType::al
                | OperandType::ax
                | OperandType::cl
                | OperandType::cx
                | OperandType::dx
                | OperandType::reg8
                | OperandType::reg16
                | OperandType::rm8
                | OperandType::rm16 => true,
                _ => false,
            },
            ast::Operand::Segment(_) => match operand_type {
                OperandType::es
                | OperandType::cs
                | OperandType::ss
                | OperandType::ds
                | OperandType::seg => true,
                _ => false,
            },
        }
    }
}

fn find_instruction_data_for(instruction: &ast::Instruction) -> Option<&'static InstructionData> {
    for data in crate::instructions::DATA {
        if data.operation != instruction.operation {
            continue;
        }

        match &instruction.operands {
            ast::Operands::None(_) => {
                if data.destination == OperandType::none && data.source == OperandType::none {
                    return Some(data);
                }
            }
            ast::Operands::Destination(_, destination) => {
                if data.source == OperandType::none
                    && destination.matches_operand_type(data.destination)
                {
                    return Some(data);
                }
            }
            ast::Operands::DestinationAndSource(_, destination, source) => {
                if destination.matches_operand_type(data.destination)
                    && source.matches_operand_type(data.source)
                {
                    return Some(data);
                }
            }
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
        (OperandType::al, OperandType::imm8) => 2,
        (OperandType::ax, OperandType::imm16) => 3,
        _ => 0,
        // _ => todo!("{:?}, {:?}", &data.destination, &data.source),
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
            OperandType::imm8,
            operand_to_type(&ast::Operand::Immediate(ast::Expression::Term(
                ast::Value::Constant(10)
            )))
        );

        assert_eq!(
            OperandType::imm16,
            operand_to_type(&ast::Operand::Immediate(ast::Expression::Term(
                ast::Value::Constant(256)
            )))
        );

        assert_eq!(
            OperandType::al,
            operand_to_type(&ast::Operand::Register(ast::Register::Byte(
                ast::ByteRegister::AL
            )))
        );

        assert_eq!(
            OperandType::ax,
            operand_to_type(&ast::Operand::Register(ast::Register::Word(
                ast::WordRegister::AX
            )))
        );

        assert_eq!(
            OperandType::reg8,
            operand_to_type(&ast::Operand::Register(ast::Register::Byte(
                ast::ByteRegister::DL
            )))
        );

        assert_eq!(
            OperandType::reg16,
            operand_to_type(&ast::Operand::Register(ast::Register::Word(
                ast::WordRegister::DX
            )))
        );
    }
}
