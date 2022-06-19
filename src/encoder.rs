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

impl Code {
    pub fn encode(&self, instruction: &ast::Instruction, output: &mut Vec<u8>) -> bool {
        match self {
            Code::ib => match &instruction.operands {
                ast::Operands::Destination(_, ast::Operand::Immediate(_expr)) => {
                    eprintln!("WARNING: dummy immediate");
                    output.push(0);
                    true
                }

                ast::Operands::DestinationAndSource(_, ast::Operand::Immediate(_expr), _) => {
                    eprintln!("WARNING: dummy immediate");
                    output.push(0);
                    true
                }

                ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_expr)) => {
                    eprintln!("WARNING: dummy immediate");
                    output.push(0);
                    true
                }

                _ => false,
            },

            Code::byte(byte) => {
                output.push(*byte);
                true
            }

            Code::plus_reg(byte) => match &instruction.operands {
                ast::Operands::Destination(_, ast::Operand::Register(register)) => {
                    output.push(byte.wrapping_add(register.encoding()));
                    true
                }

                ast::Operands::DestinationAndSource(_, ast::Operand::Register(register), _) => {
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
                    ast::Operand::Segment(seg),
                ) => {
                    output.push(mod_reg_rm(0b11, seg.encoding(), reg.encoding()));
                    true
                }

                ast::Operands::DestinationAndSource(
                    _,
                    ast::Operand::Segment(reg),
                    ast::Operand::Register(seg),
                ) => {
                    output.push(mod_reg_rm(0b11, reg.encoding(), seg.encoding()));
                    true
                }

                _ => false,
            },

            _ => false,
        }
    }

    fn size_in_bytes(&self) -> u32 {
        match self {
            Code::ib => 1,
            Code::ibs => 1,
            Code::iw => 2,
            Code::iwd => 2,
            Code::seg => todo!(),
            Code::rel => todo!(),
            Code::repe => todo!(),
            Code::rel8 => todo!(),
            Code::jmp8 => todo!(),
            Code::wait => todo!(),
            Code::byte(_) => 1,
            Code::plus_reg(_) => 1,
            Code::mrm => 1,
            Code::encoding(_) => 1,
        }
    }
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

type Encoder =
    fn(&Code, &ast::Instruction, &InstructionData, &mut Vec<u8>) -> Result<(), EncodeError>;
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
fn mod_reg_rm(encoding: u8, reg: u8, mem: u8) -> u8 {
    debug_assert!(encoding <= 0b11);
    debug_assert!(reg <= 0b111);
    debug_assert!(mem <= 0b111);
    (encoding << 6) | (reg << 3) | mem
}

pub mod funcs {
    use super::*;

    pub trait EncoderTrait {
        fn encode(
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError>;

        fn size(instruction: &ast::Instruction, data: &InstructionData)
            -> Result<u32, EncodeError>;
    }

    pub mod bytes_only {
        use super::*;

        pub fn encode(
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
            todo!("{:?} {:?}", instruction, data.codes)
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
        fn encode(
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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

            Ok(())
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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

            Ok(())
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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

            Ok(())
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
            dbg!(data.codes);
            match &instruction.operands {
                ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(expr)) => {
                    todo!()
                }

                ast::Operands::DestinationAndSource(_, _, ast::Operand::Address(_, expr, _)) => {
                    // An address can be written as an immediate
                    output.push(0);
                    output.push(0);
                    eprintln!("WARNING: dummy address");
                    Ok(())
                }

                _ => unreachable!("{:?}", instruction),
            }
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
            todo!("{:?} {:?}", instruction, data.codes)
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
            code: &Code,
            instruction: &ast::Instruction,
            data: &InstructionData,
            output: &mut Vec<u8>,
        ) -> Result<(), EncodeError> {
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

impl<'a> ast::Operand {
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

pub fn size_in_bytes<'a>(instruction: &ast::Instruction) -> Result<u32, EncodeError> {
    let data = match find_instruction_data_for(instruction) {
        Some(data) => data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    Ok(data
        .codes
        .iter()
        .map(|c| c.size_in_bytes())
        .reduce(|a, v| a + v)
        .unwrap())
}

pub fn encode<'a>(instruction: &ast::Instruction) -> Result<Vec<u8>, EncodeError> {
    let data = match find_instruction_data_for(instruction) {
        Some(data) => data,
        None => {
            let span = instruction.operands.span();
            return Err(EncodeError::InvalidOperands(span.start..span.end));
        }
    };

    let mut output = vec![];

    let encoder = data.encoder;

    for code in data.codes {
        if !code.encode(instruction, &mut output) {
            encoder(code, instruction, data, &mut output)?;
        }
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn operand_types() {
    //     assert_eq!(
    //         OperandType::imm8,
    //         operand_to_type(&ast::Operand::Immediate(ast::Expression::Term(
    //             ast::Value::Constant(10)
    //         )))
    //     );
    //
    //     assert_eq!(
    //         OperandType::imm16,
    //         operand_to_type(&ast::Operand::Immediate(ast::Expression::Term(
    //             ast::Value::Constant(256)
    //         )))
    //     );
    //
    //     assert_eq!(
    //         OperandType::al,
    //         operand_to_type(&ast::Operand::Register(ast::Register::Byte(
    //             ast::ByteRegister::AL
    //         )))
    //     );
    //
    //     assert_eq!(
    //         OperandType::ax,
    //         operand_to_type(&ast::Operand::Register(ast::Register::Word(
    //             ast::WordRegister::AX
    //         )))
    //     );
    //
    //     assert_eq!(
    //         OperandType::reg8,
    //         operand_to_type(&ast::Operand::Register(ast::Register::Byte(
    //             ast::ByteRegister::DL
    //         )))
    //     );
    //
    //     assert_eq!(
    //         OperandType::reg16,
    //         operand_to_type(&ast::Operand::Register(ast::Register::Word(
    //             ast::WordRegister::DX
    //         )))
    //     );
    // }
}
