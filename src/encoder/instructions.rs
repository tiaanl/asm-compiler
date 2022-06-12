use super::{InstructionData, OperandType};

macro_rules! id {
    ($operation:ident, $op_code:literal, $destination:ident, $source:ident, $encoder:ident) => {{
        InstructionData {
            operation: Operation::$operation,
            op_code: $op_code,
            destination: OperandType::$destination,
            source: OperandType::$source,
            encoder: super::$encoder,
        }
    }};
}

pub const DATA: &[InstructionData] = &[
    id!(Add, 0x04, al, imm_8, encode_al_and_imm_8),
    id!(Add, 0x05, ax, imm_16, encode_ax_and_imm_16),
    id!(Add, 0x00, reg_mem_8, reg_8, encode_reg_mem_8_and_reg_8),
    id!(Add, 0x01, reg_mem_16, reg_16, encode_reg_mem_16_and_reg_16),
    id!(Add, 0x02, reg_8, reg_mem_8, encode_reg_8_and_reg_mem_8),
    id!(Add, 0x03, reg_16, reg_mem_16, encode_reg_16_and_reg_mem_16),
    id!(Clc, 0xF8, none, none, encode_none_and_none),
    id!(Stc, 0xF9, none, none, encode_none_and_none),
    id!(Jmp, 0xEB, disp_8, none, encode_disp_8_and_none),
    id!(Jmp, 0xE9, disp_16, none, encode_disp_16_and_none),
];

#[derive(Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Operation {
    Add,
    Clc,
    Jmp,
    Stc,
}

pub fn str_to_operation(s: &str) -> Option<Operation> {
    Some(match s {
        "add" => Operation::Add,
        "clc" => Operation::Clc,
        "jmp" => Operation::Jmp,
        "stc" => Operation::Stc,
        _ => return None,
    })
}
