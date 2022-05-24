#[derive(Debug, PartialEq, Eq)]
pub enum DataSize {
    Byte,
    Word,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddressOrLabel<'a> {
    // Address(u32),
    Label(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operand<'a> {
    Immediate(i32),
    DirectAddress(Option<DataSize>, AddressOrLabel<'a>),
    // IndirectAddress,
    Register(&'a str),
    // Displacement,
    // FarAddress,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operands<'a> {
    None,
    Destination(Operand<'a>),
    DestinationAndSource(Operand<'a>, Operand<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction<'a> {
    pub mnemonic: &'a str,
    pub operands: Operands<'a>,
}
