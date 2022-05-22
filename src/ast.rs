#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    Immediate(i32),
    // DirectAddress,
    // IndirectAddress,
    // Register,
    // Displacement,
    // FarAddress,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operands {
    None,
    Destination(Operand),
    DestinationAndSource(Operand, Operand),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction<'a> {
    pub mnemonic: &'a str,
    pub operands: Operands,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub origin: u32,
    pub instructions: Vec<Instruction<'a>>,
}
