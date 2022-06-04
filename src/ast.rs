use std::collections::HashMap;
use std::fmt::Formatter;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    // Data transfer
    MOV,   // Move
    PUSH,  // Push
    POP,   // Pop
    XCHG,  // Exchange
    IN,    // Input from
    OUT,   // Output to
    XLAT,  // Translate byte to AL
    LEA,   // Load effective address to register
    LDS,   // Load pointer to DS
    LES,   // Load pointer to ES
    LAHF,  // Load AH with flags
    SAHF,  // Store AH into flags
    PUSHF, // Push flags
    POPF,  // Pop flags

    // Arithmetic
    ADD,  // Add
    ADC,  // Add with carry
    INC,  // Increment
    AAA,  // ASCII adjust for add
    DAA,  // Decimal adjust for add
    SUB,  // Subtract
    SBB,  // Subtract with borrow
    DEC,  // Decrement
    NEG,  // Change sign
    CMP,  // Compare
    AAS,  // ASCII adjust for subtract
    DAS,  // Decimal adjust for subtract
    MUL,  // Multiply (unsigned)
    IMUL, // Integer multiply (signed)
    AAM,  // ASCII adjust for multiply
    DIV,  // Divide (unsigned)
    IDIV, // Integer divide (signed)
    AAD,  // ASCII adjust for divide
    CBW,  // Convert byte to word
    CWD,  // Convert word to double word

    // Logic
    NOT,  // Invert
    SHL,  // Shift logical left (alias: SAL)
    SHR,  // Shift logical right
    SAR,  // Shift arithmetic right
    ROL,  // Rotate left
    ROR,  // Rotate right
    RCL,  // Rotate through carry flag left
    RCR,  // Rotate through carry flag right
    AND,  // And
    TEST, // And function to flags, no result
    OR,   // Or
    XOR,  // Exclusive or

    // String manipulation
    // TODO: Should these be without size and have the size as another operand type?
    REP,   // Repeat
    MOVSB, // Move byte
    MOVSW, // Move word
    CMPSB, // Compare byte
    CMPSW, // Compare word
    SCASB, // Scan byte
    SCASW, // Scan word
    LODSB, // Load byte to AL
    LODSW, // Load word to AX
    STOSB, // Store byte to AL
    STOSW, // Store word to AX

    // Control transfer
    CALL,   // Call
    JMP,    // Unconditional jump
    RET,    // Return from CALL
    JE,     // Jump on equal/zero (alias JZ)
    JL,     // Jump on less/not greater or equal (alias JNGE)
    JLE,    // Jump on less or equal/not greater (alias JNG)
    JB,     // Jump on below/not above or equal (alias JNAE)
    JBE,    // Jump on below or equal/not above (alias JNA)
    JP,     // Jump on parity/parity even (alias JPE)
    JO,     // Jump on overflow
    JS,     // Jump on sign
    JNE,    // Jump on not equal/not zero (alias JNZ)
    JNL,    // Jump on not less/greater or equal (alias JGE)
    JNLE,   // Jump on not less or equal/greater (alias JG)
    JNB,    // Jump on not below/above or equal (alias JAE)
    JNBE,   // Jump on not below or equal/above (alias JA)
    JNP,    // Jump on not parity/parity odd (alias JPO)
    JNO,    // Jump on not overflow
    JNS,    // Jump on not sign
    LOOP,   // Loop CX times
    LOOPE,  // Loop while zero/equal (alias LOOPZ)
    LOOPNE, // Loop while not zero/equal (alias LOOPNZ)
    JCXZ,   // Jump on CX zero
    INT,    // Interrupt
    INT1,   // Interrupt 1
    INT3,   // Interrupt 3
    INTO,   // Interrupt on overflow
    IRET,   // Interrupt return

    // Processor control
    CLC,  // Clear carry
    CMC,  // Complement carry
    STC,  // Set carry
    CLD,  // Clear direction
    STD,  // Set direction
    CLI,  // Clear interrupt
    STI,  // Set interrupt
    HLT,  // Halt
    WAIT, // Wait
    ESC,  // Escape (to external device)
    LOCK, // Bus lock prefix

    NOP, // No operation

    // Undocumented
    SALC, // Set AL on carry
}

impl Operation {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "aaa" => Some(Self::AAA),
            "aad" => Some(Self::AAD),
            "aam" => Some(Self::AAM),
            "aas" => Some(Self::AAS),
            "adc" => Some(Self::ADC),
            "add" => Some(Self::ADD),
            "and" => Some(Self::AND),
            "call" => Some(Self::CALL),
            "cbw" => Some(Self::CBW),
            "clc" => Some(Self::CLC),
            "cld" => Some(Self::CLD),
            "cli" => Some(Self::CLI),
            "cmc" => Some(Self::CMC),
            "cmp" => Some(Self::CMP),
            "cmpsb" => Some(Self::CMPSB),
            "cmpsw" => Some(Self::CMPSW),
            "cwd" => Some(Self::CWD),
            "daa" => Some(Self::DAA),
            "das" => Some(Self::DAS),
            "dec" => Some(Self::DEC),
            "div" => Some(Self::DIV),
            "esc" => Some(Self::ESC),
            "hlt" => Some(Self::HLT),
            "idiv" => Some(Self::IDIV),
            "imul" => Some(Self::IMUL),
            "in" => Some(Self::IN),
            "inc" => Some(Self::INC),
            "int" => Some(Self::INT),
            "int1" => Some(Self::INT1),
            "int3" => Some(Self::INT3),
            "into" => Some(Self::INTO),
            "iret" => Some(Self::IRET),
            "ja" => Some(Self::JNBE),
            "jae" => Some(Self::JNB),
            "jb" => Some(Self::JB),
            "jbe" => Some(Self::JBE),
            "jc" => Some(Self::JB),
            "jcxz" => Some(Self::JCXZ),
            "je" => Some(Self::JE),
            "jg" => Some(Self::JNLE),
            "jge" => Some(Self::JNL),
            "jl" => Some(Self::JL),
            "jle" => Some(Self::JLE),
            "jmp" => Some(Self::JMP),
            "jna" => Some(Self::JBE),
            "jnae" => Some(Self::JB),
            "jnb" => Some(Self::JNB),
            "jnbe" => Some(Self::JNBE),
            "jnc" => Some(Self::JNB),
            "jne" => Some(Self::JNE),
            "jnge" => Some(Self::JL),
            "jnl" => Some(Self::JNL),
            "jnle" => Some(Self::JNLE),
            "jno" => Some(Self::JNO),
            "jnp" => Some(Self::JNP),
            "jns" => Some(Self::JNS),
            "jnz" => Some(Self::JNE),
            "jo" => Some(Self::JO),
            "jp" => Some(Self::JP),
            "jpe" => Some(Self::JP),
            "jpo" => Some(Self::JNP),
            "js" => Some(Self::JS),
            "jz" => Some(Self::JE),
            "lahf" => Some(Self::LAHF),
            "lds" => Some(Self::LDS),
            "lea" => Some(Self::LEA),
            "les" => Some(Self::LES),
            "lock" => Some(Self::LOCK),
            "lodsb" => Some(Self::LODSB),
            "lodsw" => Some(Self::LODSW),
            "loop" => Some(Self::LOOP),
            "loope" => Some(Self::LOOPE),
            "loopne" => Some(Self::LOOPNE),
            "mov" => Some(Self::MOV),
            "movsb" => Some(Self::MOVSB),
            "movsw" => Some(Self::MOVSW),
            "mul" => Some(Self::MUL),
            "neg" => Some(Self::NEG),
            "nop" => Some(Self::NOP),
            "not" => Some(Self::NOT),
            "or" => Some(Self::OR),
            "out" => Some(Self::OUT),
            "pop" => Some(Self::POP),
            "popf" => Some(Self::POPF),
            "push" => Some(Self::PUSH),
            "pushf" => Some(Self::PUSHF),
            "rcl" => Some(Self::RCL),
            "rcr" => Some(Self::RCR),
            "rep" => Some(Self::REP),
            "ret" => Some(Self::RET),
            "rol" => Some(Self::ROL),
            "ror" => Some(Self::ROR),
            "sahf" => Some(Self::SAHF),
            "salc" => Some(Self::SALC),
            "sar" => Some(Self::SAR),
            "sbb" => Some(Self::SBB),
            "scasb" => Some(Self::SCASB),
            "scasw" => Some(Self::SCASW),
            "shl" => Some(Self::SHL),
            "shr" => Some(Self::SHR),
            "stc" => Some(Self::STC),
            "std" => Some(Self::STD),
            "sti" => Some(Self::STI),
            "stosb" => Some(Self::STOSB),
            "stosw" => Some(Self::STOSW),
            "sub" => Some(Self::SUB),
            "test" => Some(Self::TEST),
            "wait" => Some(Self::WAIT),
            "xchg" => Some(Self::XCHG),
            "xlat" => Some(Self::XLAT),
            "xor" => Some(Self::XOR),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Segment {
    CS,
    DS,
    ES,
    SS,
}

impl Segment {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "cs" => Some(Self::CS),
            "ds" => Some(Self::DS),
            "es" => Some(Self::ES),
            "ss" => Some(Self::SS),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DataSize {
    Byte,
    Word,
}

impl DataSize {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "byte" => Some(Self::Byte),
            "word" => Some(Self::Word),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddressOrLabel<'a> {
    // Address(u32),
    Label(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operand<'a> {
    Immediate(i32),
    DirectAddress(Option<DataSize>, AddressOrLabel<'a>, Option<Segment>, i32),
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
    pub operation: Operation,
    pub operands: Operands<'a>,
}

impl<'a> std::fmt::Display for Instruction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.operation, self.operands)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Data {
    Byte(Vec<u8>),
    Word(Vec<u8>),
    DoubleWord(Vec<u8>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Line<'a> {
    Label(&'a str),
    Instruction(Instruction<'a>),
    Data(Data),
    Constant(i32),
}

impl<'a> std::fmt::Display for Line<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(name) => write!(f, "{}:", name),
            Line::Instruction(instruction) => write!(f, "  {}", instruction),
            Line::Data(data) => write!(f, "{:?}", data),
            Line::Constant(value) => write!(f, "equ {}", value),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a> {
    pub lines: Vec<Line<'a>>,
    pub labels: HashMap<&'a str, usize>,
}
