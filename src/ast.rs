use crate::instructions::Operation;
use crate::lexer::Span;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[allow(clippy::upper_case_acronyms)]
#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ByteRegister {
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,
    AH = 4,
    CH = 5,
    DH = 6,
    BH = 7,
}

impl Display for ByteRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteRegister::AL => write!(f, "AL"),
            ByteRegister::AH => write!(f, "AH"),
            ByteRegister::CL => write!(f, "CL"),
            ByteRegister::CH => write!(f, "CH"),
            ByteRegister::DL => write!(f, "DL"),
            ByteRegister::DH => write!(f, "DH"),
            ByteRegister::BL => write!(f, "BL"),
            ByteRegister::BH => write!(f, "BH"),
        }
    }
}

impl ByteRegister {
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.to_lowercase().as_str() {
            "al" => Self::AL,
            "ah" => Self::AH,
            "cl" => Self::CL,
            "ch" => Self::CH,
            "dl" => Self::DL,
            "dh" => Self::DH,
            "bl" => Self::BL,
            "bh" => Self::BH,

            _ => return None,
        })
    }

    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[allow(clippy::upper_case_acronyms)]
#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WordRegister {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
}

impl Display for WordRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordRegister::AX => write!(f, "AX"),
            WordRegister::CX => write!(f, "CX"),
            WordRegister::DX => write!(f, "DX"),
            WordRegister::BX => write!(f, "BX"),
            WordRegister::SP => write!(f, "SP"),
            WordRegister::BP => write!(f, "BP"),
            WordRegister::SI => write!(f, "SI"),
            WordRegister::DI => write!(f, "DI"),
        }
    }
}

impl WordRegister {
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.to_lowercase().as_str() {
            "ax" => Self::AX,
            "cx" => Self::CX,
            "dx" => Self::DX,
            "bx" => Self::BX,
            "sp" => Self::SP,
            "bp" => Self::BP,
            "si" => Self::SI,
            "di" => Self::DI,

            _ => return None,
        })
    }

    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Register {
    Byte(ByteRegister),
    Word(WordRegister),
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Byte(byte) => write!(f, "{}", byte),
            Register::Word(word) => write!(f, "{}", word),
        }
    }
}

impl Register {
    pub fn from_str(s: &str) -> Option<Self> {
        if let Some(byte_register) = ByteRegister::from_str(s) {
            Some(Register::Byte(byte_register))
        } else {
            WordRegister::from_str(s).map(Register::Word)
        }
    }

    pub fn encoding(&self) -> u8 {
        match self {
            Register::Byte(r) => r.encoding(),
            Register::Word(r) => r.encoding(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Segment {
    ES = 0,
    CS = 1,
    SS = 2,
    DS = 3,
}

impl Display for Segment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Segment::ES => write!(f, "ES"),
            Segment::CS => write!(f, "CS"),
            Segment::SS => write!(f, "SS"),
            Segment::DS => write!(f, "DS"),
        }
    }
}

impl Segment {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "es" => Some(Self::ES),
            "cs" => Some(Self::CS),
            "ss" => Some(Self::SS),
            "ds" => Some(Self::DS),
            _ => None,
        }
    }

    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataSize {
    Byte,
    Word,
}

impl Display for DataSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DataSize::Byte => write!(f, "BYTE"),
            DataSize::Word => write!(f, "WORD"),
        }
    }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Constant(i32),
    Label(String),
    Register(Register),
}

impl<'a> Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(value) => write!(f, "{}", *value),
            Value::Label(label) => write!(f, "{}", *label),
            Value::Register(register) => write!(f, "reg({})", *register),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Subtract => write!(f, "-"),
            Operator::Multiply => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    PrefixOperator(Operator, Box<Expression>),
    InfixOperator(Operator, Box<Expression>, Box<Expression>),

    Term(Value),
}

impl<'a> Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::PrefixOperator(operator, right) => {
                write!(f, "{}({})", operator, right)
            }
            Expression::InfixOperator(operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Term(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Immediate(Expression),
    Address(Option<DataSize>, Expression, Option<Segment>),
    Register(Register),
    Segment(Segment),
}

impl<'a> Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Immediate(expr) => write!(f, "{}", expr),

            Operand::Address(data_size, expr, segment) => {
                if let Some(data_size) = data_size {
                    write!(f, "{} ", data_size)?;
                }
                write!(f, "[")?;

                if let Some(segment) = segment {
                    write!(f, "{}:", segment)?;
                }

                write!(f, "{}", expr)?;

                write!(f, "]")
            }

            Operand::Register(register) => write!(f, "{}", register),

            Operand::Segment(segment) => write!(f, "{}", segment),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operands {
    None(Span),
    Destination(Span, Operand),
    DestinationAndSource(Span, Operand, Operand),
}

impl<'a> Operands {
    #[allow(unused)]
    pub fn span(&self) -> &Span {
        match self {
            Operands::None(span)
            | Operands::Destination(span, _)
            | Operands::DestinationAndSource(span, _, _) => span,
        }
    }
}

impl<'a> Display for Operands {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operands::None(_) => Ok(()),
            Operands::Destination(_, destination) => write!(f, "{}", destination),
            Operands::DestinationAndSource(_, destination, source) => {
                write!(f, "{}, {}", destination, source)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub operation: Operation,
    pub operands: Operands,
}

impl<'a> Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Operands::None(_) = self.operands {
            write!(f, "{:?}", self.operation)
        } else {
            write!(f, "{:?} {}", self.operation, self.operands)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Line<'a> {
    Label(Span, &'a str),
    Instruction(Span, Instruction),
    Data(Vec<u8>),
    Constant(Expression),
    Times(Expression),
}

impl<'a> Display for Line<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(_, name) => write!(f, "{}:", name),
            Line::Instruction(_, instruction) => write!(f, "    {}", instruction),
            Line::Data(data) => write!(f, "    {:?}", data),
            Line::Constant(value) => write!(f, "    equ {}", value),
            Line::Times(expression) => write!(f, "    times {}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a> {
    pub lines: Vec<Line<'a>>,
    pub labels: HashMap<&'a str, usize>,
}
