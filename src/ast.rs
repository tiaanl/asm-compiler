use crate::instructions::Operation;
use crate::lexer::Span;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ByteRegister {
    Al = 0,
    Cl = 1,
    Dl = 2,
    Bl = 3,
    Ah = 4,
    Ch = 5,
    Dh = 6,
    Bh = 7,
}

impl Display for ByteRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteRegister::Al => write!(f, "AL"),
            ByteRegister::Ah => write!(f, "AH"),
            ByteRegister::Cl => write!(f, "CL"),
            ByteRegister::Ch => write!(f, "CH"),
            ByteRegister::Dl => write!(f, "DL"),
            ByteRegister::Dh => write!(f, "DH"),
            ByteRegister::Bl => write!(f, "BL"),
            ByteRegister::Bh => write!(f, "BH"),
        }
    }
}

impl ByteRegister {
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.to_lowercase().as_str() {
            "al" => Self::Al,
            "ah" => Self::Ah,
            "cl" => Self::Cl,
            "ch" => Self::Ch,
            "dl" => Self::Dl,
            "dh" => Self::Dh,
            "bl" => Self::Bl,
            "bh" => Self::Bh,

            _ => return None,
        })
    }

    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum WordRegister {
    Ax = 0,
    Cx = 1,
    Dx = 2,
    Bx = 3,
    Sp = 4,
    Bp = 5,
    Si = 6,
    Di = 7,
}

impl Display for WordRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordRegister::Ax => write!(f, "AX"),
            WordRegister::Cx => write!(f, "CX"),
            WordRegister::Dx => write!(f, "DX"),
            WordRegister::Bx => write!(f, "BX"),
            WordRegister::Sp => write!(f, "SP"),
            WordRegister::Bp => write!(f, "BP"),
            WordRegister::Si => write!(f, "SI"),
            WordRegister::Di => write!(f, "DI"),
        }
    }
}

impl WordRegister {
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.to_lowercase().as_str() {
            "ax" => Self::Ax,
            "cx" => Self::Cx,
            "dx" => Self::Dx,
            "bx" => Self::Bx,
            "sp" => Self::Sp,
            "bp" => Self::Bp,
            "si" => Self::Si,
            "di" => Self::Di,

            _ => return None,
        })
    }

    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

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

    #[inline]
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

    #[inline]
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
            Value::Register(register) => write!(f, "{}", *register),
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
            Operand::Immediate(expr) => expr.fmt(f),

            Operand::Address(data_size, expr, segment) => {
                if let Some(data_size) = data_size {
                    write!(f, "{} ", data_size)?;
                }
                write!(f, "[")?;

                if let Some(segment) = segment {
                    write!(f, "{}:", segment)?;
                }

                expr.fmt(f)?;

                write!(f, "]")
            }

            Operand::Register(register) => register.fmt(f),

            Operand::Segment(segment) => segment.fmt(f),
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
pub enum Line {
    Label(Span, String),
    Instruction(Span, Instruction),
    Data(Vec<u8>),
    Constant(Expression),
    Times(Expression),
}

impl<'a> Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(_, label) => write!(f, "{}:", label),
            Line::Instruction(_, instruction) => write!(f, "    {}", instruction),
            Line::Data(data) => write!(f, "    {:?}", data),
            Line::Constant(value) => write!(f, "    equ {}", value),
            Line::Times(expression) => write!(f, "    times {}", expression),
        }
    }
}
