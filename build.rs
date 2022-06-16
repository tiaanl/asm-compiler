#![allow(dead_code)]

use inflector::Inflector;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, digit1, space0, space1},
    combinator::{map, opt},
    error::{ErrorKind, ParseError},
    multi::separated_list1,
    sequence::{delimited, preceded, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition,
};
use serde::{de::Error, Deserialize, Deserializer};
use std::fmt::{Display, Formatter};
use std::{collections::HashSet, fs::File, io::Write};

#[derive(Debug, Deserialize)]
struct Encoder {
    name: String,
    codes: Vec<Code>,
}

#[derive(Debug, Deserialize)]
struct Record {
    mnemonic: String,
    destination: String,
    source: String,
    #[serde(deserialize_with = "do_code")]
    encoder: Encoder,
}

fn operand_to_type(operand: &str) -> String {
    if operand.is_empty() {
        String::from("none")
    } else {
        operand.to_owned()
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Deserialize)]
enum Code {
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

impl Display for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Code::ib => write!(f, "Code::ib"),
            Code::ibs => write!(f, "Code::ibs"),
            Code::iw => write!(f, "Code::iw"),
            Code::iwd => write!(f, "Code::iwd"),
            Code::seg => write!(f, "Code::seg"),
            Code::rel => write!(f, "Code::rel"),
            Code::repe => write!(f, "Code::repe"),
            Code::rel8 => write!(f, "Code::rel8"),
            Code::jmp8 => write!(f, "Code::jmp8"),
            Code::wait => write!(f, "Code::wait"),
            Code::byte(byte) => write!(f, "Code::byte({:#04x})", byte),
            Code::plus_reg(byte) => write!(f, "Code::plus_reg({:#04x})", byte),
            Code::mrm => write!(f, "Code::mrm"),
            Code::encoding(byte) => write!(f, "Code::encoding({:#04x})", byte),
        }
    }
}

fn str_to_code(s: &str) -> Option<Code> {
    Some(match s {
        "ib" => Code::ib,
        "ibs" => Code::ibs,
        "iw" => Code::iw,
        "iwd" => Code::iwd,
        "rel" => Code::rel,
        "seg" => Code::seg,
        "repe" => Code::repe,
        "rel8" => Code::rel8,
        "jmp8" => Code::jmp8,
        "wait" => Code::wait,
        _ => return None,
    })
}

fn encoder_name<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(|item| item.as_char() == ':', ErrorKind::AlphaNumeric)
}

fn parse_code(input: &str) -> IResult<&str, Code> {
    alt((
        map(terminated(alphanumeric1, tag("+r")), |s| {
            Code::plus_reg(u8::from_str_radix(s, 16).unwrap())
        }),
        map(tag("/r"), |_| Code::mrm),
        map(preceded(tag("/"), digit1), |s| {
            Code::encoding(u8::from_str_radix(s, 16).unwrap())
        }),
        map(alphanumeric1, |s| {
            if let Some(code) = str_to_code(s) {
                code
            } else {
                Code::byte(u8::from_str_radix(s, 16).unwrap())
            }
        }),
    ))(input)
}

fn parse_encoder(input: &str) -> IResult<&str, &str> {
    terminated(
        encoder_name, //
        tag(":"),     //
    )(input)
}

fn str_to_encoder_name(s: &str) -> Option<&str> {
    Some(match s {
        "i" => "immediate",
        "-i" => "immediate_source",
        "-r" => "register_source",
        "r" => "register",
        "ri" => "register_immediate",
        "m" => "memory",
        "mi" => "memory_immediate",
        "mr" => "memory_and_register",
        "rm" => "register_and_memory",
        "ji" => "jump_immediate",
        _ => return None,
    })
}

fn parse_record(input: &str) -> IResult<&str, (&str, Vec<Code>)> {
    delimited(
        tag("["),
        tuple((
            map(opt(terminated(parse_encoder, space0)), |name| {
                if let Some(name) = name {
                    if let Some(encoder_name) = str_to_encoder_name(name) {
                        encoder_name
                    } else {
                        panic!("invalid_code")
                    }
                } else {
                    "bytes_only"
                }
            }),
            separated_list1(space1, parse_code),
        )),
        tag("]"),
    )(input)
}

fn do_code<'de, D>(deserializer: D) -> Result<Encoder, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = serde::de::Deserialize::deserialize(deserializer)?;

    match parse_record(s) {
        Ok((_, (encoder, codes))) => Ok(Encoder {
            name: encoder.to_owned(),
            codes,
        }),
        Err(_) => Err(D::Error::custom(s)),
    }
}

fn write_record(out: &mut File, record: &Record) {
    let line = format!(
        "    id!({}, {}, {}, {}, &[{}]),\n",
        record.mnemonic.to_title_case(),
        operand_to_type(record.destination.as_str()),
        operand_to_type(record.source.as_str()),
        record.encoder.name,
        record
            .encoder
            .codes
            .iter()
            .map(|c| format!("{}", c))
            .collect::<Vec<String>>()
            .join(", "),
    );
    out.write_all(line.as_bytes()).unwrap();
}

const HEADER: &str = "//! This file is auto generated by build.rs, DO NOT MODIFY!!

use super::{Code, funcs::EncoderTrait, InstructionData, OperandType};

macro_rules! id {
    ($operation:ident, $destination:ident, $source:ident, $encoder:ident, $codes:expr) => {{
        InstructionData {
            operation: Operation::$operation,
            destination: OperandType::$destination,
            source: OperandType::$source,
            encoder: super::funcs::$encoder::encode,
            sizer: super::funcs::$encoder::size,
            codes: $codes,
        }
    }};
}

";

fn main() {
    println!("cargo:rerun-if-changed=instructions.csv");

    let mut rdr = csv::Reader::from_path("instructions.csv").unwrap();

    let records: Vec<Record> = rdr
        .deserialize::<Record>()
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    let mnemonics: HashSet<String> = records.iter().map(|r| r.mnemonic.clone()).collect();
    let mut mnemonics = mnemonics.into_iter().collect::<Vec<String>>();
    mnemonics.sort();

    let mut out = File::create("src/encoder/instructions.rs").unwrap();

    out.write_all(HEADER.as_bytes()).unwrap();
    out.write_all("#[rustfmt::skip]\npub const DATA: &[InstructionData] = &[\n".as_bytes())
        .unwrap();

    for record in &records {
        write_record(&mut out, record);
    }

    out.write_all(
        "];\n\n#[derive(Debug, Eq, PartialEq)]\n#[repr(u8)]\npub enum Operation {\n".as_bytes(),
    )
    .unwrap();
    mnemonics.iter().for_each(|m| {
        out.write_all(format!("    {},\n", m.to_title_case()).as_bytes())
            .unwrap()
    });
    out.write_all(
        "}\n\npub fn str_to_operation(s: &str) -> Option<Operation> {\n    Some(match s.to_lowercase().as_str() {\n"
            .as_bytes(),
    )
    .unwrap();
    mnemonics.iter().for_each(|m| {
        out.write_all(
            format!("        \"{}\" => Operation::{},\n", m, m.to_title_case()).as_bytes(),
        )
        .unwrap();
    });
    out.write_all("        _ => return None,\n    })\n}\n".as_bytes())
        .unwrap();
}
