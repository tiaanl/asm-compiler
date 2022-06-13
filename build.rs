use inflector::Inflector;
use nom::IResult;
use serde::de::Error;
use serde::{Deserialize, Deserializer};
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

#[derive(Debug, Deserialize)]
struct Record {
    mnemonic: String,
    op_code: String,
    destination: String,
    source: String,
    #[serde(deserialize_with = "do_code")]
    encoder: Code,
}

fn operand_to_type(operand: &str) -> String {
    if operand.is_empty() {
        String::from("none")
    } else {
        operand.to_owned()
    }
}

#[derive(Debug)]
enum Code {
    Unknown,
}

fn parse_record(input: &str) -> IResult<&str, Code> {
    Ok((input, Code::Unknown))
}

fn do_code<'de, D>(deserializer: D) -> Result<Code, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = serde::de::Deserialize::deserialize(deserializer)?;

    match parse_record(s) {
        Ok((_, code)) => Ok(code),
        Err(_) => Err(D::Error::custom("test")),
    }
}

fn write_record(out: &mut File, record: &Record) {
    let encoder = format!(
        "{}_and_{}",
        operand_to_type(record.destination.as_str()),
        operand_to_type(record.source.as_str())
    );

    let line = format!(
        "    id!({}, {}, {}, {}, {}),\n",
        record.mnemonic.to_title_case(),
        record.op_code,
        operand_to_type(record.destination.as_str()),
        operand_to_type(record.source.as_str()),
        encoder,
    );
    out.write_all(line.as_bytes()).unwrap();
}

const HEADER: &str = "use super::{EncoderTrait, InstructionData, OperandType};

macro_rules! id {
    ($operation:ident, $op_code:literal, $destination:ident, $source:ident, $encoder:ident) => {{
        InstructionData {
            operation: Operation::$operation,
            op_code: $op_code,
            destination: OperandType::$destination,
            source: OperandType::$source,
            encoder: super::$encoder::encode,
            sizer: super::$encoder::size,
        }
    }};
}

";

fn main() {
    let mut rdr = csv::Reader::from_path("instructions.csv").unwrap();

    let records: Vec<Record> = rdr
        .deserialize::<Record>()
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    let mnemonics: HashSet<String> = records.iter().map(|r| r.mnemonic.clone()).collect();

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
        "}\n\npub fn str_to_operation(s: &str) -> Option<Operation> {\n    Some(match s {\n"
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
