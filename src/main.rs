mod ast;
mod compiler;
mod encoder;
mod instructions;
mod lexer;
mod parser;

use crate::ast::{Line, Operand, Operands};
use crate::compiler::Compiler;
use crate::parser::LineConsumer;
use clap::Parser as ClapParser;

fn print_source_pos(source: &str, span: &ast::Span, path: Option<&str>) {
    let prev_new_line = if let Some(found) = source[..span.start].rfind('\n') {
        found + 1
    } else {
        0
    };

    let next_new_line = if let Some(found) = source[span.start..].find('\n') {
        span.start + found
    } else {
        source.len()
    };

    let fragment = &source[prev_new_line..next_new_line];

    let line = source[0..span.start].matches('\n').count() + 1;
    let column = span.start - prev_new_line;

    if let Some(path) = path {
        println!("{}:{}:{}", path, line, column + 1);
    }

    println!("{}", fragment);
    for _ in 0..column {
        print!(" ");
    }
    let end = if span.start == span.end {
        span.end + 1
    } else {
        span.end
    };
    for _ in span.start..end {
        print!("^");
    }
    println!();
}

#[derive(ClapParser, Debug)]
struct Args {
    source: String,

    #[clap(short, long)]
    syntax_only: bool,

    #[clap(short, long)]
    parse_only: bool,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.source).unwrap();
    let source = source.as_str();

    let mut compiler = Compiler::default();

    if let Err(err) = parser::parse(source, &mut |line| {
        if args.syntax_only {
            println!("{}", &line);
        } else if args.parse_only {
            let do_args = |operand: &Operand| match operand {
                Operand::Immediate(span, _)
                | Operand::Address(span, _, _, _)
                | Operand::Register(span, _)
                | Operand::Segment(span, _) => {
                    print_source_pos(source, span, Some(args.source.as_str()))
                }
            };
            match &line {
                Line::Instruction(span, instruction) => {
                    print_source_pos(source, span, Some(args.source.as_str()));
                    match &instruction.operands {
                        Operands::None(span) => {
                            print_source_pos(source, span, Some(args.source.as_str()))
                        }
                        Operands::Destination(span, destination) => {
                            print_source_pos(source, span, Some(args.source.as_str()));
                            do_args(destination);
                        }
                        Operands::DestinationAndSource(span, destination, source_) => {
                            print_source_pos(source, span, Some(args.source.as_str()));
                            do_args(destination);
                            do_args(source_);
                        }
                    }
                }
                Line::Label(span, _) | Line::Data(span, _) | Line::Constant(span, _) => {
                    print_source_pos(source, span, Some(args.source.as_str()))
                }
                Line::Times(_) => {}
            }
        } else {
            compiler.consume(line)
        }
    }) {
        match &err {
            parser::ParserError::Expected(span, err) => {
                eprintln!("Error: {}", err);
                print_source_pos(source, span, Some(args.source.as_str()));
            }
            _ => panic!("another error"),
        }

        return;
    }

    if args.syntax_only {
        return;
    }

    match compiler.compile() {
        Ok(_) => {
            println!("DONE");
        }
        Err(err) => {
            eprintln!("{}", err);
            print_source_pos(source, err.span(), Some(args.source.as_str()))
        }
    }
}
