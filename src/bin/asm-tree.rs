use asm_compiler::{ast, parse, ParserError};
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
    parse_only: bool,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.source).unwrap();
    let source = source.as_str();

    if let Err(err) = parse(source, &mut |line| {
        if args.parse_only {
            let do_args = |operand: &ast::Operand| match operand {
                ast::Operand::Immediate(span, _)
                | ast::Operand::Address(span, _, _, _)
                | ast::Operand::Register(span, _)
                | ast::Operand::Segment(span, _) => {
                    print_source_pos(source, span, Some(args.source.as_str()))
                }
            };
            match &line {
                ast::Line::Instruction(span, instruction) => {
                    print_source_pos(source, span, Some(args.source.as_str()));
                    match &instruction.operands {
                        ast::Operands::None(span) => {
                            print_source_pos(source, span, Some(args.source.as_str()))
                        }
                        ast::Operands::Destination(span, destination) => {
                            print_source_pos(source, span, Some(args.source.as_str()));
                            do_args(destination);
                        }
                        ast::Operands::DestinationAndSource(span, destination, source_) => {
                            print_source_pos(source, span, Some(args.source.as_str()));
                            do_args(destination);
                            do_args(source_);
                        }
                    }
                }
                ast::Line::Label(span, _)
                | ast::Line::Data(span, _)
                | ast::Line::Constant(span, _) => {
                    print_source_pos(source, span, Some(args.source.as_str()))
                }
                ast::Line::Times(_) => {}
            }
        } else {
            println!("{}", &line);
        }
    }) {
        match &err {
            ParserError::Expected(span, err) => {
                eprintln!("Error: {}", err);
                print_source_pos(source, span, Some(args.source.as_str()));
            }
            _ => panic!("another error"),
        }
    }
}
