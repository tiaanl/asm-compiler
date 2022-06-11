mod ast;
mod compiler;
mod encoder;
mod lexer;
mod parser;

use crate::compiler::Compiler;
use crate::lexer::Span;
use clap::Parser as ClapParser;

fn print_source_pos(source: &str, span: &Span, path: Option<&str>) {
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
    for _ in span.start..span.end {
        print!("^");
    }
    println!();
}

#[derive(ClapParser, Debug)]
struct Args {
    source: String,

    #[clap(short, long)]
    syntax_only: bool,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.source).unwrap();
    let source = source.as_str();

    let lines = match parser::parse(source) {
        Ok(lines) => lines,
        Err(err) => {
            match err {
                parser::ParserError::Expected(pos, err) => {
                    eprintln!("Error: {}", err);
                    print_source_pos(source, &(pos..pos + 2), Some(args.source.as_str()));
                }
            }

            return;
        }
    };

    if args.syntax_only {
        for line in &lines {
            println!("{}", line);
        }
    } else {
        let mut compiler = Compiler::new(lines);
        match compiler.compile() {
            Ok(_) => println!("DONE"),
            Err(err) => {
                eprintln!("{}", err);
                print_source_pos(source, err.span(), Some(args.source.as_str()))
            }
        }
    }
}
