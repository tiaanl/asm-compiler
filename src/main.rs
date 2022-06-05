mod ast;
mod compiler;
mod lexer;
mod parser;

use crate::compiler::Compiler;
use clap::Parser as ClapParser;

fn print_source_pos(source: &str, pos: usize, path: Option<&str>) {
    let prev_new_line = if let Some(found) = source[..pos].rfind('\n') {
        found + 1
    } else {
        0
    };

    let next_new_line = if let Some(found) = source[pos..].find('\n') {
        pos + found
    } else {
        source.len()
    };

    let fragment = &source[prev_new_line..next_new_line];

    let line = source[0..pos].matches('\n').count() + 1;
    let column = pos - prev_new_line;

    if let Some(path) = path {
        println!("{}:{}:{}", path, line, column + 1);
    }

    println!("{}", fragment);
    for _ in 0..column {
        print!(" ");
    }
    println!("^");
}

#[derive(ClapParser)]
struct Args {
    source: String,
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
                    print_source_pos(source, pos, Some(args.source.as_str()));
                }
            }

            return;
        }
    };

    let mut compiler = Compiler::new(lines);
    match compiler.compile() {
        Ok(_) => println!("DONE"),
        Err(err) => eprintln!("{:?}", err),
    }
}
