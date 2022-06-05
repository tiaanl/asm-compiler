mod ast;
mod lexer;
mod parser;

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

// fn _print_lexer_error(source: &str, err: &LexerError, path: Option<&str>) {
//     let prev_new_line = if let Some(found) = source[..err.pos].rfind('\n') {
//         found + 1
//     } else {
//         0
//     };
//
//     let next_new_line = if let Some(found) = source[err.pos..].find('\n') {
//         err.pos + found
//     } else {
//         source.len()
//     };
//
//     let fragment = &source[prev_new_line..next_new_line];
//
//     let line = source[0..err.pos].matches('\n').count() + 1;
//     let column = err.pos - prev_new_line;
//
//     if let Some(path) = path {
//         println!("{}:{}:{}", path, line, column + 1);
//     }
//
//     println!("Error: {}", err.kind);
//     println!();
//
//     println!("{}", fragment);
//     for _ in 0..column {
//         print!(" ");
//     }
//     println!("^");
// }

#[derive(ClapParser)]
struct Args {
    source: String,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.source).unwrap();
    let source = source.as_str();

    if let Err(err) = parser::parse(source) {
        match err {
            parser::ParserError::Expected(pos, err) => {
                eprintln!("Error: {}", err);
                print_source_pos(source, pos, Some("samples/snake.asm"));
            }
        }
    }
}
