use std::env;
use std::io;
use std::process;


mod symbol;
use symbol::*;

use crate::node::RegexNode;
use crate::node::RegexVM;
use crate::parser::parse;

mod node;

mod parser;


fn match_pattern(line: &str, pattern: &str) -> bool {
    let root: RegexNode = parse(pattern).unwrap();
    let vm: RegexVM = RegexVM::new(&root);
    let chars: Vec<char> = line.chars().collect();
    vm.match_regex(chars.as_slice(), 0, 0)
}


// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    eprintln!("Logs from your program will appear here!");

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        println!("match");
        process::exit(0)
    } else {
        println!("No match");
        process::exit(1)
    }
}
