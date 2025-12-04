use std::env;
use std::io;
use std::process;


mod symbol;
use symbol::*;

mod node;
use node::{RegexNode, match_node};

mod parser;
use parser::parse;



//use node::*;



fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let root = parse(pattern).unwrap();
    return match_node(input_line, Some(&root)).0;
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

    process::exit(0);
    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        println!("match");
        process::exit(0)
    } else {
        println!("No match");
        process::exit(1)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::RegexSymbol::*;
    use crate::{RegexSymbol};

    #[test]
    fn test_matches() {

    }

    #[test]
    fn test_pattern() {
        assert_eq!(RegexSymbol::from_pattern("abc").unwrap(), vec![Start, CharLiteral('a'),CharLiteral('b'),CharLiteral('c'), End]);
        assert_eq!(RegexSymbol::from_pattern("\\dbc").unwrap(), vec![Start, Digit, CharLiteral('b'), CharLiteral('c'), End]);
        assert_eq!(RegexSymbol::from_pattern("\\d\\w\\d").unwrap(), vec![Start, Digit,Alphanumeric, Digit, End]);

        assert_eq!(RegexSymbol::from_pattern("^").unwrap(), vec![Start, AnchorStart, End]);
    }

    #[test]
    fn test_regex_groups() {
        let pattern = RegexSymbol::from_pattern("[a-df]").unwrap();
        let group = PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'f'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);

        let pattern = RegexSymbol::from_pattern("[ga-df]").unwrap();
        let group = PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'f', 'g'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);

        let pattern = RegexSymbol::from_pattern("[Ga-dl-oz]").unwrap();
        let group = PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'l', 'm', 'n', 'o', 'z', 'G'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);

        let pattern = RegexSymbol::from_pattern("[^a-df]").unwrap();
        let group = NegativeCharGroup(vec!['a', 'b', 'c', 'd', 'f'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);

        // Test escape character
        let pattern = RegexSymbol::from_pattern(r"[\^]").unwrap();
        let group = PositiveCharGroup(vec!['^'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);

        let pattern = RegexSymbol::from_pattern(r"[^\^\\\-]").unwrap();
        let group = NegativeCharGroup(vec!['^', '\\', '-'].into_iter().collect());
        assert_eq!(pattern, vec![Start, group, End]);


        let pattern = RegexSymbol::from_pattern(r"((a)bc)").unwrap();
        let group = vec![Start, GroupLeft, GroupLeft, CharLiteral('a'), GroupRight, CharLiteral('b'), CharLiteral('c'), GroupRight, End];
        assert_eq!(pattern, group);
    }


}
