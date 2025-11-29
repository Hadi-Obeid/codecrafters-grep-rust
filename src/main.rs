use std::collections::HashSet;
use std::fmt;
use std::env;
use std::io;
use std::process;

#[derive(thiserror::Error, Debug)]
enum RegexError {
    #[error("Invalid Regex syntax")]
    RegexSyntaxError,
    #[error("Unsupported Escape sequence")]
    UnsupportedSequence,
}


#[derive(Debug, Clone, PartialEq)]
enum RegexSymbol {
    // Special symbols
    Start,
    End,
    Concat,

    CharLiteral(char),
    Digit,
    Alphanumeric,
    PositiveCharGroup(HashSet<char>),
    NegativeCharGroup(HashSet<char>),
    AnchorStart,
    AnchorEnd,
    // (char: symbol to match, i32: count(for range operator)))
    Plus,
    Question,
    Wildcard,
    Alternate,
    GroupLeft,
    GroupRight,
    Backreference,
}

impl fmt::Display for RegexSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Wildcard => write!(f, "."),

            Self::Start => write!(f, "START"),
            Self::End => write!(f, "END"),
            Self::Concat => write!(f, "CONCAT"),
            Self::CharLiteral(c) => write!(f, "{}", c),
            Self::Digit => write!(f, "\\d"),
            Self::Alphanumeric => write!(f, " "),
            Self::PositiveCharGroup(set) => write!(f, " "),
            Self::NegativeCharGroup(set) => write!(f, " "),
            Self::AnchorStart => write!(f, " "),
            Self::AnchorEnd => write!(f, " "),

            Self::Plus => write!(f, " "),
            Self::Question => write!(f, " "),
            Self::Wildcard => write!(f, " "),
            Self::Alternate => write!(f, " "),
            Self::GroupLeft => write!(f, " "),
            Self::GroupRight => write!(f, " "),
            Self::Backreference => write!(f, " "),

            _ => write!(f, "")
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RegexNodeChildren {
    Single(Option<usize>),
    Double([Option<usize>; 2]),
}

// Represents node in Regex Tree, children are indices in a 
#[derive(Debug, Clone)]
struct RegexNode {
    id: Option<usize>,
    symbol:  RegexSymbol,
    children: Option<RegexNodeChildren>,
}


impl RegexNode {
    fn new(symbol: RegexSymbol) -> Self{
        RegexNode {
            id: None,
            symbol,
            children: None,
        }
    }
}

#[derive(Debug, Clone)]
struct RegexTree {
    nodes: Vec<RegexNode>,
    root: Option<usize>,
}

impl RegexTree {
    fn new() -> Self {
        RegexTree { nodes: vec![], root: None }
    }

    fn add_node(&mut self, node: RegexNode, children: [Option<RegexNode>; 2]) {
        // Add node to tree
        let mut n = node.clone();
        let mut i = self.nodes.len();
        n.id = Some(i);

        match self.root {
            None => { self.root = Some(i) }
            Some(_) => {}
        }

        i += 1; 
        self.nodes.push(n);


        match children.len() {

            1 => {

            }

            2 => {}

            // Either zero children or too many
            _ => {}
        }

    }
}

impl fmt::Display for RegexTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.root {
            Some(root) => {
                write!(f, "")
            },
            None => write!(f, "(Empty Tree)")
        }
    }
}

impl RegexSymbol {

    fn from_pattern(pattern: &str) -> Result<Vec<RegexSymbol>, RegexError> {
        let mut iter = pattern.chars();
        let mut result = vec![];
        while let Some(symbol) = iter.next() {
            match symbol {
                '[' => {
                    let mut letter_group = vec![];
                    let mut start: Option<char> = None;
                    let mut is_negative = false;
                    let mut escape = false;
                    while let Some(next) = iter.next() {
                        if next == ']' {
                            if is_negative {
                                result.push(RegexSymbol::NegativeCharGroup(letter_group.into_iter().collect()));
                            } else {
                                result.push(RegexSymbol::PositiveCharGroup(letter_group.into_iter().collect()));
                            }
                            break;
                        }

                        if next == '\\' && !escape {
                            escape = true;
                            continue;
                        }

                        if next == '-' && !escape {
                            start = letter_group.pop();
                        } else if next == '^' && !escape {
                            is_negative = true;
                        } else {
                            letter_group.push(next);
                            if let Some(start_char) = start {
                                let end_char = next;
                                let chars: Vec<char> = (start_char..=end_char).collect();
                                letter_group.extend(chars);
                                start = None;
                            }
                            escape = false;
                        }

                    }

                }

                '\\' => {
                    if let Some(next) = iter.next() {
                        match next {
                            'd' => {
                                result.push(RegexSymbol::Digit);
                            }

                            'w' => {
                                result.push(RegexSymbol::Alphanumeric);
                            }

                            '\\' => {
                                result.push(RegexSymbol::CharLiteral('\\'));
                            }

                            _=> {
                                return Err(RegexError::UnsupportedSequence);
                            },

                        }
                    } else {
                        return Err(RegexError::RegexSyntaxError);
                    }
                    
                }

                '^' => {
                    result.push(RegexSymbol::AnchorStart);
                }

                '$' => {
                    result.push(RegexSymbol::AnchorEnd);
                }

                '+' => {
                    result.push(RegexSymbol::Plus);
                }

                '?' => {
                    result.push(RegexSymbol::Question);
                }

                '.' => {
                    result.push(RegexSymbol::Wildcard);
                }

                '(' => {
                    result.push(RegexSymbol::GroupLeft);
                }

                ')' => {
                    result.push(RegexSymbol::GroupRight);
                }

                _ => {
                    result.push(RegexSymbol::CharLiteral(symbol));
                }
            }
        }
        Ok(result)
    }
}


fn match_pattern(input_line: &str, pattern: &str) -> bool {
    return false;
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{RegexSymbol, RegexTree, match_pattern};

    #[test]
    fn test_matches() {

    }
#[test]
    fn test_pattern() {
        assert_eq!(RegexSymbol::from_pattern("abc").unwrap(), vec![RegexSymbol::CharLiteral('a'),RegexSymbol::CharLiteral('b'),RegexSymbol::CharLiteral('c')]);
        assert_eq!(RegexSymbol::from_pattern("\\dbc").unwrap(), vec![RegexSymbol::Digit,RegexSymbol::CharLiteral('b'),RegexSymbol::CharLiteral('c')]);
        assert_eq!(RegexSymbol::from_pattern("\\d\\w\\d").unwrap(), vec![RegexSymbol::Digit,RegexSymbol::Alphanumeric, RegexSymbol::Digit]);

        assert_eq!(RegexSymbol::from_pattern("^").unwrap(), vec![RegexSymbol::AnchorStart]);
    }

    #[test]
    fn test_regex_groups() {
        let pattern = RegexSymbol::from_pattern("[a-df]").unwrap();
        let group = RegexSymbol::PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'f'].into_iter().collect());
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern("[ga-df]").unwrap();
        let group = RegexSymbol::PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'f', 'g'].into_iter().collect());
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern("[Ga-dl-oz]").unwrap();
        let group = RegexSymbol::PositiveCharGroup(vec!['a', 'b', 'c', 'd', 'l', 'm', 'n', 'o', 'z', 'G'].into_iter().collect());
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern("[^a-df]").unwrap();
        let group = RegexSymbol::NegativeCharGroup(vec!['a', 'b', 'c', 'd', 'f'].into_iter().collect());
        assert_eq!(pattern, vec![group]);

        // Test escape character
        let pattern = RegexSymbol::from_pattern(r"[\^]").unwrap();
        let group = RegexSymbol::PositiveCharGroup(vec!['^'].into_iter().collect());
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern(r"[^\^\\\-]").unwrap();
        let group = RegexSymbol::NegativeCharGroup(vec!['^', '\\', '-'].into_iter().collect());
        assert_eq!(pattern, vec![group]);


        let pattern = RegexSymbol::from_pattern(r"((a)bc)").unwrap();
        let group = vec![RegexSymbol::GroupLeft, RegexSymbol::GroupLeft, RegexSymbol::CharLiteral('a'), RegexSymbol::GroupRight, RegexSymbol::CharLiteral('b'), RegexSymbol::CharLiteral('c'), RegexSymbol::GroupRight];
        assert_eq!(pattern, group);
    }

    #[test]
    fn test_tree() {
        let mut tree = RegexTree::new();
        assert_eq!(format!("{}", tree), "(Empty Tree)");

        tree.add_node(RegexNode::new(RegexSymbol::Wildcard), [None, None]);
        assert_eq!(format!("{}", tree), ".");
    }




}
