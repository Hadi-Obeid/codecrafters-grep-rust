use std::collections::HashSet;
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
    CharLiteral(char),
    Digit,
    Alphanumeric,
    PositiveCharGroup(HashSet<char>),
    NegativeCharGroup(HashSet<char>),
    AnchorStart,
    AnchorEnd,
    Plus(char),
    Star(char),
    Wildcard,
    Alternate,
    MatchGroup,
    Backreference,
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

                            _=> {
                                return Err(RegexError::UnsupportedSequence);
                            },

                        }
                    } else {
                        return Err(RegexError::RegexSyntaxError);
                    }
                    
                }
                _ => {
                    result.push(RegexSymbol::CharLiteral(symbol));
                }
            }
        }
        Ok(result)
    }
}


fn match_pattern(input_line: &str, pattern: &[RegexSymbol]) -> bool {
    match pattern.iter().next() {
        None => input_line.is_empty(),
        Some(RegexSymbol::AnchorStart) => match_pattern_recursive(input_line, &pattern[1..]),
        _ => match_pattern_recursive(input_line, pattern) || (!input_line.is_empty() && match_pattern(&input_line[1..], pattern))
    }
}

fn consume_char(line: &str, n: usize) -> &str {
    &line[n..]
}

fn match_pattern_recursive(input_line: &str, pattern: &[RegexSymbol]) -> bool {
    match pattern.iter().next() {
        None => true,
        // Literal
        Some(RegexSymbol::CharLiteral(c)) => {
            if !input_line.is_empty() && input_line.chars().next() == Some(*c) {
                return match_pattern_recursive(&input_line[1..], &pattern[1..]);
            } else {
                false
            }
        },

        Some(RegexSymbol::Digit) => {
            if !input_line.is_empty() && input_line.chars().next().is_some_and(|a| a.is_digit(10)) {
                return match_pattern_recursive(&input_line[1..], &pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::Alphanumeric) => {
            if !input_line.is_empty() && input_line.chars().next().is_some_and(|a| a.is_alphanumeric()) {
                return match_pattern_recursive(&input_line[1..], &pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::PositiveCharGroup(group)) => {
            if !input_line.is_empty() && input_line.chars().next().is_some_and(|a| group.contains(&a)) {
                return match_pattern_recursive(&input_line[1..], &pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::NegativeCharGroup(group)) => {
            if !input_line.is_empty() && input_line.chars().next().is_some_and(|a| ! group.contains(&a)) {
                return match_pattern_recursive(&input_line[1..], &pattern[1..]);
            } else {
                false
            }
        }
        _ => {
            panic!("Pattern not supported");
            false
        },
    }
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    eprintln!("Logs from your program will appear here!");

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = RegexSymbol::from_pattern(&env::args().nth(2).unwrap()).expect("Invalid Pattern");
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

    use crate::{match_pattern, RegexSymbol};

    #[test]
    fn test_pattern() {
        assert_eq!(RegexSymbol::from_pattern("abc").unwrap(), vec![RegexSymbol::CharLiteral('a'),RegexSymbol::CharLiteral('b'),RegexSymbol::CharLiteral('c')]);
        assert_eq!(RegexSymbol::from_pattern("\\dbc").unwrap(), vec![RegexSymbol::Digit,RegexSymbol::CharLiteral('b'),RegexSymbol::CharLiteral('c')]);
        assert_eq!(RegexSymbol::from_pattern("\\d\\w\\d").unwrap(), vec![RegexSymbol::Digit,RegexSymbol::Alphanumeric, RegexSymbol::Digit]);
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
    }

    #[test]
    fn test_match_basic_concatenation() {
        assert!(match_pattern("hello world", &RegexSymbol::from_pattern("hello").unwrap()));
        assert!(match_pattern("hello world", &RegexSymbol::from_pattern(" world").unwrap()));

        // NOT MATCH
        assert!(! match_pattern("12world", &RegexSymbol::from_pattern(" world").unwrap()));

        assert!(match_pattern("359 world", &RegexSymbol::from_pattern(r"\d\d\d world").unwrap()));

        // NOT MATCH
        assert!(! match_pattern("444 expect 4 digits", &RegexSymbol::from_pattern(r"\d\d\d\d world").unwrap()));

        assert!(match_pattern(" cad", &RegexSymbol::from_pattern(r"\w\w\w").unwrap()));

        // NOT MATCH
        assert!(! match_pattern("  c.d", &RegexSymbol::from_pattern(r"\w\w\w").unwrap()));


        assert!(match_pattern("a", &RegexSymbol::from_pattern(r"[abc]").unwrap()));

        // NOT MATCH
        assert!(! match_pattern("a", &RegexSymbol::from_pattern(r"[^abc]").unwrap()));
    }

}
