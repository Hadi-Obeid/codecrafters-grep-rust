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
    // (char: symbol to match, i32: count(for range operator)))
    Plus(Box<RegexSymbol>),
    Question(Box<RegexSymbol>),
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
                    if let Some(previous) = result.pop() {
                        result.push(RegexSymbol::Plus(Box::new(previous)));
                    }
                }

                '?' => {
                    if let Some(previous) = result.pop() {
                        result.push(RegexSymbol::Question(Box::new(previous)));
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


fn match_pattern(input_line: &str, pattern: &str) -> bool {
    match_pattern_base(&input_line.chars().collect::<Vec<char>>(), &mut RegexSymbol::from_pattern(pattern).expect("Invalid Pattern"))
}

fn match_pattern_base(input_line: &[char], pattern: &mut [RegexSymbol]) -> bool {
    match pattern.iter().next() {
        None => input_line.is_empty(),
        Some(RegexSymbol::AnchorStart) => match_pattern_recursive(input_line, &mut pattern[1..]),
        _ => match_pattern_recursive(input_line, pattern) || (!input_line.is_empty() && match_pattern_base(&input_line[1..], pattern))
    }
}


fn match_pattern_recursive(input_line: &[char], pattern: &mut [RegexSymbol]) -> bool {
    match pattern.iter().next() {
        None => true,
        // Literal
        Some(RegexSymbol::CharLiteral(c)) => {
            if !input_line.is_empty() && input_line.iter().next() == Some(c) {
                return match_pattern_recursive(&input_line[1..], &mut pattern[1..]);
            } else {
                false
            }
        },

        Some(RegexSymbol::Digit) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| a.is_digit(10)) {
                return match_pattern_recursive(&input_line[1..], &mut pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::Alphanumeric) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| a.is_alphanumeric() || *a == '_') {
                return match_pattern_recursive(&input_line[1..], &mut pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::PositiveCharGroup(group)) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| group.contains(&a)) {
                return match_pattern_recursive(&input_line[1..], &mut pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::NegativeCharGroup(group)) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| ! group.contains(&a)) {
                return match_pattern_recursive(&input_line[1..], &mut pattern[1..]);
            } else {
                false
            }
        }

        Some(RegexSymbol::Plus(symbol)) => {
            let symbol = symbol.as_ref().clone();
            let mut count = 0;
            let mut letter = input_line.into_iter();
            while let Some(&letter) = letter.next() {
                if match_pattern_recursive(&[letter], &mut [ symbol.clone() ]) {
                    count += 1;
                } else {
                    break;
                }
                println!("{letter}");
            }

            if !input_line.is_empty() && count >= 1 {
                return match_pattern_recursive(&input_line[count..], &mut pattern[1..]) || match_pattern_recursive(&input_line[count - 1..], &mut pattern[1..]);
            } else {
                false
            }

        }

        Some(RegexSymbol::Question(symbol)) => {
            let symbol = symbol.as_ref().clone();
            let mut count = 0;
            let mut letter = input_line.into_iter();
            while let Some(&letter) = letter.next() {
                if match_pattern_recursive(&[letter], &mut [ symbol.clone() ]) {
                    count += 1;
                } else {
                    break;
                }
                println!("{letter}");
            }

            if !input_line.is_empty() && count <= 1 {
                return match_pattern_recursive(&input_line[count..], &mut pattern[1..]);
            } else {
                false
            }

        }

        Some(RegexSymbol::AnchorEnd) => {
            if !input_line.is_empty() {
                false
            } else {
                true
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

    use crate::{match_pattern, RegexSymbol};

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
    }

    #[test]
    fn test_plus_star() {
        let pattern = RegexSymbol::from_pattern(r"a+").unwrap();
        assert_eq!(pattern, vec![RegexSymbol::Plus(Box::new(RegexSymbol::CharLiteral('a')))]);

        let pattern = RegexSymbol::from_pattern(r"[abc]+").unwrap();
        assert_eq!(pattern, vec![
            RegexSymbol::Plus(
                Box::new(RegexSymbol::PositiveCharGroup(vec!['a', 'b', 'c'].into_iter().collect()))
            )
        ]);
    }

    #[test]
    fn test_match_basic_concatenation() {
        assert!(match_pattern("hello world", "hello"));
        assert!(match_pattern("hello world", " world"));

        // NOT MATCH
        assert!(! match_pattern("12world", " world"));

        assert!(match_pattern("359 world", r"\d\d\d world"));

        // NOT MATCH
        assert!(! match_pattern("444 expect 4 digits", r"\d\d\d\d world"));

        assert!(match_pattern(" cad", r"\w\w\w"));

        // NOT MATCH
        assert!(! match_pattern("  c.d", r"\w\w\w"));


        assert!(match_pattern("a", r"[abc]"));

        // NOT MATCH
        assert!(! match_pattern("a", r"[^abc]"));
    }

    #[test]
    fn match_combinations() {
        assert!(match_pattern("1 apple", r"\d apple"));
        // NOT
        assert!(! match_pattern("1 orange", r"\d apple"));

        assert!(match_pattern("100 apples", r"\d\d\d apple"));
        // NOT
        assert!(! match_pattern("1 apple", r"\d\d\d apple"));

        assert!(match_pattern("3 dogs", r"\d \w\w\ws"));
        assert!(match_pattern("4 cats", r"\d \w\w\ws"));

        // NOT
        assert!(! match_pattern("1 dog", r"\d \w\w\ws"));

        assert!(match_pattern("log", r"^log"));

        assert!(match_pattern("dogdogdog", "dog$"));

        // NOT
        assert!(! match_pattern("dog log", "dog$"));

        assert!(match_pattern("aaaaaa", "a+"));
        assert!(match_pattern("caaats", "ca+ts"));
        assert!(match_pattern("caaats", "ca+at"));

        assert!(match_pattern("dogs", "dogs?"));
        
        assert!(! match_pattern("cats", "dogs?"));

    }

}
