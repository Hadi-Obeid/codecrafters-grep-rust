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
    Alternate(Vec<RegexSymbol>, Vec<RegexSymbol>),
    MatchGroup(Vec<RegexSymbol>),
    Backreference,
}

impl RegexSymbol {
    fn from_pattern(pattern: &str) -> Result<Vec<RegexSymbol>, RegexError> {
        //let mut groups = vec![];
        RegexSymbol::from_pattern_(pattern)
    }
    fn from_pattern_<'a>(pattern: &'a str) -> Result<Vec<RegexSymbol>, RegexError> {
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

                '(' => {
                    /* 
                    let mut groups: Vec<Vec<RegexSymbol>> = vec![];
                    println!("Start Regex Group");
                    groups.push(vec![]);

                    let mut group_result = vec![];

                    while let Some(next) = iter.next() {
                        println!("{next}");
                        if next == '(' {
                            println!("Start Regex Group");
                            groups.push(vec![]);
                        } else if next == ')' {
                            println!("Group End");
                            let last = groups.pop().expect("mismatched parentheses");
                            dbg!(&last);
                            dbg!(groups.len());
                            if groups.is_empty() {
                                group_result.extend(last);
                            } else {
                                group_result.push(RegexSymbol::MatchGroup(last));
                            }
                            //result.extend(groups.pop().unwrap());
                        } else {
                            let last =  groups.last_mut().unwrap();
                            last.extend(Self::from_pattern_(next.to_string().as_str()).unwrap());
                        }
                    }
                    dbg!(groups);
                    result.push(RegexSymbol::MatchGroup(group_result));
                    */
                    let mut groups: Vec<Vec<char>> = vec![vec![]];
                    println!("Start Regex Group");

                    let mut group_result = vec![];

                    while let Some(next) = iter.next() {
                        dbg!(&next);
                        if next == '(' {
                            println!("Start Regex Group");
                            groups.push(vec![]);
                        } else if next == ')' {
                            println!("Group End");
                            let last = groups.pop().expect("mismatched parentheses");
                            dbg!(&last);
                            let pattern = Self::from_pattern(last.iter().collect::<String>().as_str()).expect("Invalid Regex pattern");
                            if !groups.is_empty() {
                                group_result.push(RegexSymbol::MatchGroup(pattern));
                            } else {
                                group_result.extend(pattern);

                            }
                            //result.extend(groups.pop().unwrap());
                        } else {
                            if let Some(last) = groups.last_mut() {
                                last.push(next);
                            } else {
                                break;
                            }
                        }
                    }
                    dbg!(&RegexSymbol::MatchGroup(group_result.clone()));
                    result.push(RegexSymbol::MatchGroup(group_result));
                    // Matching parens
                    //dbg!(group_result);
                    
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

                '.' => {
                    result.push(RegexSymbol::Wildcard);
                }

                '|' => {
                    let left = result.clone();
                    let right: Vec<RegexSymbol> = Self::from_pattern(iter.as_str().clone()).expect("Invalid Right side");
                    dbg!(&left);
                    dbg!(&right);
                    result.clear();

                    result.push(RegexSymbol::Alternate(left, right));
                    return Ok(result);
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

fn match_pattern_base(input_line: &[char], pattern: &[RegexSymbol]) -> bool {
    let mut groups = vec![vec![]];
    let v = match pattern.iter().next() {
        None => input_line.is_empty(),
        Some(RegexSymbol::AnchorStart) => match_pattern_recursive(input_line, input_line, &pattern[1..], &mut groups),
        _ => match_pattern_recursive(input_line, input_line, pattern, &mut groups) || (!input_line.is_empty() && match_pattern_base(&input_line[1..], pattern))
    };
    dbg!(groups);
    v
}


fn match_pattern_recursive(input_line: &[char], initial: &[char], pattern: &[RegexSymbol], mut groups: &mut Vec<Vec<char>>) -> bool {
    let current_group: &mut Vec<char> = groups.last_mut().unwrap();
    match pattern.iter().next() {
        None => true,
        // Literal
        Some(RegexSymbol::CharLiteral(c)) => {
            if !input_line.is_empty() && input_line.iter().next() == Some(c) {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..], groups);
            } else {
                false
            }
        },

        Some(RegexSymbol::Digit) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| a.is_digit(10)) {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..],  groups);
            } else {
                false
            }
        }

        Some(RegexSymbol::Alphanumeric) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| a.is_alphanumeric() || *a == '_') {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..], groups);
            } else {
                false
            }
        }

        Some(RegexSymbol::PositiveCharGroup(group)) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| group.contains(&a)) {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..], groups);
            } else {
                false
            }
        }

        Some(RegexSymbol::NegativeCharGroup(group)) => {
            if !input_line.is_empty() && input_line.iter().next().is_some_and(|a| ! group.contains(&a)) {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..],  groups);
            } else {
                false
            }
        }

        Some(RegexSymbol::Plus(symbol)) => {
            let symbol = symbol.as_ref().clone();
            let mut line = &input_line[0..];

            let mut matched_once = false;

            while !line.is_empty() {
                line = &line[1..];
                let attempt = match_pattern_recursive(line, initial, &pattern[1..],  groups);
                if attempt {
                    matched_once = true;
                    break;
                }
            }

            if matched_once {
                true
            } else {
                false
            }


            /*
            if !input_line.is_empty() {
                return match_pattern_recursive(&input_line[1..], &mut [symbol.clone()]);            
            } else {
                false
            }
            */

        }

        Some(RegexSymbol::Question(symbol)) => {
            let symbol = symbol.as_ref().clone();

            if match_pattern_recursive(&[input_line[0]], initial, &mut[symbol], &mut groups) {
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..], &mut groups);
            } else {
                return match_pattern_recursive(input_line, initial, &pattern[1..], &mut groups);

            }


        }

        Some(RegexSymbol::Wildcard) => {
            if !input_line.is_empty() {
                current_group.push(input_line[0]);
                return match_pattern_recursive(&input_line[1..], initial, &pattern[1..], &mut groups);
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

        Some(RegexSymbol::Alternate(ref left, ref right)) => {
            return match_pattern_base(input_line, left.as_slice()) || match_pattern_base(input_line, right.as_slice());
        }

        Some(RegexSymbol::MatchGroup(ref group)) => {
            if match_pattern_recursive(input_line, initial, group, &mut groups) {

                true
            } else {
                false
            }

        },
        _ => {
            panic!("Pattern not supported");
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


        let pattern = RegexSymbol::from_pattern(r"((a)bc)").unwrap();
        let group = RegexSymbol::MatchGroup(vec![RegexSymbol::MatchGroup(vec![RegexSymbol::CharLiteral('a')]), RegexSymbol::CharLiteral('b'), RegexSymbol::CharLiteral('c')]);
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern(r"(abc)").unwrap();
        let group = RegexSymbol::MatchGroup(vec![RegexSymbol::CharLiteral('a'), RegexSymbol::CharLiteral('b'), RegexSymbol::CharLiteral('c')]);
        assert_eq!(pattern, vec![group]);

        let pattern = RegexSymbol::from_pattern(r"(cat|dog)").unwrap();
        let group = RegexSymbol::MatchGroup(vec![
            RegexSymbol::Alternate(
                vec![RegexSymbol::CharLiteral('c'),RegexSymbol::CharLiteral('a'), RegexSymbol::CharLiteral('t')], 
                vec![RegexSymbol::CharLiteral('d'),RegexSymbol::CharLiteral('o'), RegexSymbol::CharLiteral('g')], 
)]);
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

        assert!(match_pattern("dog", "d.g"));

        assert!(match_pattern("goøö0Ogol", "g.+gol"));

        assert!(! match_pattern("gol", "g.+gol"));

        assert!(match_pattern("act", "ca?t"));

        assert!(match_pattern("dog", "cat|dog"));

        assert!(match_pattern("cat", "cat|dog"));
        assert!(! match_pattern("doca", "cat|dog"));

        assert!(match_pattern("bardog", "(foo|bar)"));
        assert!(match_pattern("cat", "(cat|dog|mouse)"));
        assert!(match_pattern("dog", "(cat|dog|mouse)"));
        assert!(match_pattern("mouse", "(cat|dog|mouse)"));

        assert!(match_pattern("I see 1 cat, 2 dogs and 3 cows", r"^I see (\d (cat|dog|cow)s?(, | and )?)+$"));


    }

}
