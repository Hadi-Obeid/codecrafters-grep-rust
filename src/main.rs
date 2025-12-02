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
    MatchGroup,
    Star,
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
            Self::Alphanumeric => write!(f, "\\w"),
            Self::PositiveCharGroup(set) => write!(f, "{:?}", set),
            Self::NegativeCharGroup(set) => write!(f, " "),
            Self::AnchorStart => write!(f, " "),
            Self::AnchorEnd => write!(f, " "),
            Self::Star => write!(f, "*"),
            Self::Plus => write!(f, "+"),
            Self::Question => write!(f, "?"),
            Self::Alternate => write!(f, "|"),
            Self::GroupLeft => write!(f, "("),
            Self::GroupRight => write!(f, ")"),
            Self::Backreference => write!(f, " "),
            Self::MatchGroup => write!(f, "GROUP"),

            _ => write!(f, "")
        }
    }
}


#[derive(Debug, Clone)]
struct RegexNode {
    symbol:  RegexSymbol,
    children: Vec<RegexNode>,
}


impl RegexNode {
    fn new(symbol: RegexSymbol, children: Vec<RegexNode>) -> Self{
        RegexNode {
            symbol,
            children: children,
        }
    }

    fn print(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.children.len() == 0 {
            write!(f, "{}", self.symbol)
        } else {
            let _ = write!(f, "({}", self.symbol);
            for child in &self.children {
                write!(f," ");
                let _ = child.print(f);
            }
            write!(f, ")")
        }
    }

}
    


impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.print(f)
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

                '*' => {
                    result.push(RegexSymbol::Star);
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

fn match_node(input: &str, root: Option<&RegexNode>) -> bool {
    if let Some(root) = root {
        let input_vec = input.chars().collect::<Vec<char>>();
        let chars = input_vec.as_slice();

        let mut has_matched = false;

        let mut i = 0;

        while i < chars.len() {
            let mut matched = String::new();
            let (is_match, pos) = match_node_(chars, root, i, &mut matched);

            if is_match {
                has_matched = true;
            }

            if is_match && matched.len() > 0 {
                dbg!(i);
                dbg!(matched.len());
                i = i + matched.len();
                if !matched.is_empty() {
                    print!("{} ", matched);
                }
            } else {
                i += 1;
            }
        }

        println!();

        has_matched
    } else {
        false
    }

}

fn match_node_(input: &[char], root: &RegexNode, pos: usize, re_match: &mut String) -> (bool, usize) {
    if (pos > input.len() - 1) && root.symbol != RegexSymbol::AnchorEnd {
        (false, pos)
    } else {
        match root.symbol {
            RegexSymbol::Concat => {
                // Match left side and get position after end of pattern
                let (match_exists, match_index) = match_node_(input, &root.children[0], pos, re_match);
                // Only succeed whole match if right side is successful
                if match_exists {
                    match_node_(input,  &root.children[1], match_index, re_match)
                } else {
                    (false, pos)
                }
            }

            RegexSymbol::Star => {
                let mut is_match: bool;
                let mut index: usize;
                (is_match, index) = match_node_(input, &root.children[0], pos, re_match);
                while is_match {
                    (is_match, index) = match_node_(input, &root.children[0], index, re_match);
                }
                (true, index)
            }

            RegexSymbol::Plus => {
                let mut is_match: bool;
                let mut index: usize;
                (is_match, index) = match_node_(input, &root.children[0], pos, re_match);
                let matched_once = is_match;
                while is_match {
                    (is_match, index) = match_node_(input, &root.children[0], index, re_match);
                }
                (matched_once, index)
            }

            RegexSymbol::Question => {
                let index: usize;
                (_, index) = match_node_(input, &root.children[0], pos, re_match);
                (true, index)
            }


            RegexSymbol::Alternate => {
                let (match_exists, match_index) = match_node_(input, &root.children[0], pos, re_match);
                if !match_exists {
                    match_node_(input,  &root.children[1], pos, re_match)
                } else {
                    (match_exists, match_index)
                }
            }
            RegexSymbol::CharLiteral(c) => {
                if input[pos] == c {
                    re_match.push(input[pos]);
                    (true, pos + 1)

                } else {
                    (false, pos)
                }
            }
            RegexSymbol::Digit => {
                if input[pos].is_digit(10) {
                    re_match.push(input[pos]);
                    (true, pos + 1)

                } else {
                    (false, pos)
                }
            }

            RegexSymbol::Wildcard => {
                if pos < input.len() {
                    re_match.push(input[pos]);
                    (true, pos + 1)
                } else {
                    (false, pos)
                }
            }

            _ => (false, pos)
        }
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

    use crate::{RegexSymbol, RegexSymbol::*, RegexNode, match_node};

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
        let tree = RegexNode::new(RegexSymbol::Wildcard, vec![]);
        assert_eq!(format!("{}", tree), ".");
        let character = RegexNode::new(RegexSymbol::CharLiteral('a'), vec![]);
        let tree2 = RegexNode::new(RegexSymbol::Concat, vec![tree, character]);
        let tree3 = RegexNode::new(RegexSymbol::Concat, vec![tree2, RegexNode::new(RegexSymbol::Digit, vec![])]);

        //tree.add_node(RegexNode::new(RegexSymbol::Wildcard), [None, None]);

        assert_eq!(format!("{}", tree3), "(CONCAT (CONCAT . a) \\d)");
    }

    #[test]
    fn test_eval() {
        let t0 = RegexNode::new(CharLiteral('c'), vec![]);
        let t1 = RegexNode::new(Digit, vec![]);
        assert_eq!(match_node("c", Some(&t0)), true);
        assert_eq!(match_node("asldfalsjkdfc", Some(&t0)), true);
        assert_eq!(match_node("a", Some(&t0)), false);
        assert_eq!(match_node("laskdflsajdla", Some(&t0)), false);

        assert_eq!(match_node("1", Some(&t1)), true); 
        assert_eq!(match_node("ccclaskdflsajdla", Some(&t1)), false);

        let t2 = RegexNode::new(Concat, vec![t0, t1]);
        assert_eq!(match_node("d1", Some(&t2)), false); 
        assert_eq!(match_node("cc", Some(&t2)), false); 
        assert_eq!(match_node("c1", Some(&t2)), true); 
        assert_eq!(match_node("asldfjlaksc1", Some(&t2)), true); 

    }

    #[test]
    fn test_eval_alt() {
        let cat = RegexNode::new(Concat, 
            vec![
                RegexNode::new(CharLiteral('c'), vec![]),
                RegexNode::new(Concat, vec![
                    RegexNode::new(CharLiteral('a'), vec![]),
                    RegexNode::new(CharLiteral('t'), vec![])
                ])
            ]
        );

        let dog = RegexNode::new(Concat, 
            vec![
                RegexNode::new(CharLiteral('d'), vec![]),
                RegexNode::new(Concat, vec![
                    RegexNode::new(CharLiteral('o'), vec![]),
                    RegexNode::new(CharLiteral('g'), vec![])
                ])
            ]
        );

        assert_eq!(format!("{}", cat), "(CONCAT c (CONCAT a t))");
        assert_eq!(match_node("cat", Some(&cat)), true); 
        assert_eq!(match_node("dog", Some(&cat)), false); 
        assert_eq!(match_node("dogcat", Some(&cat)), true); 
        assert_eq!(match_node("cardogca", Some(&cat)), false); 

        let cat_and_dog = RegexNode::new(Concat, vec![cat.clone(), dog.clone()]);
        let cat_or_dog = RegexNode::new(Alternate, vec![cat.clone(), dog.clone()]);
        assert_eq!(match_node("chickendogcatfoo", Some(&cat_and_dog)), false); 
        assert_eq!(match_node("chickencatdogfoo", Some(&cat_and_dog)), true); 
        assert_eq!(match_node("cat", Some(&cat_or_dog)), true); 
        assert_eq!(match_node("dog", Some(&cat_or_dog)), true); 
        assert_eq!(match_node("dogcat", Some(&cat_or_dog)), true); 
        assert_eq!(match_node("doca", Some(&cat_or_dog)), false); 


    }

    #[test]
    fn test_quantifiers() {
        let star = RegexNode::new(Star, vec![RegexNode::new(CharLiteral('a'), vec![])]);
        let plus = RegexNode::new(Plus, vec![RegexNode::new(Digit, vec![])]);

        let question = RegexNode::new(Question, vec![RegexNode::new(CharLiteral('a'), vec![])]);
        let question_test_tree= 
            RegexNode::new(Concat, 
        vec![
                        RegexNode::new(Concat, vec![
                            RegexNode::new(CharLiteral('c'), vec![]),
                            question.clone()
                        ]),
                        RegexNode::new(CharLiteral('t'), vec![])
                 ]);

        assert_eq!(match_node("b", Some(&star)), true);
        assert_eq!(match_node("aaaaaaabbbaabaaa", Some(&star)), true);

        assert_eq!(match_node("b", Some(&plus)), false);
        assert_eq!(match_node("aaaaaaab", Some(&plus)), false);
        assert_eq!(match_node("aaa11aaaab11111", Some(&plus)), true);
        assert_eq!(match_node("aaa111aaaab1", Some(&plus)), true);

        assert_eq!(match_node("ct", Some(&question_test_tree)), true);
        assert_eq!(match_node("cat", Some(&question_test_tree)), true);
        assert_eq!(match_node("caat", Some(&question_test_tree)), false);
        println!("{}", &question_test_tree);
        assert_eq!(match_node("ctctcatdogcotcaatctctcat", Some(&question_test_tree)), true);
        //assert_eq!(match_node("ab", Some(&question)), true);
    }




}
