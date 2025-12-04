use std::collections::HashSet;
use std::fmt;


#[derive(thiserror::Error, Debug)]
pub enum RegexError {
    #[error("Invalid Regex syntax")]
    RegexSyntaxError,
    #[error("Unsupported Escape sequence")]
    UnsupportedSequence,
}


#[derive(Debug, Clone, PartialEq)]
pub enum RegexSymbol {
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
    Empty,
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
            Self::AnchorStart => write!(f, "^"),
            Self::AnchorEnd => write!(f, "$"),
            Self::Star => write!(f, "*"),
            Self::Plus => write!(f, "+"),
            Self::Question => write!(f, "?"),
            Self::Alternate => write!(f, "|"),
            Self::GroupLeft => write!(f, "("),
            Self::GroupRight => write!(f, ")"),
            Self::Backreference => write!(f, " "),
            Self::MatchGroup => write!(f, "GROUP"),
            Self::Empty => write!(f, "EMPTY"),

            _ => write!(f, "")
        }
    }
}

impl RegexSymbol {

    pub fn from_pattern(pattern: &str) -> Result<Vec<RegexSymbol>, RegexError> {
        let mut iter = pattern.chars();
        let mut result = vec![];
        result.push(RegexSymbol::Start);
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

                '|' => {
                    result.push(RegexSymbol::Alternate);
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
        result.push(RegexSymbol::End);
        Ok(result)
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_alt() {
        let abc = RegexSymbol::from_pattern("a|b").unwrap();
        assert_eq!(abc, vec![RegexSymbol::Start, RegexSymbol::CharLiteral('a'), RegexSymbol::Alternate, RegexSymbol::CharLiteral('b'), RegexSymbol::End]);

        let def = RegexSymbol::from_pattern("\\d").unwrap();
        assert_eq!(def, vec![RegexSymbol::Start, RegexSymbol::Digit, RegexSymbol::End]);
    }
}
