use std::fmt;
use crate::{RegexSymbol, parser::parse};

#[derive(Debug, Clone)]
pub struct RegexNode {
    symbol: RegexSymbol,
    children: Vec<RegexNode>,
}


impl RegexNode {
    pub fn new(symbol: RegexSymbol, children: Vec<RegexNode>) -> Self{
        RegexNode {
            symbol,
            children: children,
        }
    }
    
    // Node without children shorthand
    pub fn new_leaf(symbol: RegexSymbol) -> Self{
        RegexNode {
            symbol,
            children: vec![],
        }
    }

    pub fn print(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.children.len() == 0 {
            write!(f, "{}", self.symbol)
        } else {
            let _ = write!(f, "({}", self.symbol);
            for child in &self.children {
                write!(f," ")?;
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


fn match_pattern(input: &str, regex: &str) -> bool {
    let root = parse(regex);
    let characters = input.chars().collect::<Vec::<char>>();
    let mut candidates = Vec::<String>::new();
    match_base(&characters, 0, &root.unwrap(), &mut candidates)
}

fn match_base(input: &[char], pos: usize, node: &RegexNode, candidates: &mut Vec<String>) -> bool {

    false
}



#[cfg(test)]
mod test {
    use super::RegexSymbol::*;
    use super::*;

    #[test]
    fn test_expression() {
        assert_eq!(match_pattern("hello", "hello"), true);
        assert_eq!(match_pattern("bye", "bye"), true);
    }

}
