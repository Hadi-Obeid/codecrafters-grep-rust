use std::fmt;
use crate::RegexSymbol;

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

fn is_empty(symbol: &RegexSymbol) -> bool {
    match symbol {
        RegexSymbol::AnchorStart => true,
        RegexSymbol::AnchorEnd => true,
        RegexSymbol::Question => true,
        RegexSymbol::Star => true,
        _ =>  false,
    }
}
