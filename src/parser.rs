use std::collections::VecDeque;

use anyhow::Error;
use anyhow::anyhow;

use crate::{RegexSymbol, RegexNode};

fn more(symbols: &[RegexSymbol], pos: usize) -> bool {
    return symbols.get(pos) != Some(&RegexSymbol::End)
}

fn peek(symbols: &[RegexSymbol], pos: usize) -> Option<RegexSymbol> {
    symbols.get(pos).cloned()
}

fn is_quantifier(symbol: &RegexSymbol) -> bool {
    match symbol {
        RegexSymbol::Star => true,
        RegexSymbol::Plus => true,
        RegexSymbol::Question => true,
        _ => false,
    }
}

fn eat(symbols: &[RegexSymbol], expected: RegexSymbol, pos: &mut usize) -> Result<(), Error> {
    if peek(&symbols, *pos) == Some(expected.clone()) {
        *pos += 1;
        Ok(())
    } else {
        Err(anyhow!("Expected token {}", expected))
    }
}


fn parse_re(symbols: &[RegexSymbol], mut pos: usize) -> Result<(usize, RegexNode), Error> {
    let left;
    (pos, left) = parse_term(symbols, pos)?;
    if more(symbols, pos) && peek(symbols, pos) == Some(RegexSymbol::Alternate) {
        let right;
        eat(symbols, RegexSymbol::Alternate, &mut pos)?;
        (pos, right) = parse_re(symbols, pos)?;
        return Ok((pos, RegexNode::new(RegexSymbol::Alternate, vec![left, right])));
    }

    Ok((pos, left))
}

fn parse_term(symbols:  &[RegexSymbol], mut pos: usize) -> Result<(usize, RegexNode), Error> {
    
    let mut factor = RegexNode::new(RegexSymbol::Empty, vec![]);
    while more(symbols, pos) && peek(symbols, pos) != Some(RegexSymbol::GroupRight) && peek(symbols, pos) != Some(RegexSymbol::Alternate) {
        let next_factor;
        (pos, next_factor) = parse_factor(symbols, pos)?;

        factor = RegexNode::new(RegexSymbol::Concat, vec![factor, next_factor]);
    }
    return Ok((pos, factor));
}

fn parse_factor(symbols: &[RegexSymbol], mut pos: usize) ->  Result<(usize, RegexNode), Error> {
    let mut base;
    (pos, base) = parse_base(symbols, pos)?;
    while more(symbols, pos) && peek(symbols, pos) != Some(RegexSymbol::Alternate) {
        if let Some(symbol) = peek(symbols, pos) {
            if is_quantifier(&symbol) {
                eat(symbols, symbol.clone(), &mut pos)?;
                return Ok((pos, RegexNode::new(symbol, vec![base])));
            } else {
                break;
            }
        }
    }
    return Ok((pos, base));


}

fn parse_base(symbols: &[RegexSymbol], mut pos: usize) ->  Result<(usize, RegexNode), Error> {
    match peek(&symbols, pos) {
        Some(RegexSymbol::GroupLeft) => { 
            let re;
            eat(&symbols, RegexSymbol::GroupLeft, &mut pos)?;
            (pos, re) = parse_re(symbols, pos)?;
            eat(&symbols, RegexSymbol::GroupRight, &mut pos)?;
            Ok((pos, RegexNode::new(RegexSymbol::MatchGroup, vec![re])))
        }

        Some(symbol) => {
            eat(&symbols, symbol.clone(), &mut pos)?;
            Ok((pos, RegexNode::new_leaf(symbol)))
        }

        None => Err(anyhow!("Parser error"))
    }


}



pub fn parse(input: &str) -> Result<RegexNode, Error> {
    let symbols = RegexSymbol::from_pattern(input)?;
    let result = parse_re(&symbols[0..], 1)?;
    Ok(result.1)
}

#[cfg(test)]
mod test {
    use crate::node::match_node;

    use super::*;

    #[test]
    fn test_parse_base() {
        let root = parse("a").unwrap();
        assert_eq!(match_node("a", Some(&root)).0, true);
        assert_eq!(match_node("b", Some(&root)).0, false);
        assert_eq!(match_node("bbbba", Some(&root)).0, true);
        assert_eq!(format!("{}", root), "(CONCAT EMPTY a)");
    }

    #[test]
    fn test_parse_concat() {
        let root = parse("cats").unwrap();
        assert_eq!(match_node("dogs", Some(&root)).0, false);
        assert_eq!(match_node("cats", Some(&root)).0, true);
    }

    #[test]
    fn test_parse_alternate() {
        let root = parse("a|b").unwrap();
        assert_eq!(format!("{}", root), "(| (CONCAT EMPTY a) (CONCAT EMPTY b))");
        assert_eq!(match_node("a", Some(&root)).0, true);
        assert_eq!(match_node("b", Some(&root)).0, true);
        assert_eq!(match_node("c", Some(&root)).0, false);

        let root = parse("foo|bar").unwrap();
        assert_eq!(match_node("foo", Some(&root)).0, true);
        assert_eq!(match_node("bar", Some(&root)).0, true);
        assert_eq!(match_node("foobarbarfoo", Some(&root)).1, vec!["foo", "bar", "bar", "foo"]);
        assert_eq!(match_node("baz", Some(&root)).0, false);
    }

    #[test]
    fn test_parse_group() {
        let root = parse("(cat|dog)").unwrap();
        assert_eq!(match_node("catcatdog", Some(&root)).0, true);
        assert_eq!(match_node("lorem ipsum dolor sit amet catcatdog", Some(&root)).1, vec!["cat", "cat", "dog"]);
    }

    #[test]
    fn test_quantifier() {
        let root = parse("(ca+t)").unwrap();
        assert_eq!(match_node("catcaatcaaat", Some(&root)).0, true);
        assert_eq!(match_node("catcaatcaaat", Some(&root)).1, vec!["cat", "caat", "caaat"]);
    }

    #[test]
    fn test_anchors() {
        let root = parse("^hello").unwrap();
        assert_eq!(match_node("hello world", Some(&root)).0, true);
        //assert_eq!(match_node("catcaatcaaat", Some(&root)).1, vec!["cat", "caat", "caaat"]);
    }

    #[test]
    fn test_complex() {
        let root = parse("^I see \\d+ (cat|dog)s?$").unwrap();
        println!("{}", root);
        assert_eq!(match_node("I see 2 dog3", Some(&root)).0, false);
        assert_eq!(match_node("I see 1 cat", Some(&root)).0, true);
        assert_eq!(match_node("I see 1 cats", Some(&root)).0, true);

    }

    #[test]
    fn test_wildcard() {
        let case2 = parse("g.+gol").unwrap();
        assert_eq!(match_node("gggol", Some(&case2)).0, true);
        //assert_eq!(match_node("catcaatcaaat", Some(&root)).1, vec!["cat", "caat", "caaat"]);
    }
}
