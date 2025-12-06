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
    let mut children = Vec::<RegexNode>::new();
    while more(symbols, pos) && peek(symbols, pos) != Some(RegexSymbol::GroupRight) && peek(symbols, pos) != Some(RegexSymbol::Alternate) {
        let next_factor;
        (pos, next_factor) = parse_factor(symbols, pos)?;

        children.push(next_factor.clone());

        //factor = RegexNode::new(RegexSymbol::Concat, vec![factor, next_factor]);
    }
    if !children.is_empty() {
        factor = RegexNode::new(RegexSymbol::Concat, children);
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
    use super::*;

    #[test]
    fn test_parser() {

        let a = parse("d\\d+g|dog").unwrap();
        println!("{}",  a);
        assert_eq!(true, false);
    }

}
