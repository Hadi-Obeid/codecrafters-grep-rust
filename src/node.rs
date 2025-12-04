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

pub fn match_node(input: &str, root: Option<&RegexNode>) -> (bool, Vec<String>) {
    let mut matches = Vec::new();

    if let Some(root) = root {
        if root.symbol == RegexSymbol::Empty {
            return (false, vec![]);
        }
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
                i = i + matched.len();
                if !matched.is_empty() {
                    //print!("{} ", &matched);
                    matches.push(matched);
                }
            } else {
                i += 1;
            }
        }

        //println!();

        (has_matched, matches)
    } else {
        (false, matches)
    }

}

fn match_node_(input: &[char], root: &RegexNode, pos: usize, re_match: &mut String) -> (bool, usize) {
    if (pos > input.len() - 1) && root.symbol != RegexSymbol::AnchorEnd {
        (false, pos)
    } else {
        match &root.symbol {
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
                if input[pos] == *c {
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

            RegexSymbol::Empty => {
                (true, pos)
            }

            RegexSymbol::MatchGroup =>  {
                match_node_(input,  &root.children[0], pos, re_match)
            }

            RegexSymbol::Wildcard => {
                if pos < input.len() {
                    re_match.push(input[pos]);
                    (true, pos + 1)
                } else {
                    (false, pos)
                }
            }

            RegexSymbol::AnchorStart => {
                if pos == 0 {
                    (true, pos)
                } else {
                    (false, pos)
                }
            }

            RegexSymbol::AnchorEnd => {
                if pos >= input.len() {
                    (true, pos)
                } else {
                    (false, pos)
                }
            }

            RegexSymbol::PositiveCharGroup(set) => {
                if set.contains(&input[pos]) {
                    (true, pos + 1)
                } else {
                    (false, pos)
                }
            }

            RegexSymbol::NegativeCharGroup(set) => {
                if !set.contains(&input[pos]) {
                    (true, pos + 1)
                } else {
                    (false, pos)
                }
            }

            _ => (false, pos)
        }
    }
}


#[cfg(test)]
mod test {
    use super::RegexSymbol::*;
    use super::*;

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
        assert_eq!(match_node("c", Some(&t0)).0, true);
        assert_eq!(match_node("asldfalsjkdfc", Some(&t0)).0, true);
        assert_eq!(match_node("a", Some(&t0)).0, false);
        assert_eq!(match_node("laskdflsajdla", Some(&t0)).0, false);

        assert_eq!(match_node("1", Some(&t1)).0, true); 
        assert_eq!(match_node("ccclaskdflsajdla", Some(&t1)).0, false);

        let t2 = RegexNode::new(Concat, vec![t0, t1]);
        assert_eq!(match_node("d1", Some(&t2)).0, false); 
        assert_eq!(match_node("cc", Some(&t2)).0, false); 
        assert_eq!(match_node("c1", Some(&t2)).0, true); 
        assert_eq!(match_node("asldfjlaksc1", Some(&t2)).0, true); 

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
        assert_eq!(match_node("cat", Some(&cat)).0, true); 
        assert_eq!(match_node("dog", Some(&cat)).0, false); 
        assert_eq!(match_node("dogcat", Some(&cat)).0, true); 
        assert_eq!(match_node("cardogca", Some(&cat)).0, false); 

        let cat_and_dog = RegexNode::new(Concat, vec![cat.clone(), dog.clone()]);
        let cat_or_dog = RegexNode::new(Alternate, vec![cat.clone(), dog.clone()]);
        assert_eq!(match_node("chickendogcatfoo", Some(&cat_and_dog)).0, false); 
        assert_eq!(match_node("chickencatdogfoo", Some(&cat_and_dog)).0, true); 
        assert_eq!(match_node("cat", Some(&cat_or_dog)).0, true); 
        assert_eq!(match_node("dog", Some(&cat_or_dog)).0, true); 
        assert_eq!(match_node("dogcat", Some(&cat_or_dog)).0, true); 
        assert_eq!(match_node("doca", Some(&cat_or_dog)).0, false); 


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

        assert_eq!(match_node("b", Some(&star)).0, true);
        assert_eq!(match_node("aaaaaaabbbaabaaa", Some(&star)).0, true);

        assert_eq!(match_node("b", Some(&plus)).0, false);
        assert_eq!(match_node("aaaaaaab", Some(&plus)).0, false);
        assert_eq!(match_node("aaa11aaaab11111", Some(&plus)).0, true);
        assert_eq!(match_node("aaa111aaaab1", Some(&plus)).0, true);

        assert_eq!(match_node("ct", Some(&question_test_tree)).0, true);
        assert_eq!(match_node("cat", Some(&question_test_tree)).0, true);
        assert_eq!(match_node("caat", Some(&question_test_tree)).0, false);
        //println!("{}", &question_test_tree);
        assert_eq!(match_node("ctctcatdogcotcaatctctcat", Some(&question_test_tree)).0, true);
        //assert_eq!(match_node("ab", Some(&question)), true);
    }
}
