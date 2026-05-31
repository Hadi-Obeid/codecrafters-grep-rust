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
                _ = write!(f," ");
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


#[derive(Debug, Clone)]
enum RegexInstruction {
    Operation(RegexSymbol), // Regex commands

    Match, // Return true
    Jmp(usize), // Jmp PC
    Split(usize, usize), // Split
}

#[derive(Debug, Clone, Default)]
pub struct RegexVM {
    instructions: Vec<RegexInstruction>,
}

impl RegexVM {
    pub fn new(root: &RegexNode) -> Self {
        let mut instructions = vec![];
        RegexVM::instructions_from_tree(root, &mut instructions);
        instructions.push(RegexInstruction::Match);
        RegexVM { instructions }
    }

    pub fn match_regex(&self, source: &[char], pc: usize, pos: usize) -> bool {
        match &self.instructions[pc] {
            RegexInstruction::Match => { return true },

            RegexInstruction::Operation(operation) => {
                if pos >= source.len() { return false; }
                match operation {
                    RegexSymbol::CharLiteral(c) => {
                        if source[pos] == *c {
                            return RegexVM::match_regex(&self, source, pc + 1, pos + 1);
                        } else {
                            return false;
                        }
                    }
                    _ => { panic!("Invalid operation"); }
                }
                /*
                if source[pos] == c {
                    return RegexVM::match_regex(&self, source, pc + 1, pos + 1);
                } else {
                    return false;
                }
                */
            },


            RegexInstruction::Split(a, b) => {
                if RegexVM::match_regex(&self, source, *a, pos) {
                    return true;
                } else {
                    return RegexVM::match_regex(&self, source, *b, pos);
                }
            }

            RegexInstruction::Jmp(a) => {return RegexVM::match_regex(&self, source, *a, pos); },
            //_ => { return false; },

        }
    }

    pub fn instructions_from_tree(root: &RegexNode, instructions: &mut Vec<RegexInstruction>) {
        match &root.symbol {


            RegexSymbol::Concat => {
                RegexVM::instructions_from_tree(&root.children[0], instructions);
                RegexVM::instructions_from_tree(&root.children[1], instructions);
            }

            // TODO: add match group support
            RegexSymbol::MatchGroup => {
                RegexVM::instructions_from_tree(&root.children[0], instructions);
            }

            RegexSymbol::Alternate => {
                let split = instructions.len();
                instructions.push(RegexInstruction::Split(0, 0));

                let l1 = instructions.len();
                RegexVM::instructions_from_tree(&root.children[0], instructions);
                
                let jmp = instructions.len();
                instructions.push(RegexInstruction::Jmp(0));

                let l2 = instructions.len();
                RegexVM::instructions_from_tree(&root.children[1], instructions);
                let l3 = instructions.len();

                instructions[split] = RegexInstruction::Split(l1, l2);
                instructions[jmp] = RegexInstruction::Jmp(l3);
            }

            RegexSymbol::Plus => {
                let l1 = instructions.len();
                RegexVM::instructions_from_tree(&root.children[0], instructions);
                let l3 = instructions.len() + 1;
                instructions.push(RegexInstruction::Split(l1, l3));
            }

            RegexSymbol::Star => {
                let l1 = instructions.len();
                let l2 = l1 + 1;
                instructions.push(RegexInstruction::Split(l2, l1));
                RegexVM::instructions_from_tree(&root.children[0], instructions);
                instructions.push(RegexInstruction::Jmp(l1));
                let l3 = instructions.len(); // After operation
                instructions[l1] = RegexInstruction::Split(l2, l3);
            }

            operation => {
                instructions.push(RegexInstruction::Operation(operation.clone()));
            }
        }
    }
}

#[cfg(test)]
pub mod test {
    use crate::{node::*, parser::parse};

    /*
    #[test]
    fn test_vm() {
        // Parse expression
        let root = parse("abcd");
        //dbg!(format!("{}", &root.unwrap()));
        let mut instructions = vec![];
        RegexVM::instructions_from_tree(&root.unwrap(), &mut instructions);
        instructions.push(RegexInstruction::Match);

        for (i, instruction) in instructions.iter().enumerate() {
            match instruction {
                RegexInstruction::Char(c) => {
                    println!("{i} char {c}");
                },

                RegexInstruction::Split(a,b) => {
                    println!("{i} split {a}, {b}");
                },

                RegexInstruction::Jmp(a) => {
                    println!("{i} jmp {a}");
                }

                RegexInstruction::Match => {
                    println!("match");
                }
            }
        }
    }
    */

    #[test]
    fn test_match() {
        let root = parse("(h(e|3)llo|world)+").unwrap();
        let input: Vec<char> = "hEllo".chars().collect::<Vec<char>>();
        let vm = RegexVM::new(&root);
        assert_eq!(true, vm.match_regex(input.as_slice(), 0, 0));
        
    }
}

