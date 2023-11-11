use cstree::testing::GreenNodeBuilder;
use cstree::{RawSyntaxKind, Syntax};
use ptree::item::StringItem;
use ptree::{print_tree, TreeBuilder};
use syntree::{Error, Node, Tree};

use crate::automaton::{RuleList, Token};
use crate::parse::TokenWithLiteral;
use crate::show;


pub fn parsed_to_ptree<'a>(
	input: &mut Vec<TokenWithLiteral<'a>>,
	reduced: &mut Vec<usize>,
	rulelist: &'a RuleList,
)-> StringItem 
{
	let mut pt = TreeBuilder::new("ROOT".to_string());
	ptree_build(&mut pt, input, reduced, rulelist);
	// let tree = 
	pt.build()
}

pub fn print_parse_tree<'a>(
	input: &mut Vec<TokenWithLiteral<'a>>,
	reduced: &mut Vec<usize>,
	rulelist: &'a RuleList,
) {
	match reduced_to_parse_tree(input, reduced, rulelist) {
		 Ok(synt) =>
		match syntree_to_ptree(synt) {
			Ok(s) => {
				print_tree(&s);
			}
			Err(e) => {
				println!("{}", e);
			}
		}
		,Err(e) => {
			println!("couldnt reduced_to_parse_tree()");
			show!(e);
		}
	}
}

fn syntree_to_ptree<'a>(synt: Tree<&'a str, u32, usize>) -> Result<StringItem, &str> {
	if let Some(node) = synt.first() {
		let mut pt = TreeBuilder::new("★".to_string());
		syntree_node_to_ptree(node, &mut pt);
		Ok(pt.build())
	} else {
		Err("syntree was empty")
	}
}

fn syntree_node_to_ptree<'a, T>(node: Node<'a, T, u32, usize>, pt: &mut TreeBuilder)
where
	T: ToString,
{
	if node.has_children() {
		pt.begin_child(node.value().to_string());
		node.children().for_each(|c| {
			syntree_node_to_ptree(c, pt);
		});
		pt.end_child();
	} else {
		pt.add_empty_child(node.value().to_string());
	}
}

pub fn reduced_to_parse_tree<'a>(
	input: &mut Vec<TokenWithLiteral<'a>>,
	reduced: &mut Vec<usize>,
	rulelist: &'a RuleList,
) -> Result<Tree<&'a str, u32, usize>, Error> {
	let mut tree = syntree::Builder::new();
	if let Err(mess) = syntree_build(&mut tree, input, reduced, rulelist) {
		// println!("{}", mess);
		show!(mess);
	} else {
	}
	tree.build()
}

fn syntree_build<'a>(
	tree: &mut syntree::Builder<&'a str, u32, usize>,
	input: &mut Vec<TokenWithLiteral<'a>>,
	reduced: &mut Vec<usize>,
	rulelist: &'a RuleList,
) -> Result<(), &'a str> {
	if let Some(id) = reduced.pop() {
		let rule = rulelist.at(&id).unwrap();
		
		if let Err(erropenleft) = tree.open(&rule.left) {
			show!(erropenleft);
		}
		
		let mut right = rule.right.clone();
		right.reverse();
		right.into_iter().for_each(|t| {
			match t {
				Token::Terminal(s) => {
					if let Some(i) = input.pop() {
						// let term = format!("{}({})", s, i.literal);
						if let Err(errtoken) = tree.token(s, i.literal.len()) {
							show!(errtoken);
						}
					}
				}
				Token::Nonterminal(s) => {
					if let Err(erropen) = tree.open(s) {
						show!(erropen);
					}
					if let Err(nonterminalerr) =
					syntree_build(tree, input, reduced, rulelist) {
						show!(nonterminalerr);
					}
				}
			}
		});
		
		if let Err(errclose) = tree.close() {
			show!(errclose);
		}
		Ok(())
	} else {
		Err("reduce.pop() returned None")
	}
}

fn ptree_build<'a>(
	// tree: &mut syntree::Builder<&'a str, u32, usize>,
	pt: &mut TreeBuilder,
	input: &mut Vec<TokenWithLiteral<'a>>,
	reduced: &mut Vec<usize>,
	rulelist: &'a RuleList,
)
//  -> Result<(), &'a str> 
{
	if let Some(id) = reduced.pop() {
		let rule = rulelist.at(&id).unwrap();
		
		
		if rule.right.len() > 0 {
		
		pt.begin_child(rule.format_as_tree_node());
		
		let mut right = rule.right.clone();
		right.reverse();
		right.into_iter().for_each(|t| {
			match t {
				Token::Terminal(s) => {
					if let Some(i) = input.pop() {
						let term = format!("{}({})", s, i.literal);
						pt.add_empty_child(term);
					}
				}
				Token::Nonterminal(s) => {
					// pt.begin_child(s.to_string());
					ptree_build(pt, input, reduced, rulelist);
				}
			}
		});
		
		pt.end_child();
		
		} else {
			pt.add_empty_child(rule.left.clone());
		}
	} else {
		println!("reduce.pop() returned None");
	}
}

// #[derive(Debug, Eq, PartialEq, Clone, Copy,)]
// pub struct CSyntax<'a>(&'a str);

// impl Syntax for CSyntax<'a> {
// 	fn from_raw(raw: RawSyntaxKind) -> Self {
		
// 	}
// }


// fn reduced_to_cstree<'a>(
// 	input: &mut Vec<TokenWithLiteral<'a>>,
// 	reduced: &mut Vec<usize>,
// 	rulelist: &'a RuleList,
// )

// {
	
// }