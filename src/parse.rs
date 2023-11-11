use std::{hash::Hash, fmt::Debug};

use crate::{
	automaton::{Ahead, RuleList, Token},
	showshort,
	table::{Ope, TableSet, TerminalOpe, TerminalTable, NonterminalTable, Tables},
};

#[derive(Debug, Clone)]
pub struct ParseStacks<'a, H, N> {
	pub input: Vec<TokenWithLiteral<'a>>,
	pub stack: Vec<H>,
	pub reduced: Vec<N>,
}

// pub type Stacks<'a> = ParseStacks<'a, u64, usize>;

#[derive(Debug, Clone, Copy)]
pub struct TokenWithLiteral<'a> {
	pub terminal: Ahead<'a>,
	pub literal: &'a str,
}

impl<'a> From<&'a str> for TokenWithLiteral<'a> {
	fn from(value: &'a str) -> Self {
		Self {
			terminal: Ahead::S(value),
			literal: value,
		}
	}
}

impl<'a> ToString for TokenWithLiteral<'a> {
	fn to_string(&self) -> String {
		let t = self.terminal.to_string();
		format!("{}:{}", t, self.literal)
	}
}

pub fn parselr<'a,H>(
	input: Vec<TokenWithLiteral<'a>>,
	initialid: H,
	table: &'a Tables<'a,H,usize>,
	rulelist: &'a RuleList,
) -> ParseStacks<'a,H,usize> 
where
	H: Hash + Eq + Debug + Clone
{
	let mut s = ParseStacks::new(input);
	s.start_parse(initialid, table, rulelist);
	s.clone()
}

impl<'a,H,N> ParseStacks<'a,H,N> 
{
	// fn read(&mut self, table: &TableSet) -> Option<Ope>
	// {
	// }
	
	fn new(input: Vec<TokenWithLiteral<'a>>) -> Self {
		Self {
			input,
			stack: vec![],
			reduced: vec![],
		}
	}
}

impl<'a,H,N> ParseStacks<'a,H,N>
where 
	H: Debug,
	N: Debug
{
	fn print(&self, hideliteral: bool) {
		let input = self.input.iter().map(|t| 
			if hideliteral {
				t.terminal.to_string()
			} else {
				
				t.to_string()
			}
		).collect::<Vec<_>>();
		showshort!(input);
		showshort!(self.stack);
		showshort!(self.reduced);
	}
}


impl<'a,H,> ParseStacks<'a,H,usize>
where 
	H: Hash + Eq + Debug + Clone,

{
	fn start_parse
	(&mut self, initialid: H, table: &'a Tables<'a,H,usize>, rulelist: &'a RuleList) {
		self.input.push(TokenWithLiteral {
			terminal: Ahead::End,
			literal: "$",
		});
		self.input.reverse();
		self.stack.push(initialid);
		self.parser(table, rulelist);
	}

	
	
	fn parser(&mut self, table: &'a Tables<'a,H,usize>, rulelist: &'a RuleList) 
	// where 
		// H: Hash + Eq
	{
		//読み取り
		println!("parser()");
		self.print(true);
		if let Some(top) = self.input.last() {
			let got = || {
				let k = self.stack.last()?;
				table.terminal.get(k)?.get(&top.terminal)
			};
			let mut acc = false;
			if let Some(ope) = got() {
				match ope {
					TerminalOpe::Acc => {
						println!("accepted!");
						acc = true;
					}
					TerminalOpe::Shift(s) => {
						self.stack.push(s.clone());
					}
					TerminalOpe::Reduce(s) => {
						self.reduced.push(s.clone());
						if let Some(rule) = rulelist.at(&s) {
							for _ in 0..rule.right.len() {
								self.stack.pop();
							}
							
							let got = || {
								let k = self.stack.last()?;
								table.nonterminal.get(&k)?.get(rule.left.as_str())
							};
							
							if let Some(id) = got() {
								self.stack.push(id.clone());
							}
						} else {
							panic!("err: reduced id ");
						}
					}
				}
			} else {
				showshort!(self.stack);
				showshort!(top.terminal);
				panic!("table.terminal.get(self.stack.last)?.get(&top.terminal) returned None");
			}
			if !acc {
				self.input.pop();
				self.parser(table, rulelist);
			}
		}
	}
}
