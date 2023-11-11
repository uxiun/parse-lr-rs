use std::{collections::HashMap, hash::Hash};

use tabled::Tabled;

use crate::{
	automaton::{Ahead, AutomatonTable, RuleItems, Token, Tokenkey},
	showshort,
};

#[derive(Debug, Tabled)]
pub enum TerminalOpe<H, N> {
	Acc,
	Shift(H),
	Reduce(N),
}

pub type Ope = TerminalOpe<u64, usize>;

pub type TerminalTableRow<'a, H, N> = HashMap<Ahead<'a>, TerminalOpe<H, N>>;
pub type NonterminalTableRow<'a, H> = HashMap<&'a str, H>;
pub type TerminalTable<'a, H, N> = HashMap<H, TerminalTableRow<'a, H, N>>;
pub type NonterminalTable<'a, H> = HashMap<H, NonterminalTableRow<'a, H>>;
pub type TTR<'a> = TerminalTableRow<'a, u64, usize>;
pub type TT<'a> = TerminalTable<'a, u64, usize>;
pub type NTR<'a> = NonterminalTableRow<'a, u64>;
pub type NT<'a> = NonterminalTable<'a, u64>;

pub type KeyItemsMap<'a, H> = HashMap<H, RuleItems<'a>>;
pub type Keymap<'a> = KeyItemsMap<'a, u64>;

impl<H, N> TerminalOpe<H, N> {
	pub fn as_row_key(&self) -> Option<&H> {
		match self {
			Self::Acc => None,
			Self::Reduce(_) => None,
			Self::Shift(h) => Some(h),
		}
	}
}

impl<H, N> ToString for TerminalOpe<H, N>
where
	H: ToString,
	N: ToString,
{
	fn to_string(&self) -> String {
		match self {
			Self::Acc => "Acc".to_string(),
			Self::Reduce(n) => format!("Reduce({})", n.to_string()),
			Self::Shift(n) => format!("Shift({})", n.to_string()),
		}
	}
}

#[derive(Debug)]
pub struct TableSet<'a> {
	pub terminal: TT<'a>,
	pub nonterminal: NT<'a>,
}

#[derive(Debug)]
pub struct Tables<'a, H, N> {
	pub terminal: HashMap<H, TerminalTableRow<'a, H, N>>,
	pub nonterminal: HashMap<H, NonterminalTableRow<'a, H>>,
}

pub fn parse_table<'a>(
	initialid: u64,
	hashgroups: Vec<(u64, RuleItems<'a>)>,
	automatable: AutomatonTable<'a>,
) -> TableSet<'a> {
	let (keymap, mut tableset) = process_automaton(initialid, hashgroups, automatable);
	add_reduce(keymap, &mut tableset.terminal);

	tableset
}

fn add_reduce<'a>(keymap: Keymap<'a>, table: &mut TT<'a>)
// -> TT<'a,u64>
{
	for (id, items) in keymap.into_iter() {
		table
			.entry(id)
			.and_modify(|row| {
				items.0.clone().into_iter().for_each(|item| {
					if let None = item.atcursor() {
						for k in item.ahead.into_iter() {
							row.entry(k).or_insert(TerminalOpe::Reduce(item.rule.id));
						}
					}
				})
			})
			.or_insert(HashMap::from_iter({
				let r = items.0.into_iter().filter_map(|item| {
					if let None = item.atcursor() {
						Some(
							item
								.ahead
								.into_iter()
								.map(|k| (k, TerminalOpe::Reduce(item.rule.id))),
						)
					} else {
						None
					}
				});
				r.flatten()
			}));
	}
}

pub fn process_automaton<'a>(
	initialid: u64,
	hashgroups: Vec<(u64, RuleItems<'a>)>,
	automatable: AutomatonTable<'a>,
) -> (Keymap<'a>, TableSet<'a>) {
	let mut terminal_table: TT<'a> = HashMap::new();
	let mut nonterminal_table: NT<'a> = HashMap::new();
	for (idfrom, rrow) in automatable.into_iter() {
		// let mut accs = vec![];

		for (tokenkey, idto) in rrow.into_iter() {
			match tokenkey {
				Tokenkey::Terminal(ah) => {
					terminal_table
						.entry(idfrom)
						.and_modify(|row| {
							row.insert(ah, TerminalOpe::Shift(idto));
						})
						.or_insert(HashMap::from_iter(
							[(ah, TerminalOpe::Shift(idto))].into_iter(),
						));

					// if let Ahead::End = ah {
					// 	accs.push(idfrom);
					// }
				}
				Tokenkey::Nonterminal(s) => {
					nonterminal_table
						.entry(idfrom)
						.and_modify(|row| {
							row.insert(s, idto);
						})
						.or_insert(HashMap::from_iter([(s, idto)].into_iter()));
				}
			}
		}
		// match accs.as_slice() {
		// 	[idfrom] => {
		// 		terminal_table.insert(*idfrom, HashMap::from_iter(
		// 			[(Ahead::End, TerminalOpe::Acc)]
		// 		));
		// 	},
		// 	_ => {}
		// }
	}

	{

		// let accs = hashgroups
		// 	.iter()
		// 	.filter_map(|(idfrom, items)| {
		// 		if *idfrom == initialid &&
		// 			items.0.iter().any(|item| {

		// 				let i: Vec<_> = item.clone().ahead.into_iter().collect();
		// 				[Ahead::End] == i.as_slice()
		// 			}) {
		// 			Some(idfrom)
		// 		} else {None}
		// 	})
		// 	.collect::<Vec<_>>();
		// showshort!(accs);
		// accs.into_iter().for_each(|id| {
		// 	terminal_table.insert(*id, HashMap::from_iter([(Ahead::End, TerminalOpe::Acc)]));
		// });
	}

	(
		HashMap::from_iter(hashgroups.into_iter()),
		TableSet {
			terminal: terminal_table,
			nonterminal: nonterminal_table,
		},
	)
}
