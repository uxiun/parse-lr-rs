#![feature(slice_group_by)]
use itertools::{GroupBy, Itertools};
use parselr::{
	automaton::{self, all_first, from_raw_rules, from_rules_literal, RuleList},
	format::{show_table, simplify_tables},
	parse::parselr,
	show, showshort,
	table::{parse_table, TableSet, Tables},
	token::tokenize_word,
};
use std::{
	collections::{hash_map::RandomState, HashMap, HashSet},
	hash::{BuildHasher, Hash, Hasher},
	iter::zip,
};

use nonempty::NonEmpty;
// #[cfg(not(debug_assertions))]
macro_rules! dbg {
	($x:expr) => {
		std::convert::identity($x)
	};
}

fn main() {
	println!("Hello, world!");
	let s = vec![
		vec!["e", "n"],
		vec!["n", "n", "*", "n"],
		vec!["n", "n", "+", "n"],
		vec!["n", "1", "2", "3"],
	];
	let s = vec![
		vec!["e", "e", "+", "t"],
		vec!["e", "t"],
		vec!["t", "t", "*", "f"],
		vec!["t", "f"],
		vec!["f", "i"],
		vec!["f", "(", "e", ")"],
	];
	let lr1grammar = vec![
		vec!["S", "A"],
		vec!["A", "E", "=", "E"],
		vec!["A", "Id"],
		vec!["E", "E", "+", "T"],
		vec!["E", "T"],
		vec!["T", "Num"],
		vec!["T", "Id"],
	];
	let parsegrammar = vec![
		vec!["S", "E"],
		vec!["E", "E", "+", "T"],
		vec!["E", "T"],
		vec!["T", "T", "*", "Num"],
		vec!["T", "Num"],
	];
	let rules = from_rules_literal(from_raw_rules(parsegrammar));
	let rulelist = RuleList(&rules);
	let firsts = all_first(&rules);
	show!(&firsts);

	// let follows = follow(&rules, &firsts);

	if let Some(((initialid, hashgroups), table)) = automaton::main(&rules, &rulelist, &firsts) {
		show!(table);
		let tables = parse_table(initialid, hashgroups, table);
		show!(tables);

		let (iddict, tt, nt) = simplify_tables(&tables.terminal, &tables.nonterminal);

		let formatted_terminal_table = show_table(&tt, &nt, &rulelist);
		println!("{}", formatted_terminal_table);

		let tableset = Tables {
			nonterminal: nt,
			terminal: tt,
		};

		let src = " Num +   Num * Num";
		let input = tokenize_word(src);
		
		let initialid = iddict.get(&initialid).unwrap_or(&(1000 as usize));
		let parsed = parselr(input, *initialid, &tableset, &rulelist);
		show!(parsed);
	}

	// let left_history: HashSet<&str> = HashSet::new();
	// if let Some(initial) = RuleItems::initial(&rulelist) {
	// 	// let items = initial.grouping(HashSet::new(), &firsts, &rulelist);
	// 	show!(&initial);

	// 	let grouped = initial.grouping(left_history, &firsts, &rulelist);
	// 	show!(grouped);
	// 	let forwarded = grouped.forward();
	// 	show!(forwarded);

	// 	let random = RandomState::new();

	// 	let ((_, hashpair), table) = RuleItems::derivate(initial, &random, &firsts, &rulelist);
	// 	show!(&table);

	// 	let hashes: Vec<_> = forwarded
	// 		.into_iter()
	// 		.map(|(t, g)| get_hash(&g, &random))
	// 		.collect();
	// 	show!(hashes);
	// } else {
	// 	println!("initial was None");
	// }
}
