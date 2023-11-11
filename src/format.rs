use std::{
	collections::HashMap,
	fmt::{Debug, Display},
	hash::Hash,
};

use tabled::{builder::Builder, Table, Tabled};

use crate::{
	automaton::{Ahead, RuleList},
	misc::{fill_empty_key_hashmap, hashmap_deep_map, unions_hashmap},
	show,
	table::{NonterminalTable, TerminalOpe, TerminalTable, TerminalTableRow, TT, NonterminalTableRow}, showshort,
};

pub fn show_row<H, N>(row: &TerminalTableRow<H, N>) -> String
where
	H: ToString,
	N: ToString,
{
	let i = Table::new(
		row
			.into_iter()
			.map(|(ah, ope)| (ah.to_string(), ope.to_string())),
	);
	i.to_string()
	// let s = Table::new(terminal_table.iter()).to_string();
}

pub fn show_table<H,N>(
	tt: &TerminalTable<H, N>,
	nt: &NonterminalTable<H>,
	rulelist: &RuleList,
) -> String
where
	H: ToString + Tabled + Hash + Eq + Clone + Debug + Ord,
	N: ToString
	// N: ToString + Display,
{
	// let (iddict, tt) = simplify_terminal_table(tt);
	let mut terminals: Vec<_> = rulelist.terminals().into_iter().map(|s| s.to_string()).collect();
	terminals.push(Ahead::End.to_string());
	let mut nonterminals: Vec<_> = rulelist.nonterminals().into_iter().map(|s| s.to_string()).collect();
	
	terminals.sort();
	nonterminals.sort();
	
	
	let tokens = [
		terminals.clone(),
		nonterminals.clone(),
	];
	let header = tokens.iter().flatten().map(|s| s.to_string());
	
	let mut builder = Builder::default();
	builder.set_header(["id\\token".to_string()].into_iter().chain(header) );
	
	let ttstr: HashMap<_, HashMap<String, String>> =
		HashMap::from_iter(tt.into_iter().map(|(h, row)| {
			(
				h,
				fill_empty_key_hashmap(
					"".to_string(),
					{
						let allkey = terminals.iter().map(|s| s.to_string()).collect::<Vec<_>>();
						allkey
					},
					{
						&HashMap::from_iter(row.into_iter().map(|(ah, ope)| {
							(
								ah.to_string(),
								ope.to_string(), // TerminalOpe::to_string(&ope)
							)
						}))
					},
				),
			)
		}));
	
	// let get_iddict = |key| {
	// 	*iddict.get(key).unwrap_or(&(10000 as usize))
	// } ;
	
	// let ntstr = HashMap::from_iter(
	// 	nt.into_iter().map(|(i, row)| (i
	
	// 	))
	// )
	let ntstr: HashMap<_, HashMap<String, String>> =
	nt.into_iter()
		.map(|(h, row)| {
			(
				// iddict.get(h).unwrap_or(&(10000 as usize)).clone(),
				h,
				fill_empty_key_hashmap(
					"".to_string(), 
					nonterminals.iter().map(|s| s.to_string()).collect(),
				&row
					.into_iter()
					.map(|(non, n)| (non.to_string(), n.to_string() ))
					.collect::<HashMap<String,String>>(),
				)
			)
		})
		.collect()
	;

	let mut rows: Vec<(_, Vec<(String,String)>)> = Vec::new();
	for (u, row) in ttstr.into_iter() {
		let mut v = row.into_iter().collect::<Vec<_>>();
		v.sort_by_key(|(k,v)| k.to_string());
		if let Some(nonrow) = ntstr.get(&u) {
			let mut pairs = nonrow.into_iter().collect::<Vec<_>>();
			pairs.sort_by_key(|(non, u)| non.to_string() );
			v.extend(pairs.into_iter().map(|(non,i)| (non.clone(), i.clone()) ));
		}
		// showshort!(v);
		rows.push((u, v));
	}

	// let maps = [
	// 	ttstr,
	// 	ntstr
	// 	// hashmap_deep_map(nt, {|_,_,n: usize| n.to_string() })
	// ];
	// let mixed = unions_hashmap(&maps);
	
	// let mut rows = mixed.into_iter().collect::<Vec<_>>();
	rows.sort_by_key(|(i,_)| *i);
	
	for (id, row) in rows.into_iter() {
		let is = id.to_string();
		let row = [is]
			.into_iter()
			.chain(row.into_iter().map(|(non, s)| s))
			.collect::<Vec<_>>();
		// showshort!(row);
		builder.push_record(row);
	}

	builder.build().to_string()
}

fn get_or_increment<K>(key: &K, h: &mut HashMap<K, usize>) -> usize
where
	K: Hash + Eq + Clone,
{
	let l = h.len();
	let &mut k = h.entry(key.clone()).or_insert(l);
	k
}

pub fn simplify_tables<'a,H,N>(
	tt: &'a TerminalTable<H, N>,
	nt: &'a NonterminalTable<H>
) -> (HashMap<H, usize>,
	HashMap<usize, HashMap<Ahead<'a>, TerminalOpe<usize, N>>>
, HashMap<usize, NonterminalTableRow<'a,usize>>)
where
	H: Hash+Eq+Clone+Debug,
	N: Clone
{
	let (iddict, tt) = simplify_terminal_table(tt);
	let dict = iddict.clone();
	let get_iddict = |key| {
		*dict.get(key).unwrap_or(&(10000 as usize))
	} ;
	
	(iddict, tt, 
	nt.into_iter().map(|(k,v)| (
		get_iddict(k),
		v.into_iter().map(|(non, h)| (*non, get_iddict(h))).collect()
	)).collect()
	)
}

pub fn simplify_terminal_table<'a, H, N>(
	tt: &'a TerminalTable<H, N>,
	// nt: &NonterminalTable<N>,
	// rulelist: &RuleList
) -> (HashMap<H, usize>,
	HashMap<usize, HashMap<Ahead<'a>, TerminalOpe<usize, N>>>)
where
	H: Hash + Eq + Clone + Debug,
	N: Clone
{
	let mut simplified_item_id_dict: HashMap<H, usize> = HashMap::new();
	let mut table: HashMap<usize, HashMap<Ahead<'_>, TerminalOpe<usize, N>>> = HashMap::new();
	
	for (h, row) in tt.into_iter() {
		let hkey = get_or_increment(h, &mut simplified_item_id_dict);
		
		let row: HashMap<Ahead<'_>, TerminalOpe<usize, N>, _> =
			HashMap::from_iter(row.into_iter().map(|(&terminal, ope)| {
				let o = match ope {
					TerminalOpe::Shift(h) => {
						TerminalOpe::Shift(get_or_increment(h, &mut simplified_item_id_dict))
					}
					TerminalOpe::Acc => TerminalOpe::Acc,
					TerminalOpe::Reduce(s) => TerminalOpe::Reduce(s.clone()),
				};
				(terminal, o)
			}));
		
		table.insert(hkey, row);
	}
	
	show!(simplified_item_id_dict);
	
	(simplified_item_id_dict, table)
}

// pub fn simplify_nonterminal_table<'a,N>(
// 	nt: &NonterminalTable<N>
// )
// where
// 	N: Hash+Eq
// {
// 	let mut ids = HashMap::new();
// 	let mut table = HashMap::new();

// 	for (n, row) in nt.into_iter() {
// 		let key = get_or_increment(n, &mut ids);
// 		let row = HashMap::from_iter(
// 			row.into_iter().map(|(non, n)| (*non,

// 			))
// 		)
// 	}
// }
