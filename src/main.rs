#![feature(slice_group_by)]
use itertools::{GroupBy, Itertools};
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

macro_rules! show {
	($x:expr) => {
		println!("▶ {} ↓", stringify!($x));
		println!("{:#?}", $x);
		println!("——————————")
	};
}

macro_rules! showshort {
	($x:expr) => {
		println!("▶ {} ↓", stringify!($x));
		println!("{:?}", $x);
		println!("——————————")
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
	let rules = from_rules_literal(from_raw_rules(lr1grammar));
	let grammar = Grammar::from_rules(&rules);
	let rulelist = RuleList(&rules);
	let firsts = all_first(&rules);
	show!(&firsts);

	let follows = follow(&rules, &firsts);
	show!(follows);

	let left_history: HashSet<&str> = HashSet::new();
	if let Some(initial) = RuleItems::initial(&rulelist) {
		// let items = initial.grouping(HashSet::new(), &firsts, &rulelist);
		show!(&initial);

		let grouped = initial.grouping(left_history, &firsts, &rulelist);
		show!(grouped);
		let forwarded = grouped.forward();
		show!(forwarded);

		let random = RandomState::new();

		let ((_, hashpair), table) = RuleItems::derivate(initial, &random, &firsts, &rulelist);
		show!(&table);

		let hashes: Vec<_> = forwarded
			.into_iter()
			.map(|(t, g)| get_hash(&g, &random))
			.collect();
		show!(hashes);
	} else {
		println!("initial was None");
	}
}

fn get_hash<V>(value: &V, random: &RandomState) -> u64
where
	V: Hash,
{
	let mut hasher = random.build_hasher();
	value.hash(&mut hasher);
	hasher.finish()
}

fn groupBy_run<K, V>(i: &[(K, V)]) -> HashMap<&K, Vec<&V>>
where
	K: Eq + Hash,
{
	let mut h = HashMap::new();
	for (key, value) in i.into_iter() {
		h.entry(key)
			.and_modify(|e: &mut Vec<&V>| e.push(value))
			.or_insert(vec![value]);
	}

	h
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Token<'a> {
	Nonterminal(&'a str),
	Terminal(&'a str),
}
impl<'a> Token<'a> {
	fn unwrap(&self) -> String {
		match *self {
			Self::Nonterminal(s) => s.to_string(),
			Self::Terminal(s) => s.to_string(),
		}
	}
}

#[derive(Debug, Clone)]
struct RuleLiteral<'a> {
	left: String,
	right: Vec<&'a str>,
}
impl<'a> RuleLiteral<'a> {
	fn create(value: Vec<&str>) -> Option<RuleLiteral> {
		let (head, tail) = value.split_at(1);
		match head.get(0) {
			None => None,
			Some(h) => Some(RuleLiteral {
				left: h.to_string(),
				right: tail.into_iter().map(|s| s.to_owned()).collect(),
			}),
		}
	}
}

fn from_raw_rules<'a>(rules: Vec<Vec<&'a str>>) -> Vec<RuleLiteral<'a>> {
	rules
		.into_iter()
		.filter_map(|r| RuleLiteral::create(r))
		.collect()
}

#[derive(Debug)]
struct Rule<'a> {
	id: usize,
	left: String,
	right: Vec<Token<'a>>,
}

impl Hash for Rule<'_> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		for token in self.right.iter() {
			let b = match token {
				Token::Nonterminal(t) => t.as_bytes(),
				Token::Terminal(t) => t.as_bytes(),
			};
			state.write(b);
		}
	}
}

type GrammarType<'a> = HashMap<String, Vec<&'a [Token<'a>]>>;
struct Grammar<'a>(GrammarType<'a>);

impl<'a> Grammar<'a> {
	fn from_rules(rules: &'a [Rule<'a>]) -> Self {
		Grammar(from_rules_to_grammar(rules))
	}
}

struct RuleList<'a>(&'a [Rule<'a>]);
// impl <'a> RuleList<'a> {
// 	fn filter_by_left(&self, left: &str) -> Self {
// 		let x =self.0.into_iter().filter(|r| r.left == left).map(|r| *r) ;
// 		RuleList(x.collect())
// 	}
// }

#[derive(Debug, Clone)]
struct RuleItem<'a> {
	rule: &'a Rule<'a>,
	cursor: usize,
	ahead: HashSet<&'a str>,
}

#[derive(Debug, Clone, Hash)]
struct RuleItems<'a>(Vec<RuleItem<'a>>);

#[derive(Debug)]
struct ItemEntry<'a> {
	// id: usize, //同じleftを持つ規則列の序数
	cursor: usize,
	ahead: Vec<&'a str>,
	tokens: &'a [Token<'a>], // == grammar.get(left)[id].right
}

type ItemMap<'a> = HashMap<String, Vec<ItemEntry<'a>>>;

#[derive(Debug)]
struct Items<'a>(ItemMap<'a>);

impl<'a, 'd> RuleItem<'a> {
	fn atcursor(&self) -> Option<&'d Token<'a>> {
		self.rule.right.get(self.cursor)
	}

	fn cursor_increment(&self) -> Self {
		Self {
			ahead: self.ahead.clone(),
			cursor: self.cursor + 1,
			rule: self.rule,
		}
	}
}

impl Hash for RuleItem<'_> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.rule.hash(state);
		for h in self.ahead.iter() {
			state.write(h.as_bytes());
		}
		state.write(&self.cursor.to_be_bytes())
	}
}

type AutomatonRow<'a> = HashMap<Token<'a>, u64>;
type AutomatonTable<'a> = HashMap<u64, AutomatonRow<'a>>;

fn from_rules_to_grammar<'a>(rules: &'a [Rule]) -> GrammarType<'a> {
	rules
		.group_by(|s, d| s.left == d.left)
		.map(|r| {
			(
				r[0].left.clone(),
				r.into_iter().map(|r| r.right.as_slice()).collect(),
			)
		})
		.collect()
}

fn from_rules_literal(rules: Vec<RuleLiteral>) -> Vec<Rule> {
	// let mut rs = Vec::new();
	// rules.into_iter().fold(rs, |rs, r| {

	// })
	let nons: Vec<String> = rules.iter().map(|rule| rule.left.clone()).collect();
	rules
		.clone()
		.into_iter()
		.enumerate()
		.map(|(id, r)| Rule {
			id,
			left: r.left,
			right: r
				.right
				.into_iter()
				.map(|s| {
					if nons.iter().any(|n| *n == s) {
						Token::Nonterminal(s)
					} else {
						Token::Terminal(s)
					}
				})
				.collect(),
		})
		.collect()
}

fn first<'a>(
	rules: &'a [Rule],
	nonterminal: &str,
	visited: &mut HashSet<&'a str>,
) -> HashSet<&'a str> {
	// let mut h = HashSet::new();
	let fs: Vec<&Token> = rules
		.iter()
		.filter_map(|r| {
			if &r.left == nonterminal {
				r.right.get(0)
			} else {
				None
			}
		})
		.collect();
	fs.into_iter()
		.flat_map(|fi| match fi {
			Token::Nonterminal(nonterminal) => match visited.get(nonterminal) {
				None => {
					visited.insert(&nonterminal);
					first(rules, nonterminal, visited).into_iter().collect()
				}
				Some(_) => vec![],
			},
			Token::Terminal(t) => vec![t.to_owned()],
		})
		.collect()
}

fn all_first<'a>(rules: &'a [Rule]) -> HashMap<&'a str, HashSet<&'a str>> {
	let h = HashMap::new();
	rules.iter().fold(h, |mut dict, r| {
		match dict.get(r.left.as_str()) {
			None => {
				// let mut v = Hash;
				let s = first(rules, &r.left, &mut HashSet::new());
				dict.insert(&r.left, s);
				dict
			}
			Some(_) => dict,
		}
	})
}

type Group<'a> = HashMap<&'a str, HashSet<&'a str>>;

fn follow<'a, 'd>(
	rules: &'a [Rule],
	// follow_dict: &'a mut HashMap<&'a str, HashSet<&'a str>>,
	firsts: &'d HashMap<&'a str, HashSet<&'a str>>,
) -> HashMap<&'a str, HashSet<&'a str>> {
	let mut dict: HashMap<&str, (HashSet<&str>, HashSet<&str>)> = HashMap::new();
	for rule in rules
	// .into_iter().fold(HashMap::new(), |res, rule|
	{
		// let ps: Vec<(&str, Vec<&str>)> =
		let maxlen = rule.right.iter().len();
		let mut prev_token = None;
		// let d =
		for i in (0..maxlen)
		// .fold(dict ,|i|
		{
			match rule.right.iter().nth(i) {
				Some(Token::Nonterminal(t)) => {
					if i == maxlen - 1 {
						match dict.get_mut(t) {
							None => {
								dict.insert(t, (HashSet::new(), HashSet::from([rule.left.as_str()])));
							}
							Some((terminals, nons)) => {
								nons.insert(&rule.left);
							}
						}
					}
					if let (Some(prev), Some(v)) = (prev_token, firsts.get(t)) {
						dict
							.entry(prev)
							.and_modify(|(ts, ns)| {
								ns.extend(v.iter());
							})
							.or_insert((v.to_owned(), HashSet::new()));
					}
					prev_token = Some(t);
				}
				Some(Token::Terminal(t)) => {
					match prev_token {
						Some(prev) => {
							// let key = prev.clone();
							dict
								.entry(prev)
								.and_modify(|(ts, ns)| {
									ts.insert(&t);
								})
								.or_insert((HashSet::from([t.to_owned()]), HashSet::new()));
						}
						None => {}
					}
				}
				_ => {}
			}
		}
		// );
	}
	// )
	println!("follow dict");
	println!("{:?}", dict);

	let (expanded, terminalonly) = expand_follow_dict(&dict, HashMap::new());

	terminalonly.to_owned()
}

// fn expand_follow_dict (
// 	dict: & HashMap<&str, (HashSet<& str>, HashSet<& str>)>
// 	,terminal_only: & mut HashMap<& str, HashSet<& str>>
// )
fn expand_follow_dict<'a, 'd>(
	dict: &'d HashMap<&'a str, (HashSet<&'a str>, HashSet<&'a str>)>,
	terminal_only: HashMap<&'a str, HashSet<&'a str>>,
) -> (
	HashMap<&'a str, (HashSet<&'a str>, HashSet<&'a str>)>,
	HashMap<&'a str, HashSet<&'a str>>,
) {
	// search nonterminal only entry from dict, move to terminal_only
	// for (k, v) in
	// 	dict.iter().filter_map(|(&key, &(ref terminals, ref nons)) |
	// 		if nons.len()==0 {
	// 			Some((key, terminals))
	// 		} else {None}
	// 	)
	// 	// .collect::<Vec<(&str, &HashSet<&str>)>>()
	// 	// .iter()
	// 	// );
	// 	{
	// 		nochange = terminal_only.contains_key(k) && nochange;
	// 		terminal_only.entry(k).or_insert(v.clone());
	// 	}
	let next_only = dict
		.iter()
		.filter_map(|(&key, &(ref terminals, ref nons))| {
			if nons.len() == 0 && !terminal_only.contains_key(key) {
				// list.push((key, terminals));
				// (false, list)
				Some((key, terminals))
			} else {
				None
				// (nochange, list)
			}
		});

	// let haschange = next_only.len()

	let next_only_dict: HashMap<&str, _> = next_only.collect();
	let nochange = next_only_dict.len() == 0;

	if !nochange {
		let mut di = HashMap::new();
		dict.iter().for_each(|(&key, &(ref terminals, ref nons))| {
			// di.entry(key).and_modify(|e| )
			let (mut new_terminals, mut still_nons): (Vec<&str>, Vec<&str>) = nons.iter().fold(
				(vec![], vec![]),
				|(mut news, mut stills), n| match next_only_dict.get(n) {
					None => {
						// di.insert(key, (terminals, nons));
						stills.push(*n);
						(news, stills)
					}
					Some(terminals) => {
						news.extend(terminals.iter());
						(news, stills)
					}
				},
			);

			new_terminals.extend(terminals.iter());
			di.insert(
				key,
				(
					new_terminals.into_iter().collect(),
					still_nons.into_iter().collect(),
				),
			);
		});

		next_only_dict.keys().for_each(|k| {});

		let mut o: HashMap<&str, HashSet<_>> = HashMap::new();
		o.extend(terminal_only.into_iter().map(|(k, v)| (k, v.to_owned())));
		o.extend(next_only_dict.into_iter().map(|(k, v)| (k, v.to_owned())));
		expand_follow_dict(&mut di, o)
	} else {
		//  for (k,(ts, ns)) in dict.into_iter() {
		// 	  terminal_only.entry(k)
		// 	//   .and_modify(|(ters, nons)| )
		// 			.or_insert(ts.to_owned());
		//  }
		(dict.to_owned(), terminal_only.clone())
	}

	// for e in nonlies.iter().map(|(k,v) | k)
	// {
	// 	dict.
	// }
}

// #[derive(Debug)]
// struct Item<'a> {
// 	rule: Rule<'a>,
// 	cursor: usize,
// 	ahead: Vec<&'a str>
// }

// impl<'a> Item<'a> {
// 	fn new(rule: Rule<'a>) -> Self {
// 		Item {
// 			rule,
// 			cursor: 0,
// 			ahead: vec![]
// 		}
// 	}
// }
impl<'a, 'd: 'a> RuleItems<'a> {
	fn initial(rulelist: &'a RuleList) -> Option<Self> {
		let r = rulelist
			.0
			.into_iter()
			.as_slice()
			.group_by(|s, d| s.left == d.left)
			.find(|rs| rs.len() == 1)?
			.get(0);

		Some(RuleItems(
			r.into_iter()
				.map(|r| RuleItem {
					rule: r,
					cursor: 0,
					ahead: HashSet::new(),
				})
				.collect(),
		))
	}

	fn slice(&self) -> &[RuleItem] {
		&self.0.as_slice()
	}

	fn derivate(
		// slic: &'d [RuleItem<'a>],
		self,
		random: &RandomState,
		// count_bonus: usize,
		// groups: Vec<Self>,
		firsts: &'a Group,
		rulelist: &'a RuleList,
	) -> ((u64, Vec<(u64, Self)>), AutomatonTable<'a>) {
		// let sli: RuleItems<'a> = self.grouping(HashSet::new(), firsts, rulelist);
		let toslice = &self.0;
		let current = self.grouping(HashSet::new(), firsts, rulelist);
		showshort!(&current);

		let gs = RuleItems::forward(
			// toslice.as_slice()
			current.clone(),
		);
		show!(gs);

		let (hashgroups, tables): (Vec<_>, Vec<AutomatonTable>) = gs
			.clone()
			.into_iter()
			// .enumerate()
			.map(|(t, g)| {
				RuleItems::derivate(
					// g.0.as_slice()
					g, random, firsts, rulelist,
				)
			})
			.unzip();

		let (hashes, groups): (Vec<u64>, Vec<Vec<_>>) = hashgroups.into_iter().unzip();

		let row: AutomatonRow = HashMap::from_iter(
			zip(hashes, gs.clone())
				.into_iter()
				.map(|(i, (t, g))| (t, i)),
		);

		let hash = get_hash(&current, &random);

		let hs = HashMap::from_iter({
			let mut es: Vec<(u64, AutomatonRow)> = tables
				.into_iter()
				.map(|table| table.into_iter())
				.flatten()
				.collect();
			es.push((hash, row));
			es.into_iter()
		});

		let mut v = vec![(hash, current)];
		v.extend(groups.into_iter().flatten());

		((hash, v), hs)
	}

	// fn forward(slic: &'d [RuleItem<'a>]) -> Vec<(Token<'a>, Self)> {
	fn forward(self) -> Vec<(Token<'a>, Self)> {
		// v.into_iter().as_slice()
		// slic
		let v = self.0;
		// let mut grouped = vec![];
		let gg = v.into_iter().group_by(|j| match j.clone().atcursor() {
			None => None,
			Some(t) => Some(*t),
		});
		let gv = gg
			.into_iter()
			.map(|(k, v)| (k, v.collect()))
			.collect::<Vec<(_, Vec<_>)>>();
		let res = groupBy_run(&gv);

		res
			.into_iter()
			.filter_map(|(t, s)| match *t {
				None => None,
				Some(token) => Some((token, {
					let items = s
						.into_iter()
						.flat_map(|g| g.into_iter().map(|g| g.cursor_increment()))
						.collect();
					RuleItems(items)
				})),
			})
			.collect()

		// for (key, group) in &v.into_iter().group_by(|j|
		// 	match j.clone().atcursor() {
		// 		None => None,
		// 		Some(t) => Some( *t )
		// 	}
		// ) {
		// 	if let Some(token) = key {
		// 		grouped.push((
		// 			token,
		// 			RuleItems(group.map(|item| item.cursor_increment()).collect()),
		// 		))
		// 	}

		// }
		// grouped
	}

	fn grouping(
		&self,
		left_history: HashSet<&str>,
		firsts: &'a Group,
		rulelist: &'a RuleList,
	) -> Self {
		let toadd: Vec<_> = self
			.0
			.iter()
			.enumerate()
			.filter_map(|(selfi, item)| match item.rule.right.get(item.cursor) {
				Some(Token::Nonterminal(nonterminal)) => {
					let ah = match item.rule.right.get(item.cursor + 1) {
						Some(Token::Terminal(terminal)) => Some(HashSet::from([*terminal])),
						Some(Token::Nonterminal(non)) => Some(firsts.get(non)?.iter().map(|s| *s).collect()),
						_ => None,
					};
					Some((item.rule, nonterminal, ah.unwrap_or(item.ahead.clone())))
				}
				_ => None,
			})
			.collect();

		let mut bans = vec![];
		let mut upded_left_ahead = HashMap::new();
		let selfupdated = self
			.0
			.iter()
			.enumerate()
			.map(|(selfi, item)| {
				if let Some((addrule, &sameleft, hset)) = toadd
					.iter()
					.find(|(addrule, sameleft, ahead)| sameleft.to_string() == item.rule.left)
				{
					bans.push(sameleft);

					let mut h = HashSet::new();
					h.extend(hset);
					h.extend(item.ahead.clone().into_iter());
					upded_left_ahead.entry(sameleft).or_insert(h.clone());
					RuleItem {
						// ahead: HashSet::from_iter([
						// 	hset,
						// 	item.ahead.iter().collect() ].into_iter().flatten()
						// .map(|s| *s)
						ahead: h,
						cursor: item.cursor,
						rule: item.rule,
					}
				} else {
					item.clone()
				}
			})
			.collect::<Vec<_>>();

		let nextbase = toadd
			.iter()
			.filter(|(fromrule, sameleft, hset)| {
				!&bans.contains(sameleft) && !left_history.contains(*sameleft)
			})
			.flat_map(|(fromrule, sameleft, hset)| {
				rulelist
					.0
					.into_iter()
					.filter(|&r| r.left == sameleft.to_string())
					.map(|r| RuleItem {
						rule: r,
						cursor: 0,
						ahead: {
							let mut h = HashSet::new();
							h.extend(hset.clone());
							h.extend(
								upded_left_ahead
									.get(fromrule.left.as_str())
									.into_iter()
									.map(|d| d.into_iter())
									.flatten(),
							);
							// h.extend(selfupdated.iter().find_map(|r| {
							// 	if r.rule.left == sameleft.to_string() {
							// 		Some(r.ahead.clone())
							// 	} else {None}
							// }).into_iter().flatten()
							// );

							h
						}, // HashSet::from_iter([
						   // 	hset.into_iter().collect::<Vec<_>>(),
						   // 	selfupdated.iter().find_map(|r|
						   // 		if r.rule.left == sameleft.to_string() {
						   // 			let s = r.ahead;
						   // 			Some(s.into_iter())
						   // 		} else {None}
						   // 	).into_iter().flatten().collect()
						   // ].into_iter().flatten().map(|s| *s) ),
					})
			});

		let nextitems = RuleItems(nextbase.collect());
		let history = HashSet::from_iter(
			[
				left_history.into_iter().collect::<Vec<_>>(),
				self.0.iter().map(|item| item.rule.left.as_str()).collect(),
			]
			.into_iter()
			.flatten(),
		);

		let deeper = if nextitems.0.len() > 0 {
			nextitems.grouping(history, firsts, rulelist)
		} else {
			RuleItems(vec![])
		};

		RuleItems(Vec::from_iter(
			[selfupdated, deeper.0].into_iter().flatten(),
		))
	}
}
/*
fn itemgroup(items: Items, firsts: &Group, grammar: &Grammar) -> Items {
	let mut itemsj = Items::empty();
	let content = items.0.into_iter().map(|(k, entries)| {

		if let Some(t) = item.tokens.get(item.cursor) {
			match *t {
				Token::Nonterminal(atcursor) => {
					let (head, tail) = item.tokens.split_at(item.cursor + 1);
					let mut ts = vec![];
					ts.extend(
						[
							tail.iter().collect(),
							item.ahead.into_iter().map(|d| Token::Terminal(d)).collect(),
						]
						.into_iter()
						.flatten(),
					);

					match ts.get(0) {
						Some(Token::Nonterminal(f)) => grammar
							.0
							.get(atcursor)
							.unwrap_or(&vec![])
							.iter()
							.map(| right| {

										ItemEntry {
											ahead: vec![f],
											cursor: 0,
											tokens: right,
										}


							})
							.collect(),
						_ => vec![],
					}
				}
				Token::Terminal(q) => vec![],
			}
		} else {
			vec![]
		}
	});

	Items(HashMap::from_iter(content))

	// itemsj
}
*/
