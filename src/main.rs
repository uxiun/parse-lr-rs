use std::collections::{HashMap, HashSet};

use nonempty::NonEmpty;
// #[cfg(not(debug_assertions))]
macro_rules! show {
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
	let rules = from_rules_literal(from_raw_rules(s));
	// println!("{:#?}", rules);
	let firsts = all_first(&rules);
	println!("{:#?}", &firsts);
	
	let follows = follow(&rules, &firsts);
	// let follows = follow(&rules);
	println!("follows");
	
	println!("{:?}", follows);
}

#[derive(Debug, PartialEq, Eq)]
enum Token<'a> {
	Nonterminal(&'a str),
	Terminal(&'a str),
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
	left: String,
	right: Vec<Token<'a>>,
}
fn from_rules_literal(rules: Vec<RuleLiteral>) -> Vec<Rule> {
	// let mut rs = Vec::new();
	// rules.into_iter().fold(rs, |rs, r| {

	// })
	let nons: Vec<String> = rules.iter().map(|rule| rule.left.clone()).collect();
	rules
		.clone()
		.into_iter()
		.map(|r| Rule {
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

fn follow<'a,'d>(
	rules: &'a [Rule],
	// follow_dict: &'a mut HashMap<&'a str, HashSet<&'a str>>,
	firsts: &'d HashMap<&'a str, HashSet<&'a str>>
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
						dict.entry(prev)
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
