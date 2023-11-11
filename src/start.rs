use std::collections::HashMap;

use ptree::print_tree;

use crate::{token::tokenize_word, parse::parselr, automaton::RuleList, show, tree::parsed_to_ptree};


pub fn parseprint<'a>(
	sentences: &[&'a str],
	initialid: u64,
	iddict: &HashMap<u64,usize>,
	rulelist: &RuleList,
	tableset: &crate::table::Tables<'_, usize, usize> 
)
{
	for src in sentences {
		println!("let's parse src");
		show!(src);
		let mut input = tokenize_word(src);
		
		let initialid = iddict.get(&initialid).unwrap_or(&(1000 as usize));
		let mut parsed = parselr(input.clone(), *initialid, tableset, rulelist);
		show!(parsed);
		
		// print_parse_tree(&mut input, &mut parsed.reduced, &rulelist);
		// builderror
	
		let tree = parsed_to_ptree(&mut input, &mut parsed.reduced, &rulelist);
		print_tree(&tree);
	}
}