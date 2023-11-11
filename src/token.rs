use nom::{
	bytes::complete::is_not,
	character::complete::{anychar, multispace1, space0},
	error::ParseError,
	multi::{many0, separated_list0},
	sequence::{delimited, preceded},
	IResult, InputLength, Parser,
};

use crate::{
	automaton::{Ahead, RuleList},
	parse::TokenWithLiteral,
	show,
};

pub fn tokenize_word<'a>(src: &'a str) -> Vec<TokenWithLiteral> {
	// let spaces = |s| {multispace1(s)};
	// if let Ok((rem, s)) = trim(src) {
	// show!(s);
	tokenize_separated_list0(src, multispace1, not_space)
	// } else {
	// vec![]
	// }
}

fn words(s: &str) -> IResult<&str, Vec<&str>> {
	many0(preceded(multispace1, not_space))(s)
}

fn not_space(s: &str) -> IResult<&str, &str> {
	is_not(" \t\r\n")(s)
}

fn tokenize_separated_list0<'a, E, F, G>(
	src: &'a str,
	// rulelist: RuleList<'a>,
	// mut separator: impl FnMut(&'a str) -> IResult<&'a str, Vec<&'a str>, E>
	sep: G,
	f: F,
) -> Vec<TokenWithLiteral>
where
	// I: Clone + InputLength,
	E: ParseError<&'a str>,
	F: Parser<&'a str, &'a str, E>,
	G: Parser<&'a str, &'a str, E>,
{
	// separated_list0(sep, f)()
	// let res = separator(src);
	let res = many0(preceded(
		many0(sep)
		, f))(src);
	
	match res {
		Ok((remained_src, ve)) => {
			// show!(remained_src);
			ve.into_iter()
				.map(|o| TokenWithLiteral {
					literal: o,
					terminal: o.into(),
				})
				.collect()
		}
		Err(e) => {
			// show!(e); // cannot
			match e {
				nom::Err::Error(e) => {
					println!("Err::Error");
				}
				nom::Err::Failure(e) => {
					println!("Err::Failure");
				}
				nom::Err::Incomplete(e) => {
					println!("Err::Incomplete");
				}
			}
			vec![]
		}
	}
}
