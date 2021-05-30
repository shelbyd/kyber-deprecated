use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, anychar, multispace0},
    combinator::recognize,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};
use std::{collections::HashMap, error::Error};

#[derive(Debug)]
pub struct Grammar {
    rules: HashMap<String, Expr>,
    order: Vec<String>,
}

impl Grammar {
    pub fn from_file(file: &str) -> Result<Self, Box<dyn Error + '_>> {
        let mut order = Vec::new();
        let rules = rules(LocatedSpan::new_extra(file, RecursiveInfo::new()))?
            .1
            .into_iter()
            .map(|(ident, rule)| (ident.to_string(), rule))
            .inspect(|(ident, _)| order.push(ident.clone()))
            .collect();
        Ok(Grammar { rules, order })
    }

    pub fn parse<'t>(&'t self, file: &'t str) -> Option<ParsedFile<'t>> {
        let (file, rule) = self
            .order
            .iter()
            .filter_map(|ident| {
                let rule = &self.rules[ident];
                rule.parse(file, ident, &self.rules)
            })
            .next()?;
        Some(rule)
    }
}

type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

fn rules(s: Span) -> IResult<Span, Vec<(Span, Expr)>> {
    let (s, rules) = many0(delimited(multispace0, rule, tag(";")))(s)?;

    Ok((s, rules))
}

fn rule(s: Span) -> IResult<Span, (Span, Expr)> {
    let (s, ident) = ident(s)?;
    let (s, _) = tag(" = ")(s)?;
    let (s, expr) = expr(s)?;

    Ok((s, (ident, expr)))
}

fn ident(s: Span) -> IResult<Span, Span> {
    let allowed = (alpha1, tag("_"));
    let (s, name) = recognize(many1(alt(allowed)))(s)?;
    Ok((s, name))
}

fn expr(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = alt((one_of, one_or_more, parens, rule_expr, char_range))(s)?;

    Ok((s, expr))
}

fn parens(s: Span) -> IResult<Span, Expr> {
    let (s, _) = tag("(")(s)?;
    let (s, expr) = expr(s)?;
    let (s, _) = tag(")")(s)?;

    Ok((s, expr))
}

fn rule_expr(s: Span) -> IResult<Span, Expr> {
    let (s, ident) = ident(s)?;

    Ok((s, Expr::Rule(ident.to_string())))
}

fn char_range(s: Span) -> IResult<Span, Expr> {
    let (s, (start, _, end)) = tuple((char_, tag(".."), char_))(s)?;

    Ok((s, Expr::CharRange(start, end)))
}

fn char_(s: Span) -> IResult<Span, char> {
    delimited(tag("'"), anychar, tag("'"))(s)
}

#[recursive_parser]
fn one_of(s: Span) -> IResult<Span, Expr> {
    let (s, expr0) = expr(s)?;
    let (s, _) = tag(" | ")(s)?;
    let (s, expr1) = expr(s)?;

    Ok((s, Expr::OneOf(Box::new(expr0), Box::new(expr1))))
}

#[recursive_parser]
fn one_or_more(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = terminated(expr, tag("+"))(s)?;

    Ok((s, Expr::OneOrMore(Box::new(expr))))
}

#[derive(Debug)]
enum Expr {
    Rule(String),
    CharRange(char, char),
    OneOf(Box<Expr>, Box<Expr>),
    OneOrMore(Box<Expr>),
}

impl Expr {
    fn parse<'i>(
        &'i self,
        input: &'i str,
        my_name: &'i str,
        rules: &'i HashMap<String, Expr>,
    ) -> Option<(&'i str, ParsedFile<'i>)> {
        match self {
            Expr::CharRange(start, end) => {
                let mut chars = input.char_indices();
                let (index, char_) = chars.next()?;
                if (*start..=*end).contains(&char_) {
                    let parsed = ParsedFile::new(my_name, &input[..=index]);
                    Some((chars.as_str(), parsed))
                } else {
                    None
                }
            }
            Expr::OneOrMore(expr) => {
                let (mut input, mut parsed) = expr.parse(input, my_name, rules)?;
                while let Some((parsed_input, next_parsed)) = expr.parse(input, my_name, rules) {
                    input = parsed_input;
                    parsed.append(next_parsed);
                }
                Some((input, parsed))
            }
            Expr::Rule(ident) => {
                let expr = &rules[ident];
                let (input, mut parsed) = expr.parse(input, ident, rules)?;
                parsed.push_rule(my_name);
                Some((input, parsed))
            }
            Expr::OneOf(expr0, expr1) => expr0
                .parse(input, my_name, rules)
                .or_else(|| expr1.parse(input, my_name, rules)),
            expr => unimplemented!("{:?}", expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedFile<'t> {
    regions: Vec<Region<'t>>,
}

impl<'t> ParsedFile<'t> {
    pub fn rule_stacks(&self) -> impl Iterator<Item = (&[&'t str], &'t str)> {
        self.regions.iter().map(Region::as_ref)
    }

    fn new(rule: &'t str, text: &'t str) -> Self {
        ParsedFile {
            regions: vec![Region::new(rule, text)],
        }
    }

    fn empty(text: &'t str) -> Self {
        ParsedFile {
            regions: vec![Region::empty(text)],
        }
    }

    fn push_rule(&mut self, rule: &'t str) {
        for region in self.regions.iter_mut() {
            region.push_rule(rule);
        }
    }

    fn append(&mut self, other: ParsedFile<'t>) {
        self.regions.extend(other.regions)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Region<'t> {
    text: &'t str,
    rules: Vec<&'t str>,
}

impl<'t> Region<'t> {
    fn new(rule: &'t str, text: &'t str) -> Self {
        Region {
            text,
            rules: vec![rule],
        }
    }

    fn empty(text: &'t str) -> Self {
        Region {
            text,
            rules: Vec::new(),
        }
    }

    fn as_ref<'r>(&'r self) -> (&'r [&'t str], &'t str) {
        (self.rules.as_slice(), self.text)
    }

    fn push_rule(&mut self, rule: &'t str) {
        self.rules.insert(0, rule)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_grammar() {
        let grammar = Grammar::from_file("").unwrap();
        assert_eq!(grammar.parse(""), None);
    }

    #[test]
    fn digit() {
        let grammar = Grammar::from_file("digit = '0'..'9';").unwrap();
        let file = grammar.parse("3").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["digit"][..], "3")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn one_or_more_rules() {
        let grammar = Grammar::from_file(
            r"
            num = digit+;
            digit = '0'..'9';
        ",
        )
        .unwrap();
        let file = grammar.parse("345").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["num", "digit"][..], "3")));
        assert_eq!(stacks.next(), Some((&["num", "digit"][..], "4")));
        assert_eq!(stacks.next(), Some((&["num", "digit"][..], "5")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn one_of_rule() {
        let grammar = Grammar::from_file(
            r"
            num_or_alpha = alpha | digit;
            alpha = 'a'..'z';
            digit = '0'..'9';
        ",
        )
        .unwrap();
        let file = grammar.parse("3").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["num_or_alpha", "digit"][..], "3")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn combined_rules() {
        let grammar = Grammar::from_file(
            r"
            hex = (alpha | digit)+;
            alpha = 'a'..'f';
            digit = '0'..'9';
        ",
        )
        .unwrap();
        let file = grammar.parse("3ae").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["hex", "digit"][..], "3")));
        assert_eq!(stacks.next(), Some((&["hex", "alpha"][..], "a")));
        assert_eq!(stacks.next(), Some((&["hex", "alpha"][..], "e")));
        assert_eq!(stacks.next(), None);
    }
}
