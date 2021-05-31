use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_until},
    character::complete::{alpha1, anychar, multispace0, none_of},
    combinator::{eof, recognize, value},
    error::ParseError,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult, Parser,
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
        let mut result = None;
        let mut contents = file;

        loop {
            let parsed = self
                .order
                .iter()
                .filter_map(|ident| {
                    let rule = &self.rules[ident];
                    rule.parse(contents, ident, &self.rules)
                })
                .next();
            match (parsed, result.as_mut()) {
                (Some((remaining_contents, parsed)), None) => {
                    result = Some(parsed);
                    contents = remaining_contents;
                }
                (Some((remaining_contents, parsed)), Some(mut already_parsed)) => {
                    already_parsed.append(parsed);
                    contents = remaining_contents;
                }
                (None, None) if contents != "" => {
                    let boundary = (1..)
                        .filter(|i| contents.is_char_boundary(*i))
                        .next()
                        .unwrap();
                    result = Some(ParsedFile::empty(&contents[..boundary]));
                    contents = &contents[boundary..];
                }
                (None, Some(result)) if contents != "" => {
                    let boundary = (1..)
                        .filter(|i| contents.is_char_boundary(*i))
                        .next()
                        .unwrap();
                    result.append(ParsedFile::empty(&contents[..boundary]));
                    contents = &contents[boundary..];
                }
                (None, _) => break,
            }
        }
        return result;
    }
}

type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

fn rules(s: Span) -> IResult<Span, Vec<(Span, Expr)>> {
    let (s, rules) = many0(delimited(multispace0, rule, ws_around(tag(";"))))(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = eof(s)?;

    Ok((s, rules))
}

fn rule(s: Span) -> IResult<Span, (Span, Expr)> {
    let (s, ident) = ident(s)?;
    let (s, _) = ws_around(tag("="))(s)?;
    let (s, expr) = expr(s)?;

    Ok((s, (ident, expr)))
}

fn ws_around<I: Clone, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: nom::InputTakeAtPosition,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
{
    move |input: I| {
        let (input, _) = multispace0(input)?;
        let (input, result) = f.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

fn ident(s: Span) -> IResult<Span, Span> {
    let allowed = (alpha1, tag("_"));
    let (s, name) = recognize(many1(alt(allowed)))(s)?;
    Ok((s, name))
}

fn expr(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = alt((
        one_of,
        one_or_more,
        sequence,
        parens,
        rule_expr,
        char_range,
        exact_string,
    ))(s)?;

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

fn exact_string(s: Span) -> IResult<Span, Expr> {
    let (s, _) = tag("'")(s)?;
    let (s, string) = escaped_transform(
        none_of("'\\"),
        '\\',
        alt((
            value("\n", tag("n")),
            value("\t", tag("t")),
            value("\r", tag("r")),
            value("'", tag("'")),
        )),
    )(s)?;
    let (s, _) = tag("'")(s)?;

    Ok((s, Expr::String(string.to_string())))
}

fn char_(s: Span) -> IResult<Span, char> {
    delimited(tag("'"), anychar, tag("'"))(s)
}

#[recursive_parser]
fn one_of(s: Span) -> IResult<Span, Expr> {
    let (s, expr0) = expr(s)?;
    let (s, _) = ws_around(tag("|"))(s)?;
    let (s, expr1) = expr(s)?;

    Ok((s, Expr::OneOf(Box::new(expr0), Box::new(expr1))))
}

#[recursive_parser]
fn sequence(s: Span) -> IResult<Span, Expr> {
    let (s, expr0) = expr(s)?;
    let (s, _) = tag(" ")(s)?;
    let (s, expr1) = expr(s)?;

    Ok((s, Expr::Sequence(Box::new(expr0), Box::new(expr1))))
}

#[recursive_parser]
fn one_or_more(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = terminated(expr, tag("+"))(s)?;

    Ok((s, Expr::OneOrMore(Box::new(expr))))
}

#[derive(Debug)]
enum Expr {
    Rule(String),
    String(String),
    CharRange(char, char),
    OneOf(Box<Expr>, Box<Expr>),
    Sequence(Box<Expr>, Box<Expr>),
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
            Expr::String(s) => {
                let input = input.strip_prefix(s)?;
                Some((input, ParsedFile::new(my_name, s)))
            }
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
            Expr::Sequence(expr0, expr1) => {
                let (input, mut parsed0) = expr0.parse(input, my_name, rules)?;
                let (input, parsed1) = expr1.parse(input, my_name, rules)?;
                parsed0.append(parsed1);
                Some((input, parsed0))
            }
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
        self.regions.extend(other.regions);
        self.collapse();
    }

    fn collapse(&mut self) {
        let mut new_vec = Vec::with_capacity(self.regions.len());
        let mut existing_drain = self.regions.drain(..);

        loop {
            match (new_vec.last_mut(), existing_drain.next()) {
                (None, Some(drain)) => {
                    new_vec.push(drain);
                }
                (Some(last), Some(drain)) => match last.try_include(drain) {
                    Some(not_included) => {
                        new_vec.push(not_included);
                    }
                    None => {}
                },
                (_, None) => break,
            }
        }

        drop(existing_drain);
        self.regions = new_vec;
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

    fn try_include(&mut self, other: Region<'t>) -> Option<Region<'t>> {
        if self.rules != other.rules {
            return Some(other);
        }
        if let Some(t) = join_strs(self.text, other.text) {
            self.text = t;
            return None;
        }
        return Some(other);
    }
}

fn join_strs<'s>(s0: &'s str, s1: &'s str) -> Option<&'s str> {
    unsafe {
        if s0.as_ptr().add(s0.len()) != s1.as_ptr() {
            return None;
        }
        let slice = std::slice::from_raw_parts(s0.as_ptr(), s0.len() + s1.len());
        std::str::from_utf8(slice).ok()
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
        assert_eq!(stacks.next(), Some((&["num", "digit"][..], "345")));
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
        assert_eq!(stacks.next(), Some((&["hex", "alpha"][..], "ae")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn exact_string() {
        let grammar = Grammar::from_file(
            r"
            abc = 'abc';
        ",
        )
        .unwrap();
        let file = grammar.parse("abc").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["abc"][..], "abc")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn sequence() {
        let grammar = Grammar::from_file(
            r"
            abc = 'a' b c;
            b = 'b';
            c = 'c';
        ",
        )
        .unwrap();
        let file = grammar.parse("abc").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["abc"][..], "a")));
        assert_eq!(stacks.next(), Some((&["abc", "b"][..], "b")));
        assert_eq!(stacks.next(), Some((&["abc", "c"][..], "c")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn control_characters() {
        let grammar = Grammar::from_file(
            r"
            newline = '\n';
            quote = '\'';
            tab = '\t';
        ",
        )
        .unwrap();
        let file = grammar.parse("\n\'\t").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["newline"][..], "\n")));
        assert_eq!(stacks.next(), Some((&["quote"][..], "'")));
        assert_eq!(stacks.next(), Some((&["tab"][..], "\t")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn rust_ident() {
        let grammar = Grammar::from_file(
            r"
            ident = ('a'..'z' | 'A'..'Z' | '_')+;
        ",
        )
        .unwrap();
        let file = grammar.parse("foobar").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["ident"][..], "foobar")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn incomplete_parse() {
        let grammar = Grammar::from_file(
            r"
            abc = 'abc';
        ",
        )
        .unwrap();
        let file = grammar.parse("abcdef").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["abc"][..], "abc")));
        assert_eq!(stacks.next(), Some((&[][..], "def")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn middle_parse() {
        let grammar = Grammar::from_file(
            r"
            abc = 'abc';
        ",
        )
        .unwrap();
        let file = grammar.parse("defabcdef").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&[][..], "def")));
        assert_eq!(stacks.next(), Some((&["abc"][..], "abc")));
        assert_eq!(stacks.next(), Some((&[][..], "def")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn newlines() {
        let grammar = Grammar::from_file(
            r"
            abc = 'abc';
        ",
        )
        .unwrap();
        let file = grammar.parse("def\nabc\ndef").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&[][..], "def\n")));
        assert_eq!(stacks.next(), Some((&["abc"][..], "abc")));
        assert_eq!(stacks.next(), Some((&[][..], "\ndef")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn newline_rule() {
        let grammar = Grammar::from_file(
            r"
            abc =
                'abc';
        ",
        )
        .unwrap();
        let file = grammar.parse("abc").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["abc"][..], "abc")));
        assert_eq!(stacks.next(), None);
    }

    #[test]
    fn multiline_or() {
        let grammar = Grammar::from_file(
            r"
            abc =
                'a'
                | 'b'
                | 'c'
                ;
        ",
        )
        .unwrap();
        let file = grammar.parse("abc").unwrap();

        let mut stacks = file.rule_stacks();
        assert_eq!(stacks.next(), Some((&["abc"][..], "a")));
        assert_eq!(stacks.next(), Some((&["abc"][..], "b")));
        assert_eq!(stacks.next(), Some((&["abc"][..], "c")));
        assert_eq!(stacks.next(), None);
    }

    #[cfg(test)]
    mod join_strs {
        use super::*;

        #[macro_use]
        use quickcheck_macros::*;

        #[quickcheck]
        fn good_strs(s: String, index: usize) -> bool {
            if !s.is_char_boundary(index) {
                return true;
            }
            let (left, right) = s.split_at(index);
            join_strs(left, right).is_some()
        }

        #[quickcheck]
        fn bad_strs(s0: String, s1: String) -> bool {
            join_strs(&s0, &s1).is_none()
        }

        #[quickcheck]
        fn good_strs_value(s: String, index: usize) -> bool {
            if !s.is_char_boundary(index) {
                return true;
            }
            let (left, right) = s.split_at(index);
            let joined = join_strs(left, right).unwrap();
            joined == s
        }
    }
}
