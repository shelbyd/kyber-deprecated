// TODO(shelbyd): Remove after implementing this file.
#![allow(unused)]

use serde::Deserialize;
use std::{collections::HashMap, error::Error};

mod grammar;
use grammar::*;

#[derive(Default)]
pub struct LanguageSupport {
    languages: HashMap<String, Language>,
}

impl LanguageSupport {
    pub fn load() -> Result<Self, Box<dyn Error>> {
        use walkdir::WalkDir;

        let mut config_dir = dirs::home_dir().expect("could not get home_dir");
        config_dir.push(".kyber");

        let files = WalkDir::new(config_dir)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file());

        let mut support = LanguageSupport::default();

        for file in files {
            let stem = file
                .path()
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("file to have utf8 stem");
            match file.path().extension().and_then(|ext| ext.to_str()) {
                Some("peg") => {
                    log::debug!("loading peg file '{}'", file.path().display());
                    let contents = std::fs::read_to_string(file.path())?;
                    let grammar = Grammar::from_file(&contents).unwrap();
                    support.entry(stem).grammar = Some(grammar);
                }
                Some("json") => {
                    log::debug!("loading json config file '{}'", file.path().display());
                    let config = Config::parse(std::fs::read_to_string(file.path())?)?;
                    support.entry(stem).config = Some(config);
                }
                _ => {
                    log::warn!("unrecognized file in '{}'", file.path().display());
                }
            }
        }

        Ok(support)
    }

    fn entry(&mut self, language: &str) -> &mut Language {
        self.languages
            .entry(language.to_string())
            .or_insert(Language::default())
    }

    pub fn color<'s>(&'s self, contents: &'s str, file_path: Option<&str>) -> ColoredText<'s> {
        file_path
            .and_then(|p| self.language(p))
            .map(|l| l.color(contents))
            .unwrap_or_else(|| ColoredText::uncolored(contents))
    }

    fn language(&self, path: &str) -> Option<&Language> {
        self.languages
            .iter()
            .filter(|(_, lang)| lang.matches(path))
            .map(|(_, lang)| lang)
            .next()
    }
}

#[derive(Default)]
struct Language {
    grammar: Option<Grammar>,
    config: Option<Config>,
}

impl Language {
    fn color<'t>(&'t self, text: &'t str) -> ColoredText<'t> {
        let (grammar, config) = match (self.grammar.as_ref(), self.config.as_ref()) {
            (Some(g), Some(c)) => (g, c),
            _ => return ColoredText::uncolored(text),
        };
        let parsed = match grammar.parse(text) {
            Some(p) => p,
            None => return ColoredText::uncolored(text),
        };
        parsed
            .rule_stacks()
            .map(|(stack, text)| {
                let color = stack
                    .iter()
                    .rev()
                    .filter_map(|rule| config.color_for(rule))
                    .next();
                (color, text)
            })
            .collect()
    }

    fn matches(&self, path: &str) -> bool {
        self.config
            .as_ref()
            .map(|c| c.matches_file(path))
            .unwrap_or(false)
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Config {
    match_files: Vec<Glob>,
    highlight: HashMap<String, Color>,
}

impl Config {
    fn parse(file: String) -> Result<Self, Box<dyn Error>> {
        Ok(serde_json::from_str(&file)?)
    }

    fn matches_file(&self, path: &str) -> bool {
        self.match_files.iter().any(|glob| glob.0.matches(path))
    }

    fn color_for(&self, rule: &str) -> Option<Color> {
        unimplemented!("color_for");
    }
}

struct Glob(glob::Pattern);

impl<'de> Deserialize<'de> for Glob {
    fn deserialize<D>(de: D) -> Result<Self, <D as serde::Deserializer<'de>>::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::*;
        use std::fmt;

        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Glob;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "a glob pattern")
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                let pat = glob::Pattern::new(s)
                    .map_err(|_| Error::invalid_value(Unexpected::Str(s), &self))?;
                Ok(Glob(pat))
            }
        }

        de.deserialize_str(Visitor)
    }
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Color {
    Blue,
    Orange,
}

impl Into<tui::style::Color> for Color {
    fn into(self) -> tui::style::Color {
        unimplemented!("into");
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct ColoredText<'t> {
    colored: Vec<(Option<Color>, &'t str)>,
}

impl<'t> ColoredText<'t> {
    fn new(texts: &[(Option<Color>, &'t str)]) -> Self {
        ColoredText {
            colored: texts.iter().map(|(c, s)| (*c, *s)).collect(),
        }
    }

    fn uncolored(text: &'t str) -> Self {
        ColoredText {
            colored: vec![(None, text)],
        }
    }

    fn only(color: Option<Color>, text: &'t str) -> Self {
        ColoredText {
            colored: vec![(color, text)],
        }
    }

    pub fn lines<'s>(&'s self) -> impl Iterator<Item = ColoredText<'s>> {
        let mut lines = self.colored.iter().flat_map(|(color, text)| {
            text.lines()
                .enumerate()
                .map(move |(i, l)| (i > 0, *color, l))
        });

        let mut current = None;
        std::iter::from_fn(move || loop {
            let (is_split, color, text) = match lines.next() {
                Some(n) => n,
                None => return current.take(),
            };
            if is_split {
                return current.replace(ColoredText::only(color, text));
            }
            match current.as_mut() {
                None => {
                    current.replace(ColoredText::only(color, text));
                }
                Some(mut c) => c.colored.push((color, text)),
            }
        })
    }

    pub fn into_iter(self) -> impl Iterator<Item = (Option<Color>, &'t str)> {
        self.colored.into_iter()
    }
}

impl<'s> core::iter::FromIterator<(Option<Color>, &'s str)> for ColoredText<'s> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Option<Color>, &'s str)>,
    {
        ColoredText {
            colored: iter.into_iter().collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod colored_text {
        use super::*;

        #[test]
        fn empty() {
            assert_eq!(ColoredText::default().lines().next(), None);
        }

        #[test]
        fn one_line() {
            let text = ColoredText::uncolored("some text");
            let mut iter = text.lines();

            assert_eq!(iter.next(), Some(ColoredText::uncolored("some text")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn multiple_lines() {
            let text = ColoredText::uncolored("some text\nmore text");
            let mut iter = text.lines();

            assert_eq!(iter.next(), Some(ColoredText::uncolored("some text")));
            assert_eq!(iter.next(), Some(ColoredText::uncolored("more text")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn color_across_lines() {
            let text = ColoredText::new(&[(Some(Color::Orange), "some text\nmore text")]);
            let mut iter = text.lines();

            assert_eq!(
                iter.next(),
                Some(ColoredText::only(Some(Color::Orange), "some text"))
            );
            assert_eq!(
                iter.next(),
                Some(ColoredText::only(Some(Color::Orange), "more text"))
            );
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn multiple_colors_one_line() {
            let text =
                ColoredText::new(&[(Some(Color::Orange), "some "), (Some(Color::Blue), "text")]);
            let mut iter = text.lines();

            assert_eq!(iter.next(), Some(text.clone()));
            assert_eq!(iter.next(), None);
        }
    }
}
