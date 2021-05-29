// TODO(shelbyd): Remove after implementing this file.
#![allow(unused)]

use serde::Deserialize;
use std::{collections::HashMap, error::Error};

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
                    let grammar = Grammar::parse(std::fs::read_to_string(file.path())?)?;
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
}

#[derive(Default)]
struct Language {
    grammar: Option<Grammar>,
    config: Option<Config>,
}

struct Grammar {}

impl Grammar {
    fn parse(file: String) -> Result<Self, Box<dyn Error>> {
        // TODO(shelbyd): Implement grammar parsing.
        Ok(Grammar {})
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

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum Color {
    Orange,
}
