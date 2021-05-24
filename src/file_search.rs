use termion::event::{Event, Key};
use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Span, Spans, Text},
    widgets::{List, ListItem, Paragraph},
    Frame,
};

use crate::{control_flow::*, event_handling::*};

#[derive(Default)]
pub struct FileSearch {
    fuzzy_text: String,
}

impl FileSearch {
    pub fn render_height(&self) -> u16 {
        (self.suggestions().count() as u16) + 1
    }

    pub fn suggestions(&self) -> impl Iterator<Item = FuzzyMatch> + '_ {
        use ignore::Walk;

        Walk::new(".")
            .into_iter()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.file_type().map(|f| f.is_file()).unwrap_or(false))
            .map(|entry| entry.into_path().into_os_string())
            .filter_map(|os_string| os_string.into_string().ok())
            .filter_map(move |path| FuzzyMatch::from_checked(path, &self.fuzzy_text))
            .take(5)
    }

    pub fn render<B: Backend>(&self, frame: &mut Frame<B>, size: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(0), Constraint::Length(1)])
            .split(size);

        let items = self
            .suggestions()
            .map(|s| ListItem::new(s))
            .collect::<Vec<_>>();
        let list = List::new(items);
        frame.render_widget(list, chunks[0]);

        let search_text = vec![Spans::from(vec![
            Span::styled(">>> ", Style::default().fg(Color::Blue)),
            Span::raw(&self.fuzzy_text),
        ])];
        let p =
            Paragraph::new(search_text).style(Style::default().fg(Color::White).bg(Color::Black));
        frame.render_widget(p, chunks[1]);
    }
}

impl EventHandler for FileSearch {
    type Handled = ControlFlow<Option<String>>;

    fn event(&mut self, event: &Event) -> EventHandling<ControlFlow<Option<String>>> {
        if &Event::Key(Key::Char('\n')) == event {
            let result = self.suggestions().next().map(Into::into);
            return ControlFlow::Break(result).into();
        }
        if let EventHandling::Handled(()) = self.fuzzy_text.event(&event) {
            return ControlFlow::Continue.into();
        }

        match event {
            Event::Key(Key::Esc) => return ControlFlow::Break(None).into(),
            _ => {}
        }

        EventHandling::NotHandled
    }
}

#[derive(Default)]
pub struct FuzzyMatch {
    regions: Vec<(String, bool)>,
}

impl FuzzyMatch {
    fn from_checked(input: String, pattern: &str) -> Option<Self> {
        use std::collections::VecDeque;
        let mut input = input.chars().collect::<VecDeque<_>>();
        let mut pattern = pattern.chars().collect::<VecDeque<_>>();

        let mut match_ = FuzzyMatch::default();

        loop {
            match (input.pop_front(), pattern.get(0)) {
                (Some(input_char), Some(pat_char)) if input_char == *pat_char => {
                    match_.push_match(input_char);
                    pattern.pop_front();
                }
                (Some(c), _) => match_.push_mismatch(c),
                (None, Some(_)) => return None,
                (None, None) => break,
            }
        }

        Some(match_)
    }

    fn push_match(&mut self, char_: char) {
        match self.regions.last_mut() {
            Some((s, true)) => s.push(char_),
            Some(_) | None => self.regions.push((String::from(char_), true)),
        }
    }

    fn push_mismatch(&mut self, char_: char) {
        match self.regions.last_mut() {
            Some((s, false)) => s.push(char_),
            Some(_) | None => self.regions.push((String::from(char_), false)),
        }
    }
}

impl<'a> Into<Text<'a>> for FuzzyMatch {
    fn into(self) -> Text<'a> {
        let spans = Spans::from(
            self.regions
                .into_iter()
                .map(|(s, is_match)| {
                    let color = if is_match { Color::Blue } else { Color::White };
                    Span::styled(s, Style::default().fg(color))
                })
                .collect::<Vec<_>>(),
        );

        spans.into()
    }
}

impl Into<String> for FuzzyMatch {
    fn into(self) -> String {
        self.regions.into_iter().map(|(s, _)| s).collect()
    }
}
