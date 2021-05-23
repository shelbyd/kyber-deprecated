use std::{error::Error, io};
use termion::{
    event::{Event, Key},
    input::TermRead,
    raw::IntoRawMode,
};
use tui::{
    backend::Backend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{List, ListItem, Paragraph, Wrap},
    Frame,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ControlFlow {
    Continue,
    Break,
}

trait EventHandler {
    type Handled;

    fn event(&mut self, event: &Event) -> EventHandling<Self::Handled>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum EventHandling<H = ()> {
    Handled(H),
    NotHandled,
}

impl EventHandler for String {
    type Handled = ();

    fn event(&mut self, event: &Event) -> EventHandling {
        match event {
            Event::Key(Key::Char(c)) => self.push(*c),
            Event::Key(Key::Backspace) => {
                self.pop();
            }
            _ => return EventHandling::NotHandled,
        }
        EventHandling::Handled(())
    }
}

impl<T> From<T> for EventHandling<T> {
    fn from(t: T) -> EventHandling<T> {
        EventHandling::Handled(t)
    }
}

#[derive(Default)]
struct FileSearch {
    fuzzy_text: String,
}

impl FileSearch {
    fn render_height(&self) -> u16 {
        (self.suggestions().count() as u16) + 1
    }

    fn suggestions(&self) -> impl Iterator<Item = FuzzyMatch> + '_ {
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

    fn render<B: Backend>(&self, frame: &mut Frame<B>, size: Rect) {
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
    type Handled = ControlFlow;

    fn event(&mut self, event: &Event) -> EventHandling<ControlFlow> {
        if let EventHandling::Handled(()) = self.fuzzy_text.event(&event) {
            return ControlFlow::Continue.into();
        }

        match event {
            Event::Key(Key::Esc) => return ControlFlow::Break.into(),
            _ => {}
        }

        EventHandling::NotHandled
    }
}

#[derive(Default)]
struct FuzzyMatch {
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

#[derive(Default)]
struct App {
    file_search: Option<FileSearch>,
}

impl App {
    fn render<B: Backend>(&self, frame: &mut Frame<B>) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(
                    self.file_search
                        .as_ref()
                        .map(|s| s.render_height())
                        .unwrap_or(0),
                ),
            ])
            .split(frame.size());

        let text = vec![
            Spans::from(vec![
                Span::raw("First"),
                Span::styled("line", Style::default().add_modifier(Modifier::ITALIC)),
                Span::raw("."),
            ]),
            Spans::from(Span::styled("Second line", Style::default().fg(Color::Red))),
        ];
        let p = Paragraph::new(text)
            .style(Style::default().fg(Color::White).bg(Color::Black))
            .alignment(Alignment::Center)
            .wrap(Wrap { trim: true });
        frame.render_widget(p, chunks[0]);

        if let Some(search) = self.file_search.as_ref() {
            search.render(frame, chunks[1]);
        }
    }
}

impl EventHandler for App {
    type Handled = ControlFlow;

    fn event(&mut self, event: &Event) -> EventHandling<ControlFlow> {
        if let Some(EventHandling::Handled(flow)) =
            self.file_search.as_mut().map(|search| search.event(&event))
        {
            if flow == ControlFlow::Break {
                self.file_search = None;
            }
            return ControlFlow::Continue.into();
        }

        match event {
            Event::Key(Key::Char('q')) => return ControlFlow::Break.into(),
            Event::Key(Key::Ctrl('p')) => {
                self.file_search = Some(FileSearch::default());
            }
            _ => {}
        }

        EventHandling::NotHandled
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let stdin = io::stdin();
    let mut events = stdin.events();

    let stdout = io::stdout().into_raw_mode()?;
    let stdout = termion::input::MouseTerminal::from(stdout);
    let backend = tui::backend::TermionBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend)?;

    let mut app = App::default();

    loop {
        terminal.draw(|f| {
            app.render(f);
        })?;

        let event = match events.next() {
            Some(e) => e,
            None => break,
        };
        let event = event?;
        match app.event(&event) {
            EventHandling::Handled(ControlFlow::Break) => break,
            EventHandling::Handled(ControlFlow::Continue) => {}
            EventHandling::NotHandled => {
                log::debug!("unhandled event {:?}", event);
            }
        }
    }

    Ok(())
}
