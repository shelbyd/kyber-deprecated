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
    text::{Span, Spans},
    widgets::{Block, Paragraph, Wrap},
    Frame,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ControlFlow {
    Continue,
    Break,
}

#[derive(Default)]
struct FileSearch {
    fuzzy_text: String,
}

impl FileSearch {
    fn render_height(&self) -> u16 {
        (self.suggestions().len() as u16) + 1
    }

    fn suggestions(&self) -> Vec<String> {
        Vec::new()
    }

    fn event(&mut self, event: Event) -> ControlFlow {
        match event {
            Event::Key(Key::Esc) => return ControlFlow::Break,
            Event::Key(Key::Char(c)) => self.fuzzy_text.push(c),
            _ => {},
        }

        ControlFlow::Continue
    }

    fn render<B: Backend>(&self, frame: &mut Frame<B>, size: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(0), Constraint::Length(1)])
            .split(size);

        let search_text = vec![Spans::from(vec![
            Span::styled(">>> ", Style::default().fg(Color::Blue)),
            Span::raw(&self.fuzzy_text),
        ])];
        let p =
            Paragraph::new(search_text).style(Style::default().fg(Color::White).bg(Color::Black));
        frame.render_widget(p, chunks[1]);
    }
}

#[derive(Default)]
struct App {
    file_search: Option<FileSearch>,
}

impl App {
    fn event(&mut self, event: Event) -> ControlFlow {
        if let Some(search) = self.file_search.as_mut() {
            match search.event(event) {
                ControlFlow::Break => {
                    self.file_search = None;
                }
                ControlFlow::Continue => {}
            }
        } else {
            match event {
                Event::Key(Key::Char('q')) => return ControlFlow::Break,
                Event::Key(Key::Ctrl('p')) => {
                    self.file_search = Some(FileSearch::default());
                }
                _ => {}
            }
        }

        ControlFlow::Continue
    }

    fn render<B: Backend>(&self, frame: &mut Frame<B>) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(
                    self.file_search
                        .as_ref()
                        .map(|s| s.render_height() + 2)
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

fn main() -> Result<(), Box<dyn Error>> {
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
        match app.event(event?) {
            ControlFlow::Break => break,
            ControlFlow::Continue => {}
        }
    }

    Ok(())
}
