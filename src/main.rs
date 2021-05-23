use std::{error::Error, io};
use termion::{
    event::{Event, Key},
    input::TermRead,
    raw::IntoRawMode,
};
use tui::{
    layout::Alignment,
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Paragraph, Widget, Wrap},
};

struct App;

impl App {
    fn render(&self) -> impl Widget {
        let text = vec![
            Spans::from(vec![
                Span::raw("First"),
                Span::styled("line", Style::default().add_modifier(Modifier::ITALIC)),
                Span::raw("."),
            ]),
            Spans::from(Span::styled("Second line", Style::default().fg(Color::Red))),
        ];
        Paragraph::new(text)
            .block(
                Block::default()
                    .title("Paragraph")
                    .borders(tui::widgets::Borders::ALL),
            )
            .style(Style::default().fg(Color::White).bg(Color::Black))
            .alignment(Alignment::Center)
            .wrap(Wrap { trim: true })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut events = stdin.events();

    let stdout = io::stdout().into_raw_mode()?;
    let stdout = termion::input::MouseTerminal::from(stdout);
    let backend = tui::backend::TermionBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend)?;

    let mut app = App;

    loop {
        terminal.draw(|f| {
            let size = f.size();
            f.render_widget(app.render(), size)
        })?;

        let event = match events.next() {
            Some(e) => e,
            None => break,
        };
        match event? {
            Event::Key(Key::Char('q')) => break,
            _ => {}
        }
    }

    Ok(())
}
