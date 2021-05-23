use std::{error::Error, io};
use termion::raw::IntoRawMode;
use tui::{
    layout::Alignment,
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Paragraph, Wrap},
};

fn main() -> Result<(), Box<dyn Error>> {
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = termion::input::MouseTerminal::from(stdout);
    let backend = tui::backend::TermionBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend)?;

    for _ in 0..10 {
        terminal.draw(|f| {
            let size = f.size();
            let text = vec![
                Spans::from(vec![
                    Span::raw("First"),
                    Span::styled("line", Style::default().add_modifier(Modifier::ITALIC)),
                    Span::raw("."),
                ]),
                Spans::from(Span::styled("Second line", Style::default().fg(Color::Red))),
            ];
            let p = Paragraph::new(text)
                .block(
                    Block::default()
                        .title("Paragraph")
                        .borders(tui::widgets::Borders::ALL),
                )
                .style(Style::default().fg(Color::White).bg(Color::Black))
                .alignment(Alignment::Center)
                .wrap(Wrap { trim: true });

            f.render_widget(p, size)
        })?;
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    Ok(())
}
