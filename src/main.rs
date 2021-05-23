use std::{error::Error, io};
use termion::raw::IntoRawMode;

fn main() -> Result<(), Box<dyn Error>> {
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = termion::input::MouseTerminal::from(stdout);
    let backend = tui::backend::TermionBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend)?;

    for _ in 0..10 {
        terminal.draw(|f| {
            let size = f.size();
            let block = tui::widgets::Block::default()
                .title("Block")
                .borders(tui::widgets::Borders::ALL);
            f.render_widget(block, size)
        })?;
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    Ok(())
}
