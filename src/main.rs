use std::{error::Error, io};
use termion::{input::TermRead, raw::IntoRawMode};

mod app;
use app::*;

mod control_flow;
use control_flow::*;

mod event_handling;
use event_handling::*;

mod file_search;

mod language_support;
use language_support::*;

mod widgets;
use widgets::*;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let mut app = App::new(LanguageSupport::load()?);

    let stdin = io::stdin();
    let mut events = stdin.events();

    let stdout = io::stdout().into_raw_mode()?;
    let stdout = termion::input::MouseTerminal::from(stdout);
    let backend = tui::backend::TermionBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend)?;

    loop {
        terminal.draw(|f| {
            let widget = crate::app::render(&app);
            widget.render_into(f, f.size());
            if let Some((x, y)) = app.cursor(f.size()) {
                f.set_cursor(x, y);
            }
        })?;

        let event = match events.next() {
            Some(e) => e,
            None => break,
        };
        let event = event?;
        match app.event(&event) {
            EventHandling::Handled(ControlFlow::Break(())) => break,
            EventHandling::Handled(ControlFlow::Continue) => {}
            EventHandling::NotHandled => {
                log::debug!("unhandled event {:?}", event);
            }
        }
    }

    Ok(())
}
