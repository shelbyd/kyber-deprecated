use termion::event::{Event, Key};
use tui::{
    layout::{Rect},
    style::{Color, Style},
    text::{Span, Spans},
    widgets::Paragraph,
};

use crate::{control_flow::*, event_handling::*, file_search::*, language_support::*, widgets::*};

#[derive(Default)]
struct Cursor {
    line: usize,
    column: usize,
}

impl Cursor {
    fn event(&mut self, event: &Event, contents: &str) -> EventHandling {
        if let &Event::Key(Key::Char(c)) = event {
            match c {
                'j' => self.delta(0, 1, contents),
                'k' => self.delta(0, -1, contents),
                'l' => self.delta(1, 0, contents),
                'h' => self.delta(-1, 0, contents),
                _ => return EventHandling::NotHandled,
            }
            return EventHandling::Handled(());
        }
        EventHandling::NotHandled
    }

    fn delta(&mut self, col: isize, line: isize, contents: &str) {
        use core::cmp::*;

        let usize_delta = |val, delta: isize| {
            let with_delta = (val as isize).saturating_add(delta);
            max(0, with_delta) as usize
        };
        let desired_line = usize_delta(self.line, line);
        let desired_col = usize_delta(self.column, col);

        let line = min(contents.lines().count() - 1, desired_line);
        let line_contents = contents.lines().nth(line).unwrap();
        let column = min(line_contents.chars().count() - 1, desired_col);

        self.line = line;
        self.column = column;
    }

    fn render_lines(&self, render_rect: Rect) -> (usize, usize) {
        let height = render_rect.height as usize;
        let begin = self.line.saturating_sub(height / 2);
        (begin, begin + height)
    }
}

pub struct App {
    file_search: Option<FileSearch>,
    contents: String,
    cursor: Cursor,
    language_support: LanguageSupport,
}

impl App {
    pub fn new(language_support: LanguageSupport) -> Self {
        App {
            language_support,
            file_search: None,
            contents: String::new(),
            cursor: Cursor::default(),
        }
    }

    pub fn cursor(&self, rect: Rect) -> Option<(u16, u16)> {
        let (begin, _) = self.cursor.render_lines(rect);
        Some((self.cursor.column as u16, (self.cursor.line - begin) as u16))
    }
}

impl EventHandler for App {
    type Handled = ControlFlow;

    fn event(&mut self, event: &Event) -> EventHandling<ControlFlow> {
        let file_search_handling = self.file_search.as_mut().map(|search| search.event(&event));
        if let Some(EventHandling::Handled(flow)) = file_search_handling {
            if let ControlFlow::Break(file) = flow {
                self.file_search = None;
                if let Some(path) = file {
                    self.contents = std::fs::read_to_string(path).unwrap();
                    self.cursor = Cursor::default();
                }
            }
            return ControlFlow::Continue.into();
        }

        if let EventHandling::Handled(()) = self.cursor.event(&event, &self.contents) {
            return ControlFlow::Continue.into();
        }

        match event {
            Event::Key(Key::Char('q')) => return ControlFlow::Break(()).into(),
            Event::Key(Key::Ctrl('p')) => {
                self.file_search = Some(FileSearch::default());
            }
            _ => {}
        }

        EventHandling::NotHandled
    }
}

pub fn render(app: &App) -> impl Render + '_ {
    let file_search_len = app
        .file_search
        .as_ref()
        .map(|s| s.render_height())
        .unwrap_or(0);

    let mut vertical = Vertical::new().min(
        0,
        CursorText {
            cursor: &app.cursor,
            text: app.contents.as_ref(),
        },
    );
    if let Some(s) = app.file_search.as_ref() {
        let render = crate::file_search::render(s);
        vertical = vertical.length(file_search_len, render);
    }
    vertical
}

struct CursorText<'a> {
    cursor: &'a Cursor,
    text: &'a str,
}

impl<'a> Render for CursorText<'a> {
    fn render(self: Box<Self>, rect: Rect, buffer: &mut tui::buffer::Buffer) {
        let (begin, end) = self.cursor.render_lines(rect);
        let text = self
            .text
            .lines()
            .skip(begin)
            .take(end - begin)
            .map(|line| Spans::from(Span::raw(line)))
            .collect::<Vec<_>>();
        let p = Paragraph::new(text).style(Style::default().fg(Color::White).bg(Color::Black));
        tui::widgets::Widget::render(p, rect, buffer);
    }
}
