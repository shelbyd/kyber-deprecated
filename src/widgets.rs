use tui::{
    backend::Backend,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    widgets::Widget,
    Frame,
};

pub struct Vertical<'w> {
    constrained: Vec<(Constraint, Box<dyn Render + 'w>)>,
}

impl<'w> Vertical<'w> {
    pub fn new() -> Self {
        Self {
            constrained: Vec::new(),
        }
    }

    pub fn min<R: Render + 'w>(mut self, amount: u16, render: R) -> Vertical<'w> {
        self.constrained.push((Constraint::Min(amount), Box::new(render)));
        self
    }

    pub fn length<R: Render + 'w>(mut self, amount: u16, render: R) -> Vertical<'w> {
        self.constrained.push((Constraint::Length(amount), Box::new(render)));
        self
    }
}

impl<'w> Render for Vertical<'w> {
    fn render(self: Box<Self>, rect: Rect, buffer: &mut Buffer) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(self.constrained.iter().map(|(c, _)| *c).collect::<Vec<_>>())
            .split(rect);
        for ((_, widget), rect) in self.constrained.into_iter().zip(chunks) {
            widget.render(rect, buffer);
        }
    }
}

pub trait Render {
    fn render(self: Box<Self>, rect: Rect, buffer: &mut Buffer);

    fn render_into<B: Backend>(self, frame: &mut Frame<B>, rect: Rect)
    where
        Self: Sized,
    {
        struct W<R>(R);
        impl<R: Render> Widget for W<R> {
            fn render(self, rect: Rect, buffer: &mut Buffer) {
                Box::new(self.0).render(rect, buffer);
            }
        }

        frame.render_widget(W(self), rect);
    }
}

impl<W> Render for W
where
    W: Widget,
{
    fn render(self: Box<Self>, rect: Rect, buffer: &mut Buffer) {
        Widget::render(*self, rect, buffer)
    }
}

pub struct Empty;

impl Render for Empty {
    fn render(self: Box<Self>, _: Rect, _: &mut Buffer) {}
}
