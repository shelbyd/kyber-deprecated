use termion::event::{Event, Key};

pub trait EventHandler {
    type Handled;

    fn event(&mut self, event: &Event) -> EventHandling<Self::Handled>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EventHandling<H = ()> {
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
