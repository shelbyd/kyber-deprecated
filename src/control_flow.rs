#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlow<B = ()> {
    Continue,
    Break(B),
}
