use crate::screen::{Color, Screen};
use crate::ui::event::WidgetIdType;
use crate::ui::widget::Widget;

/// A simple widget that just displays text (useful for labels, status bars, etc.)
#[derive(Debug, Clone)]
pub struct Label<Id: WidgetIdType> {
    text: String,
    fg_color: Color,
    bg_color: Color,
    size: (usize, usize),
    _phantom: std::marker::PhantomData<Id>,
}

impl<Id: WidgetIdType> Label<Id> {
    pub fn new(text: impl Into<String>, fg_color: Color, bg_color: Color) -> Self {
        let text = text.into();
        let width = text.len();
        Self {
            text,
            fg_color,
            bg_color,
            size: (width, 1),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn with_size(mut self, width: usize, height: usize) -> Self {
        self.size = (width, height);
        self
    }
}

impl<Id: WidgetIdType> Widget<Id> for Label<Id> {
    fn render(&self, screen: &mut Screen, x: usize, y: usize) {
        screen.write_string(x, y, &self.text, self.fg_color, self.bg_color);
    }

    fn get_size(&self) -> (usize, usize) {
        self.size
    }
}
