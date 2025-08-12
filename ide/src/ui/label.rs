use crate::ui::Widget;
use crate::Screen;

/// A simple widget that just displays text (useful for labels, status bars, etc.)
#[derive(Debug, Clone)]
pub struct Label {
    text: String,
    fg_color: u8,
    bg_color: u8,
    size: (usize, usize),
}

impl Label {
    pub fn new(text: impl Into<String>, fg_color: u8, bg_color: u8) -> Self {
        let text = text.into();
        let width = text.len();
        Self {
            text,
            fg_color,
            bg_color,
            size: (width, 1),
        }
    }

    pub fn with_size(mut self, width: usize, height: usize) -> Self {
        self.size = (width, height);
        self
    }
}

impl Widget for Label {
    fn render(&self, screen: &mut Screen, x: usize, y: usize) {
        screen.write_string(x, y, &self.text, self.fg_color, self.bg_color);
    }

    fn get_size(&self) -> (usize, usize) {
        self.size
    }
}
