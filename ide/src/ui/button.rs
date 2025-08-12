use crate::screen::{CellAttribute, Color, Screen};
use crate::ui::event::{Event, SpecialKey};
use crate::ui::widget::Widget;

/// A clickable button widget with text label
#[derive(Debug, Clone)]
pub struct Button {
    id: String,
    label: String,
    size: (usize, usize),
    focused: bool,
    enabled: bool,
    fg_color: Color,
    bg_color: Color,
    focus_fg_color: Color,
    focus_bg_color: Color,
}

impl Button {
    /// Create a new button with the given id and label
    pub fn new(id: impl Into<String>, label: impl Into<String>) -> Self {
        let label = label.into();
        let width = label.len() + 4; // Add padding for [ label ]

        Self {
            id: id.into(),
            label,
            size: (width, 1),
            focused: false,
            enabled: true,
            fg_color: Color::White,
            bg_color: Color::Blue,
            focus_fg_color: Color::Black,
            focus_bg_color: Color::LightGray,
        }
    }

    /// Set custom size for the button
    pub fn with_size(mut self, width: usize, height: usize) -> Self {
        self.size = (width, height);
        self
    }

    /// Set custom colors
    pub fn with_colors(mut self, fg: Color, bg: Color, focus_fg: Color, focus_bg: Color) -> Self {
        self.fg_color = fg;
        self.bg_color = bg;
        self.focus_fg_color = focus_fg;
        self.focus_bg_color = focus_bg;
        self
    }

    /// Enable or disable the button
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Check if the button is currently enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Render the button with proper styling based on focus state
    fn render_button_text(&self, screen: &mut Screen, x: usize, y: usize) {
        let (width, _) = self.size;
        let (fg, bg) = if self.focused {
            (self.focus_fg_color, self.focus_bg_color)
        } else {
            (self.fg_color, self.bg_color)
        };

        // Clear the button area
        let empty_line = " ".repeat(width);
        screen.write_string(x, y, &empty_line, fg, bg);

        // Center the label within the button
        let _label_start = if width > self.label.len() {
            (width - self.label.len()) / 2
        } else {
            0
        };

        // Add brackets around the label for button appearance
        let button_text = if width >= self.label.len() + 2 {
            format!("[{}]", self.label)
        } else {
            self.label.clone()
        };

        // Calculate centered position for the bracketed text
        let text_start = if width > button_text.len() {
            (width - button_text.len()) / 2
        } else {
            0
        };

        screen.write_string(x + text_start, y, &button_text, fg, bg);
    }
}

impl Widget for Button {
    fn render(&self, screen: &mut Screen, x: usize, y: usize) {
        self.render_button_text(screen, x, y);

        // If focused, add blinking effect to make it more obvious
        if self.focused {
            // Re-render with blink attribute for extra visibility
            let (width, _) = self.size;
            let button_text = format!("[{}]", self.label);
            let text_start = if width > button_text.len() {
                (width - button_text.len()) / 2
            } else {
                0
            };

            screen.write_string_with_attributes(
                x + text_start,
                y,
                &button_text,
                self.focus_fg_color,
                self.focus_bg_color,
                &[CellAttribute::Blink],
            );
        }
    }

    fn get_size(&self) -> (usize, usize) {
        self.size
    }

    fn handle_event(&mut self, event: Event) -> Option<Event> {
        if !self.enabled {
            return Some(event);
        }

        match event {
            // Handle keyboard activation (Space or Enter when focused)
            Event::KeyPress { key: ' ' } | Event::KeyPress { key: '\r' } if self.focused => {
                Some(Event::ButtonClick {
                    button_id: self.id.clone(),
                })
            }
            Event::KeySpecial {
                key: SpecialKey::Enter,
            } if self.focused => Some(Event::ButtonClick {
                button_id: self.id.clone(),
            }),

            // Handle mouse clicks (coordinates will be checked by parent)
            Event::MouseClick { .. } => Some(Event::ButtonClick {
                button_id: self.id.clone(),
            }),

            // Pass through all other events
            _ => Some(event),
        }
    }

    fn can_focus(&self) -> bool {
        self.enabled
    }

    fn set_focus(&mut self, focused: bool) {
        self.focused = focused;
    }

    fn get_id(&self) -> Option<&str> {
        Some(&self.id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_button_creation() {
        let button = Button::new("test", "Click Me");
        assert_eq!(button.id, "test");
        assert_eq!(button.label, "Click Me");
        assert_eq!(button.size, (12, 1)); // "Click Me" + 4 padding = 12
        assert!(!button.focused);
        assert!(button.enabled);
    }

    #[test]
    fn test_button_focus() {
        let mut button = Button::new("test", "Test");
        assert!(!button.focused);
        assert!(button.can_focus());

        button.set_focus(true);
        assert!(button.focused);
    }

    #[test]
    fn test_button_events() {
        let mut button = Button::new("test", "Test");
        button.set_focus(true);

        // Test space key activation
        let result = button.handle_event(Event::KeyPress { key: ' ' });
        assert!(matches!(result, Some(Event::ButtonClick { button_id }) if button_id == "test"));

        // Test enter key activation
        let result = button.handle_event(Event::KeySpecial {
            key: SpecialKey::Enter,
        });
        assert!(matches!(result, Some(Event::ButtonClick { button_id }) if button_id == "test"));

        // Test disabled button
        button.set_enabled(false);
        let result = button.handle_event(Event::KeyPress { key: ' ' });
        assert!(matches!(result, Some(Event::KeyPress { key: ' ' })));
    }
}
