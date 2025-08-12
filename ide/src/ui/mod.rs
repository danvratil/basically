//! UI Framework for QBasic IDE
//!
//! This module provides a hierarchical widget system with bidirectional event handling,
//! designed for authentic QBasic IDE recreation using character-based rendering.

pub mod button;
pub mod container;
pub mod event;
pub mod label;
pub mod widget;

// Re-export commonly used types
pub use button::Button;
pub use container::{ChildWidget, Panel};
pub use event::{DialogId, DialogResult, Event, MainId, MenuId, SpecialKey, WidgetIdType};
pub use label::Label;
pub use widget::{Container, Widget};

// Re-export Color from screen module for convenience
pub use crate::screen::Color;

/// Convenience function to create a new UI panel with MainId type
pub fn panel(width: usize, height: usize) -> Panel<MainId> {
    Panel::new(width, height)
}

/// Convenience function to create a new button with MainId type
pub fn button(id: MainId, label: impl Into<String>) -> Button<MainId> {
    Button::new(id, label)
}

/// Convenience function to create a new label with MainId type
pub fn label(text: impl Into<String>, fg_color: Color, bg_color: Color) -> Label<MainId> {
    Label::new(text, fg_color, bg_color)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convenience_functions() {
        let _panel = panel(80, 25);
        let _button = button("test", "Click Me");
        let _label = label("Hello", Color::White, Color::Black);
    }

    #[test]
    fn test_widget_composition() {
        // Test creating a simple UI with a panel containing buttons
        let ui = panel(40, 10)
            .add_child(Box::new(label("Welcome to QBasic IDE", Color::Yellow, Color::Blue)), 2, 1)
            .add_child(Box::new(button("ok", "OK")), 10, 8)
            .add_child(Box::new(button("cancel", "Cancel")), 20, 8);

        assert_eq!(ui.children.len(), 3);

        // Verify the panel can receive focus (because it has focusable children)
        assert!(ui.can_focus());
    }
}
