#[derive(Debug, Clone, PartialEq)]
pub enum SpecialKey {
    Tab,
    ShiftTab,
    Enter,
    Escape,
    Up,
    Down,
    Left,
    Right,
    Home,
    End,
    PageUp,
    PageDown,
    Delete,
    Backspace,
    Insert,
}

#[derive(Debug, Clone)]
pub enum DialogResult {
    Ok,
    Cancel,
    Yes,
    No,
    Custom(String),
}

#[derive(Debug, Clone)]
pub enum Event {
    // Input events (dispatched top-down)
    KeyPress { key: char },
    KeySpecial { key: SpecialKey },
    MouseClick { x: usize, y: usize },
    MouseMove { x: usize, y: usize },

    // Widget events (bubble up)
    ButtonClick { button_id: String },
    DialogResult { result: DialogResult },
    MenuSelect { menu_id: String, item_id: String },
    TextChanged { widget_id: String, text: String },

    // System events
    FocusChanged { widget_id: String },
    WindowResize,
    Quit,
}

impl Event {
    pub fn is_input_event(&self) -> bool {
        matches!(
            self,
            Event::KeyPress { .. }
                | Event::KeySpecial { .. }
                | Event::MouseClick { .. }
                | Event::MouseMove { .. }
        )
    }

    pub fn is_keyboard_event(&self) -> bool {
        matches!(self, Event::KeyPress { .. } | Event::KeySpecial { .. })
    }

    pub fn is_mouse_event(&self) -> bool {
        matches!(self, Event::MouseClick { .. } | Event::MouseMove { .. })
    }
}
