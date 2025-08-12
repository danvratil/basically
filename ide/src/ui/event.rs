use std::fmt::Debug;
use std::hash::Hash;

/// Trait that all widget ID types must implement
pub trait WidgetIdType: Debug + Clone + PartialEq + Hash + Send + Sync + 'static {
    /// Convert the ID to a string representation (for debugging/logging)
    fn as_str(&self) -> &'static str;
}

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

/// Generic event system - parameterized by widget ID type
#[derive(Debug, Clone)]
pub enum Event<Id: WidgetIdType> {
    // Input events (dispatched top-down)
    KeyPress { key: char },
    KeySpecial { key: SpecialKey },
    MouseClick { x: usize, y: usize },
    MouseMove { x: usize, y: usize },

    // Widget events (bubble up) - now type-safe!
    ButtonClick { button_id: Id },
    DialogResult { result: DialogResult },
    MenuSelect { menu_id: Id, item_id: Id },
    TextChanged { widget_id: Id, text: String },

    // System events
    FocusChanged { widget_id: Id },
    WindowResize,
    Quit,
}

impl<Id: WidgetIdType> Event<Id> {
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

// Example ID types for different widget contexts

/// Main application widget IDs
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum MainId {
    NewButton,
    OpenButton,
    SaveButton,
    ExitButton,
    StatusLabel,
    TitleLabel,
    InstructionsLabel,
}

impl WidgetIdType for MainId {
    fn as_str(&self) -> &'static str {
        match self {
            MainId::NewButton => "new",
            MainId::OpenButton => "open", 
            MainId::SaveButton => "save",
            MainId::ExitButton => "exit",
            MainId::StatusLabel => "status",
            MainId::TitleLabel => "title",
            MainId::InstructionsLabel => "instructions",
        }
    }
}

/// Menu system widget IDs
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum MenuId {
    FileMenu,
    EditMenu,
    ViewMenu,
    HelpMenu,
    FileNew,
    FileOpen,
    FileSave,
    FileExit,
}

impl WidgetIdType for MenuId {
    fn as_str(&self) -> &'static str {
        match self {
            MenuId::FileMenu => "file_menu",
            MenuId::EditMenu => "edit_menu",
            MenuId::ViewMenu => "view_menu", 
            MenuId::HelpMenu => "help_menu",
            MenuId::FileNew => "file_new",
            MenuId::FileOpen => "file_open",
            MenuId::FileSave => "file_save",
            MenuId::FileExit => "file_exit",
        }
    }
}

/// Dialog box widget IDs  
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum DialogId {
    OkButton,
    CancelButton,
    YesButton,
    NoButton,
    MessageLabel,
    TitleLabel,
}

impl WidgetIdType for DialogId {
    fn as_str(&self) -> &'static str {
        match self {
            DialogId::OkButton => "ok",
            DialogId::CancelButton => "cancel",
            DialogId::YesButton => "yes",
            DialogId::NoButton => "no",
            DialogId::MessageLabel => "message",
            DialogId::TitleLabel => "title",
        }
    }
}
