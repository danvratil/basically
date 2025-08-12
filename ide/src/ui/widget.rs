use crate::screen::Screen;
use crate::ui::event::{Event, WidgetIdType};

/// Core trait that all UI widgets must implement
/// Generic over the ID type for type-safe widget identification
pub trait Widget<Id: WidgetIdType> {
    /// Render the widget at the given coordinates relative to its parent
    /// (x, y) represents the top-left corner where this widget should draw itself
    fn render(&self, screen: &mut Screen, x: usize, y: usize);

    /// Get the size of this widget (width, height) in characters
    fn get_size(&self) -> (usize, usize);

    /// Handle an incoming event
    /// Returns None if the event was consumed, Some(event) to bubble up
    /// Default implementation just returns the event unchanged, i.e. no event handling.
    fn handle_event(&mut self, event: Event<Id>) -> Option<Event<Id>> {
        Some(event)
    }

    /// Whether this widget can receive focus.
    /// Returns `false` by default. Widgets that can be focused should override this.
    fn can_focus(&self) -> bool {
        false
    }

    /// Set the focus state of this widget
    /// Default implementation does nothing, widgets that can be focused should override this.
    fn set_focus(&mut self, _focused: bool) {}

    /// Check if a point (relative to this widget's parent) is within this widget's bounds
    /// Used for hit-testing mouse events
    fn contains_point(&self, x: usize, y: usize, widget_x: usize, widget_y: usize) -> bool {
        let (width, height) = self.get_size();
        x >= widget_x && x < widget_x + width && y >= widget_y && y < widget_y + height
    }

    /// Get mutable access to child widgets (if this widget is a container)
    /// Returns None for leaf widgets
    fn get_children(&mut self) -> Option<&mut Vec<Box<dyn Widget<Id>>>> {
        None
    }

    /// Get the widget's unique identifier (optional, used for debugging and events)
    fn get_id(&self) -> Option<&Id> {
        None
    }
}

/// Helper trait for widgets that contain other widgets
pub trait Container<Id: WidgetIdType>: Widget<Id> {
    /// Dispatch an event to the appropriate child widget
    fn dispatch_to_children(&mut self, event: Event<Id>) -> Option<Event<Id>>;

    /// Get the index of the currently focused child
    fn get_focused_child(&self) -> Option<usize>;

    /// Set focus to the next focusable child (for Tab navigation)
    fn focus_next_child(&mut self);

    /// Set focus to the previous focusable child (for Shift+Tab navigation)
    fn focus_previous_child(&mut self);

    /// Set focus to a specific child by index
    fn focus_child(&mut self, index: usize);
}

