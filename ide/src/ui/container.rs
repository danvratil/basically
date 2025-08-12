use crate::screen::{Color, Screen};
use crate::ui::event::{Event, SpecialKey, WidgetIdType};
use crate::ui::widget::{Container, Widget};

/// A widget that contains and manages other widgets
/// Handles layout, focus management, and event dispatch
#[derive(Debug)]
pub struct Panel<Id: WidgetIdType> {
    pub children: Vec<ChildWidget<Id>>,
    focused_child: Option<usize>,
    size: (usize, usize),
    bg_color: Color,
}

/// Represents a child widget with its position within the container
pub struct ChildWidget<Id: WidgetIdType> {
    pub widget: Box<dyn Widget<Id>>,
    pub position: (usize, usize), // (x, y) relative to container
}

impl<Id: WidgetIdType> std::fmt::Debug for ChildWidget<Id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ChildWidget")
            .field("widget_id", &self.widget.get_id())
            .field("position", &self.position)
            .finish()
    }
}

impl<Id: WidgetIdType> Panel<Id> {
    /// Create a new empty container with the specified size
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            children: Vec::new(),
            focused_child: None,
            size: (width, height),
            bg_color: Color::Black,
        }
    }

    /// Set the background color for the container
    pub fn with_bg_color(mut self, bg_color: Color) -> Self {
        self.bg_color = bg_color;
        self
    }

    /// Add a child widget at the specified position
    pub fn add_child(mut self, widget: Box<dyn Widget<Id>>, x: usize, y: usize) -> Self {
        self.children.push(ChildWidget {
            widget,
            position: (x, y),
        });
        self
    }

    /// Add a child widget and return a mutable reference to self for chaining
    pub fn add_child_mut(&mut self, widget: Box<dyn Widget<Id>>, x: usize, y: usize) -> &mut Self {
        self.children.push(ChildWidget {
            widget,
            position: (x, y),
        });
        self
    }

    /// Find the child widget that contains the given point
    fn find_child_at_point(&self, x: usize, y: usize) -> Option<usize> {
        for (index, child) in self.children.iter().enumerate() {
            let (child_x, child_y) = child.position;
            if child.widget.contains_point(x, y, child_x, child_y) {
                return Some(index);
            }
        }
        None
    }

    /// Set focus to the first focusable child
    fn focus_first_child(&mut self) {
        for (index, child) in self.children.iter().enumerate() {
            if child.widget.can_focus() {
                self.focus_child(index);
                return;
            }
        }
        self.focused_child = None;
    }

    /// Handle tab navigation - common logic for containers
    fn handle_tab_navigation(&mut self, shift_pressed: bool) -> Option<Event<Id>> {
        if shift_pressed {
            self.focus_previous_child();
        } else {
            self.focus_next_child();
        }
        None // Consume the tab event
    }

    /// Clear focus from the currently focused child
    fn clear_current_focus(&mut self) {
        if let Some(index) = self.focused_child {
            if let Some(child) = self.children.get_mut(index) {
                child.widget.set_focus(false);
            }
        }
    }
}

impl<Id: WidgetIdType> Widget<Id> for Panel<Id> {
    fn render(&self, screen: &mut Screen, x: usize, y: usize) {
        // First, clear the container's background
        let (width, height) = self.size;
        for row in 0..height {
            let empty_line = " ".repeat(width);
            screen.write_string(x, y + row, &empty_line, Color::White, self.bg_color);
        }

        // Then render all child widgets at their positions
        for child in &self.children {
            let (child_x, child_y) = child.position;
            child.widget.render(screen, x + child_x, y + child_y);
        }
    }

    fn get_size(&self) -> (usize, usize) {
        self.size
    }

    fn handle_event(&mut self, event: Event<Id>) -> Option<Event<Id>> {
        match &event {
            // Handle tab navigation
            Event::KeySpecial {
                key: SpecialKey::Tab,
            } => self.handle_tab_navigation(false),
            Event::KeySpecial {
                key: SpecialKey::ShiftTab,
            } => self.handle_tab_navigation(true),

            // Handle mouse clicks - find which child to send to
            Event::MouseClick { x, y } => {
                if let Some(child_index) = self.find_child_at_point(*x, *y) {
                    // Set focus to the clicked child if it can receive focus
                    if self.children[child_index].widget.can_focus() {
                        self.focus_child(child_index);
                    }

                    // Forward the event to the child
                    if let Some(child) = self.children.get_mut(child_index) {
                        return child.widget.handle_event(event);
                    }
                }
                Some(event) // No child hit, bubble up
            }

            // Forward keyboard events to focused child
            _ if event.is_keyboard_event() => {
                if let Some(focused_index) = self.focused_child {
                    if let Some(child) = self.children.get_mut(focused_index) {
                        return child.widget.handle_event(event);
                    }
                }
                Some(event) // No focused child, bubble up
            }

            // For all other events, try dispatching to children
            _ => self.dispatch_to_children(event),
        }
    }

    fn can_focus(&self) -> bool {
        // A container can receive focus if any of its children can receive focus
        self.children.iter().any(|child| child.widget.can_focus())
    }

    fn set_focus(&mut self, focused: bool) {
        if focused && self.focused_child.is_none() {
            // If we're gaining focus and no child is focused, focus the first focusable child
            self.focus_first_child();
        } else if !focused {
            // If we're losing focus, clear focus from current child
            self.clear_current_focus();
            self.focused_child = None;
        }
    }

    fn get_children(&mut self) -> Option<&mut Vec<Box<dyn Widget<Id>>>> {
        // This is a bit tricky because our children are wrapped in ChildWidget
        // For now, return None - containers that need this can implement a custom method
        None
    }
}

impl<Id: WidgetIdType> Container<Id> for Panel<Id> {
    fn dispatch_to_children(&mut self, event: Event<Id>) -> Option<Event<Id>> {
        // Try sending the event to all children until one consumes it
        for child in &mut self.children {
            if let None = child.widget.handle_event(event.clone()) {
                return None; // Event was consumed
            }
        }
        Some(event) // No child consumed the event, bubble up
    }

    fn get_focused_child(&self) -> Option<usize> {
        self.focused_child
    }

    fn focus_next_child(&mut self) {
        let start_index = self.focused_child.map(|i| i + 1).unwrap_or(0);

        // Look for the next focusable child
        for i in start_index..self.children.len() {
            if self.children[i].widget.can_focus() {
                self.focus_child(i);
                return;
            }
        }

        // Wrap around to the beginning
        for i in 0..start_index {
            if self.children[i].widget.can_focus() {
                self.focus_child(i);
                return;
            }
        }
    }

    fn focus_previous_child(&mut self) {
        let start_index = self.focused_child.unwrap_or(0);

        // Look backwards for a focusable child
        if start_index > 0 {
            for i in (0..start_index).rev() {
                if self.children[i].widget.can_focus() {
                    self.focus_child(i);
                    return;
                }
            }
        }

        // Wrap around to the end
        for i in (start_index + 1..self.children.len()).rev() {
            if self.children[i].widget.can_focus() {
                self.focus_child(i);
                return;
            }
        }
    }

    fn focus_child(&mut self, index: usize) {
        // Clear focus from current child
        self.clear_current_focus();

        // Set focus to new child
        if let Some(child) = self.children.get_mut(index) {
            if child.widget.can_focus() {
                child.widget.set_focus(true);
                self.focused_child = Some(index);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ui::button::Button;
    use crate::ui::widget::Label;

    #[test]
    fn test_container_creation() {
        let container = Panel::new(20, 10);
        assert_eq!(container.size, (20, 10));
        assert_eq!(container.children.len(), 0);
        assert!(container.focused_child.is_none());
    }

    #[test]
    fn test_adding_children() {
        let container = Panel::new(20, 10)
            .add_child(Box::new(Label::new("Test", 15, 0)), 2, 3)
            .add_child(Box::new(Button::new("btn1", "Click")), 5, 7);

        assert_eq!(container.children.len(), 2);
        assert_eq!(container.children[0].position, (2, 3));
        assert_eq!(container.children[1].position, (5, 7));
    }

    #[test]
    fn test_focus_management() {
        let mut container = Panel::new(20, 10)
            .add_child(Box::new(Label::new("Label", 15, 0)), 0, 0) // Can't focus
            .add_child(Box::new(Button::new("btn1", "Button1")), 0, 2) // Can focus
            .add_child(Box::new(Button::new("btn2", "Button2")), 0, 4); // Can focus

        // Initially no focus
        assert!(container.focused_child.is_none());

        // Setting focus should focus first focusable child
        container.set_focus(true);
        assert_eq!(container.focused_child, Some(1)); // Button1

        // Tab should move to next focusable child
        container.focus_next_child();
        assert_eq!(container.focused_child, Some(2)); // Button2

        // Tab again should wrap to first
        container.focus_next_child();
        assert_eq!(container.focused_child, Some(1)); // Button1
    }
}
