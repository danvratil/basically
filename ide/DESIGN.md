# QBasic IDE Architecture Design

This document outlines the architecture for the QBasic 1.1 IDE implementation using modern Rust libraries while maintaining authentic DOS-era look and feel.

## Technology Stack

- **winit**: Window creation and event handling
- **pixels**: CPU-based framebuffer rendering
- **image**: PNG sprite sheet loading for bitmap fonts

## Core Architecture

### Character Buffer System

The IDE uses an authentic 80x25 character grid system mimicking DOS text mode:

```rust
struct Cell {
    char: u8,           // ASCII/CP437 character
    fg_color: u8,       // 4-bit foreground color
    bg_color: u8,       // 4-bit background color  
    attributes: u8,     // bold, blink, etc.
}

type Screen = [[Cell; SCREEN_WIDTH]; SCREEN_HEIGHT];
const SCREEN_WIDTH: usize = 80;
const SCREEN_HEIGHT: usize = 25;
```

### Single-Threaded Architecture

- **Main Thread**: 
  - Handles winit events and user input
  - Updates Screen buffer with UI state
  - Manages IDE logic (editing, menus, dialogs)
  - Renders characters directly to pixels framebuffer
  - Presents final framebuffer to window

### Font System

- **Bitmap Font**: Uses authentic IBM VGA 9x16 bitmap font
- **Sprite Sheet**: 16x16 grid PNG containing all 256 CP437 characters
- **Character Dimensions**: 9 pixels wide × 16 pixels tall
- **Window Size**: 720×400 pixels (80×25 characters)

## Rendering Pipeline

```rust
struct BitmapFont {
    image_data: Vec<u8>,    // RGBA pixel data from sprite sheet
    width: u32,             // Sprite sheet width (144 pixels for 16×9px chars)
    height: u32,            // Sprite sheet height (256 pixels for 16×16px chars)
}
```

1. **Character Lookup**: For each cell in 80×25 grid:
   - Calculate sprite sheet position: `(char_code % 16, char_code / 16)`
   - Extract 9×16 pixel bitmap from sprite sheet
   - Use alpha channel as bitmap mask (opaque = foreground, transparent = background)

2. **Color Application**: 
   - Apply authentic DOS 16-color palette
   - Simple threshold: alpha > 128 = foreground color, else background color

3. **Framebuffer Rendering**:
   - Direct pixel-by-pixel rendering to pixels crate framebuffer
   - No anti-aliasing or font hinting - pure bitmap rendering
   - Present final 720×400 framebuffer to window

## UI Framework

### Design Philosophy
- Implement custom UI framework tailored to QBasic's specific patterns
- Modal dialogs, menu bars, text areas with authentic behavior
- Retained UI model (not immediate mode like modern frameworks)

### Core UI Components

```rust
trait Widget {
    fn handle_key(&mut self, key: Key) -> bool;       // Returns true if key consumed
    fn handle_mouse(&mut self, mouse: MouseEvent) -> bool;
    fn render(&self, screen: &mut Screen, x: i32, y: i32);
    fn can_focus(&self) -> bool;
}

struct Dialog {
    widgets: Vec<Box<dyn Widget>>,
    focused_widget: usize,
    // Layout and positioning info
}
```

### UI Elements to Implement
- Modal dialogs (File Open, About, etc.)
- Menu bar with keyboard shortcuts (Alt+F, etc.)
- Text editing areas with cursor and selection
- Simple widgets: buttons, checkboxes, input fields
- Focus chain management (Tab/Shift+Tab navigation)

## Why This Approach

### Advantages over ratatui
- **Authentic appearance**: Pixel-perfect recreation with exact DOS colors
- **Font control**: Use actual IBM VGA bitmap fonts from DOS era
- **Consistent experience**: Same visual output across all platforms
- **Full control**: Handle blinking cursor, selection, modals exactly like original

### Advantages over modern UI frameworks
- **Perfect fit**: Tailored specifically to QBasic's UI patterns
- **Simplicity**: Minimal, focused implementation
- **Educational value**: Understanding low-level UI concepts
- **Authenticity**: Maintains the exact look and feel of original QBasic IDE

## Implementation Notes

### Font Rendering Evolution
- **Initial approach**: Started with fontdue for TTF rendering but encountered alignment issues
- **crossfont attempt**: Tried crossfont (used by Alacritty) but bitmap format was complex
- **Final solution**: IBM VGA bitmap font sprite sheet - simple, authentic, and pixel-perfect

### Performance Benefits
- **Zero font processing**: Direct bitmap lookups, no glyph rasterization
- **Predictable layout**: Every character is exactly 9×16 pixels
- **Minimal dependencies**: Only `image` crate for PNG loading
- **Authentic appearance**: Matches original DOS/VGA text mode exactly

## UI Framework Architecture (v2)

### Widget System Design

The UI framework implements a hierarchical widget system with bidirectional event handling, designed for authentic QBasic IDE recreation.

#### Core Widget Trait

```rust
trait Widget {
    // Rendering (relative coordinates)
    fn render(&self, screen: &mut Screen, x: usize, y: usize);
    fn get_size(&self) -> (usize, usize);
    
    // Event handling (bidirectional)
    fn handle_event(&mut self, event: Event) -> Option<Event>; // None = consumed, Some = bubble up
    
    // Focus management
    fn can_focus(&self) -> bool;
    fn set_focus(&mut self, focused: bool);
    fn contains_point(&self, x: usize, y: usize) -> bool;
    
    // Child widget support
    fn get_children(&mut self) -> Option<&mut Vec<Box<dyn Widget>>>;
}
```

#### Event System

**Event Types:**
```rust
#[derive(Debug, Clone)]
enum Event {
    // Input events (dispatched top-down)
    KeyPress { key: char },
    KeySpecial { key: SpecialKey }, // Tab, Arrow keys, etc.
    MouseClick { x: usize, y: usize },
    
    // Widget events (bubble up)
    ButtonClick { button_id: String },
    DialogResult { result: DialogResult },
    MenuSelect { menu_id: String, item_id: String },
    
    // System events
    FocusChanged { widget_id: String },
    WindowResize,
}
```

### Bidirectional Event Flow

#### 1. Event Dispatch (Top → Down)
Events enter at the root widget and are routed to appropriate children:

- **Keyboard Events**: Sent to currently focused widget
- **Mouse Events**: Hit-tested by coordinates to find target widget  
- **Global Events**: Broadcast to all widgets (like resize)

```rust
impl Container {
    fn dispatch_event(&mut self, event: Event) -> Option<Event> {
        match event {
            Event::KeyPress(_) | Event::KeySpecial(_) => {
                // Route to focused child
                if let Some(idx) = self.focused_child {
                    self.children[idx].handle_event(event)
                } else {
                    Some(event) // No focus, bubble up
                }
            }
            Event::MouseClick { x, y } => {
                // Hit-test to find target child
                for child in &mut self.children {
                    if child.contains_point(x, y) {
                        return child.handle_event(event);
                    }
                }
                Some(event) // No hit, bubble up
            }
        }
    }
}
```

#### 2. Event Bubbling (Bottom → Up)
Child widgets can:
- **Consume events**: Return `None` to stop propagation
- **Transform events**: Convert `ButtonClick` → `DialogResult`
- **Pass through**: Return `Some(event)` unchanged

### Widget Hierarchy Examples

#### Basic Button
```rust
struct Button {
    label: String,
    id: String,
    focused: bool,
    size: (usize, usize),
}

impl Widget for Button {
    fn handle_event(&mut self, event: Event) -> Option<Event> {
        match event {
            Event::KeyPress { key: ' ' } | Event::KeyPress { key: '\r' } if self.focused => {
                Some(Event::ButtonClick { button_id: self.id.clone() })
            }
            Event::MouseClick { x, y } if self.contains_point(x, y) => {
                Some(Event::ButtonClick { button_id: self.id.clone() })
            }
            _ => Some(event) // Pass through unhandled events
        }
    }
}
```

#### Dialog Container
```rust
struct Dialog {
    children: Vec<Box<dyn Widget>>,
    focused_child: Option<usize>,
    title: String,
}

impl Widget for Dialog {
    fn handle_event(&mut self, event: Event) -> Option<Event> {
        match event {
            Event::ButtonClick { button_id } => {
                // Transform button clicks into dialog results
                match button_id.as_str() {
                    "ok" => Some(Event::DialogResult { result: DialogResult::Ok }),
                    "cancel" => Some(Event::DialogResult { result: DialogResult::Cancel }),
                    _ => self.dispatch_to_children(event)
                }
            }
            Event::KeySpecial { key: SpecialKey::Tab } => {
                self.focus_next_child();
                None // Consume tab navigation
            }
            _ => self.dispatch_to_children(event)
        }
    }
}
```

### Focus Management

**Focus Chain**: Widgets maintain focus order for Tab navigation
**Focus Rules**:
- Only one widget has focus at a time per container
- Tab/Shift+Tab moves focus between focusable widgets
- Mouse clicks change focus to clicked widget
- Modal dialogs capture focus exclusively

### Coordinate System

**Relative Positioning**: All coordinates are relative to parent widget
- Widget at (0,0) renders at its parent's origin
- Simplifies widget composition and layout calculations
- Makes widget reuse across different containers seamless

### Widget Layout Strategy

**Fixed Positioning** (initial implementation):
- Widgets specify exact (x, y, width, height) within their parent
- Simple and predictable for dialog-based UI
- Matches QBasic's fixed-layout approach

**Future**: Could evolve to automatic layout (flexbox-style) for more complex UIs

### Implementation Phases

1. **Phase 1**: Core trait + Button + Container
2. **Phase 2**: Focus management + keyboard navigation  
3. **Phase 3**: TextBox + Menu + Dialog compositions
4. **Phase 4**: Modal dialog system + authentic QBasic widgets
5. **Phase 5**: Full IDE layout (MenuBar + TextEditor + StatusBar)

This design provides the flexibility for complex UI compositions while maintaining the authentic feel of the original QBasic IDE.

## Future Considerations

- Sound effects (PC speaker beeps) can be added easily with winit
- Mouse support will be more reliable than terminal-based solutions
- Windowed debugging views can be implemented when needed
- Bitmap font approach supports full CP437 character set authentically