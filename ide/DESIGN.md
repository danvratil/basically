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

## Future Considerations

- Sound effects (PC speaker beeps) can be added easily with winit
- Mouse support will be more reliable than terminal-based solutions
- Windowed debugging views can be implemented when needed
- Bitmap font approach supports full CP437 character set authentically