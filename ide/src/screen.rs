#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CellAttribute {
    Blink,
}

/// DOS/VGA 16-color palette
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Color {
    Black = 0,
    Blue = 1,
    Green = 2,
    Cyan = 3,
    Red = 4,
    Magenta = 5,
    Brown = 6,
    LightGray = 7,
    DarkGray = 8,
    LightBlue = 9,
    LightGreen = 10,
    LightCyan = 11,
    LightRed = 12,
    LightMagenta = 13,
    Yellow = 14,
    White = 15,
}

impl Color {
    /// Convert color to its numeric value (for compatibility with existing code)
    pub fn as_u8(self) -> u8 {
        self as u8
    }
    
    /// Create a color from a numeric value (0-15)
    pub fn from_u8(value: u8) -> Self {
        match value & 15 {
            0 => Color::Black,
            1 => Color::Blue,
            2 => Color::Green,
            3 => Color::Cyan,
            4 => Color::Red,
            5 => Color::Magenta,
            6 => Color::Brown,
            7 => Color::LightGray,
            8 => Color::DarkGray,
            9 => Color::LightBlue,
            10 => Color::LightGreen,
            11 => Color::LightCyan,
            12 => Color::LightRed,
            13 => Color::LightMagenta,
            14 => Color::Yellow,
            15 => Color::White,
            _ => Color::White, // Default fallback
        }
    }
    
    /// Get the RGB values for this color (matches DOS VGA palette)
    pub fn to_rgb(self) -> [u8; 3] {
        match self {
            Color::Black => [0, 0, 0],
            Color::Blue => [0, 0, 170],
            Color::Green => [0, 170, 0],
            Color::Cyan => [0, 170, 170],
            Color::Red => [170, 0, 0],
            Color::Magenta => [170, 0, 170],
            Color::Brown => [170, 85, 0],
            Color::LightGray => [170, 170, 170],
            Color::DarkGray => [85, 85, 85],
            Color::LightBlue => [85, 85, 255],
            Color::LightGreen => [85, 255, 85],
            Color::LightCyan => [85, 255, 255],
            Color::LightRed => [255, 85, 85],
            Color::LightMagenta => [255, 85, 255],
            Color::Yellow => [255, 255, 85],
            Color::White => [255, 255, 255],
        }
    }
    
    /// Get all available colors (useful for color picker widgets)
    pub fn all() -> [Color; 16] {
        [
            Color::Black,
            Color::Blue,
            Color::Green,
            Color::Cyan,
            Color::Red,
            Color::Magenta,
            Color::Brown,
            Color::LightGray,
            Color::DarkGray,
            Color::LightBlue,
            Color::LightGreen,
            Color::LightCyan,
            Color::LightRed,
            Color::LightMagenta,
            Color::Yellow,
            Color::White,
        ]
    }
}

#[derive(Clone, Debug)]
pub struct Cell {
    pub char: u8,
    pub fg_color: Color,
    pub bg_color: Color,
    pub attributes: Vec<CellAttribute>,
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            char: b' ',
            fg_color: Color::LightGray,
            bg_color: Color::Black,
            attributes: Vec::new(),
        }
    }
}

pub const SCREEN_WIDTH: usize = 80;
pub const SCREEN_HEIGHT: usize = 25;

#[derive(Clone, Debug)]
pub struct Screen {
    cells: [[Cell; SCREEN_WIDTH]; SCREEN_HEIGHT],
}

impl Screen {
    pub fn new() -> Self {
        Screen {
            cells: std::array::from_fn(|_| std::array::from_fn(|_| Cell::default())),
        }
    }

    pub fn clear(&mut self) {
        for row in self.cells.iter_mut() {
            for cell in row.iter_mut() {
                *cell = Cell::default();
            }
        }
    }

    pub fn write_string(&mut self, x: usize, y: usize, text: &str, fg_color: Color, bg_color: Color) {
        self.write_string_with_attributes(x, y, text, fg_color, bg_color, &[]);
    }

    pub fn write_string_with_attributes(
        &mut self,
        x: usize,
        y: usize,
        text: &str,
        fg_color: Color,
        bg_color: Color,
        attributes: &[CellAttribute],
    ) {
        for (i, ch) in text.bytes().enumerate() {
            if x + i >= SCREEN_WIDTH || y >= SCREEN_HEIGHT {
                break;
            }
            self.cells[y][x + i] = Cell {
                char: ch,
                fg_color,
                bg_color,
                attributes: attributes.to_vec(),
            };
        }
    }
    
    /// Legacy method for compatibility with u8 colors - converts to Color enum
    pub fn write_string_u8(&mut self, x: usize, y: usize, text: &str, fg_color: u8, bg_color: u8) {
        self.write_string(x, y, text, Color::from_u8(fg_color), Color::from_u8(bg_color));
    }

    pub fn iter(&self) -> impl Iterator<Item = &[Cell; SCREEN_WIDTH]> {
        self.cells.iter()
    }
}
