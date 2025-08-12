#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CellAttribute {
    Blink,
}

#[derive(Clone, Debug)]
pub struct Cell {
    pub char: u8,
    pub fg_color: u8, // 0-15 actual color
    pub bg_color: u8, // 0-15 actual color
    pub attributes: Vec<CellAttribute>,
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            char: b' ',
            fg_color: 7, // White
            bg_color: 0, // Black
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

    pub fn write_string(&mut self, x: usize, y: usize, text: &str, fg_color: u8, bg_color: u8) {
        self.write_string_with_attributes(x, y, text, fg_color, bg_color, &[]);
    }

    pub fn write_string_with_attributes(
        &mut self,
        x: usize,
        y: usize,
        text: &str,
        fg_color: u8,
        bg_color: u8,
        attributes: &[CellAttribute],
    ) {
        for (i, ch) in text.bytes().enumerate() {
            if x + i >= SCREEN_WIDTH || y >= SCREEN_HEIGHT {
                break;
            }
            self.cells[y][x + i] = Cell {
                char: ch,
                fg_color: fg_color & 15, // Ensure color is 0-15
                bg_color: bg_color & 15, // Ensure color is 0-15
                attributes: attributes.to_vec(),
            };
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &[Cell; SCREEN_WIDTH]> {
        self.cells.iter()
    }
}
