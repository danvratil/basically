use anyhow::Result;
use image::ImageReader;
use pixels::Pixels;
use std::time::Duration;
use winit::dpi::LogicalSize;

use crate::screen::{Cell, CellAttribute, Screen};

// IBM VGA font constants
const CHAR_WIDTH: usize = 9;
const CHAR_HEIGHT: usize = 16;
const WINDOW_WIDTH: u32 = (80 * CHAR_WIDTH) as u32;
const WINDOW_HEIGHT: u32 = (25 * CHAR_HEIGHT) as u32;

// Blinking text timing (matches VGA standard)
const BLINK_CYCLE_MS: u128 = 500;

// Font sprite sheet - 16x16 grid of characters
const FONT_DATA: &[u8] = include_bytes!("../assets/IBM_VGA_9x16.png");

// DOS color palette (16 colors)
const DOS_PALETTE: [[u8; 3]; 16] = [
    [0, 0, 0],       // Black
    [0, 0, 170],     // Blue
    [0, 170, 0],     // Green
    [0, 170, 170],   // Cyan
    [170, 0, 0],     // Red
    [170, 0, 170],   // Magenta
    [170, 85, 0],    // Brown
    [170, 170, 170], // Light Gray
    [85, 85, 85],    // Dark Gray
    [85, 85, 255],   // Light Blue
    [85, 255, 85],   // Light Green
    [85, 255, 255],  // Light Cyan
    [255, 85, 85],   // Light Red
    [255, 85, 255],  // Light Magenta
    [255, 255, 85],  // Yellow
    [255, 255, 255], // White
];

struct BitmapFont {
    image_data: Vec<u8>,
    width: u32,
    height: u32,
}

impl BitmapFont {
    fn new(font_data: &[u8]) -> Result<Self> {
        let img = ImageReader::new(std::io::Cursor::new(font_data))
            .with_guessed_format()?
            .decode()?;

        let rgba_img = img.to_rgba8();
        Ok(BitmapFont {
            image_data: rgba_img.into_raw(),
            width: img.width(),
            height: img.height(),
        })
    }

    fn get_char_pixels(&self, char_code: u8) -> Vec<u8> {
        // 16x16 grid layout
        let char_x = (char_code % 16) as u32;
        let char_y = (char_code / 16) as u32;

        let mut char_data = Vec::with_capacity(CHAR_WIDTH * CHAR_HEIGHT);

        for y in 0..CHAR_HEIGHT {
            for x in 0..CHAR_WIDTH {
                let sprite_x = char_x * CHAR_WIDTH as u32 + x as u32;
                let sprite_y = char_y * CHAR_HEIGHT as u32 + y as u32;

                if sprite_x < self.width && sprite_y < self.height {
                    let pixel_idx = ((sprite_y * self.width + sprite_x) * 4) as usize;
                    // Use alpha channel for bitmap (white = foreground, transparent = background)
                    let alpha = if pixel_idx + 3 < self.image_data.len() {
                        self.image_data[pixel_idx + 3]
                    } else {
                        0
                    };
                    char_data.push(alpha);
                } else {
                    char_data.push(0);
                }
            }
        }

        char_data
    }
}

pub struct Renderer {
    font: BitmapFont,
    start_time: std::time::Instant,
}

impl Renderer {
    pub fn new() -> Result<Self> {
        let font = BitmapFont::new(FONT_DATA)?;
        println!(
            "Successfully loaded bitmap font: {}x{}",
            font.width, font.height
        );

        Ok(Renderer {
            font,
            start_time: std::time::Instant::now(),
        })
    }

    pub fn render(&self, pixels: &mut Pixels, screen: &Screen) -> Result<()> {
        let frame = pixels.frame_mut();
        let elapsed = self.start_time.elapsed();

        // Clear frame to black
        frame.fill(0);

        for (row, line) in screen.iter().enumerate() {
            for (col, cell) in line.iter().enumerate() {
                self.render_cell(frame, col, row, cell, elapsed);
            }
        }

        pixels.render()?;
        Ok(())
    }

    // Helper function to set a single pixel with bounds checking
    fn set_pixel(frame: &mut [u8], x: usize, y: usize, color: [u8; 3]) -> bool {
        if x >= WINDOW_WIDTH as usize || y >= WINDOW_HEIGHT as usize {
            return false;
        }

        let frame_idx = (y * WINDOW_WIDTH as usize + x) * 4;
        if frame_idx + 3 < frame.len() {
            frame[frame_idx] = color[0]; // R
            frame[frame_idx + 1] = color[1]; // G
            frame[frame_idx + 2] = color[2]; // B
            frame[frame_idx + 3] = 255; // A
            true
        } else {
            false
        }
    }

    // Helper function to fill a rectangle with solid color
    fn fill_rect(
        frame: &mut [u8],
        x: usize,
        y: usize,
        width: usize,
        height: usize,
        color: [u8; 3],
    ) {
        for dy in 0..height {
            for dx in 0..width {
                Self::set_pixel(frame, x + dx, y + dy, color);
            }
        }
    }

    // Helper function to render a solid cell (for blinking-off text)
    fn render_solid_cell(frame: &mut [u8], x: usize, y: usize, color: [u8; 3]) {
        Self::fill_rect(frame, x, y, CHAR_WIDTH, CHAR_HEIGHT, color);
    }

    // Helper function to render a character cell with bitmap
    fn render_char_cell(
        &self,
        frame: &mut [u8],
        x: usize,
        y: usize,
        char: u8,
        fg_color: [u8; 3],
        bg_color: [u8; 3],
    ) {
        let char_pixels = self.font.get_char_pixels(char);

        for dy in 0..CHAR_HEIGHT {
            for dx in 0..CHAR_WIDTH {
                let char_pixel_idx = dy * CHAR_WIDTH + dx;
                let is_foreground = if char_pixel_idx < char_pixels.len() {
                    char_pixels[char_pixel_idx] > 128 // Simple threshold
                } else {
                    false
                };

                let color = if is_foreground { fg_color } else { bg_color };
                Self::set_pixel(frame, x + dx, y + dy, color);
            }
        }
    }

    fn render_cell(
        &self,
        frame: &mut [u8],
        col: usize,
        row: usize,
        cell: &Cell,
        elapsed: Duration,
    ) {
        let x = col * CHAR_WIDTH;
        let y = row * CHAR_HEIGHT;

        // Check if cell has blink attribute
        let is_blinking = cell.attributes.contains(&CellAttribute::Blink);

        // Calculate blink state
        let should_show_fg = if is_blinking {
            (elapsed.as_millis() / BLINK_CYCLE_MS) % 2 == 0
        } else {
            true
        };

        let bg_color = DOS_PALETTE[cell.bg_color as usize & 15];

        // Optimization: if blinking text is "off", just fill with background color
        if is_blinking && !should_show_fg {
            Self::render_solid_cell(frame, x, y, bg_color);
            return;
        }

        // Normal rendering path for visible text
        let fg_color = DOS_PALETTE[cell.fg_color as usize & 15];
        self.render_char_cell(frame, x, y, cell.char, fg_color, bg_color);
    }
}

pub fn get_window_size() -> LogicalSize<u32> {
    LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT)
}
