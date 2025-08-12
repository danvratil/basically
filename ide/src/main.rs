mod renderer;
mod screen;

use anyhow::Result;
use pixels::{Pixels, SurfaceTexture};
use std::sync::Arc;
use winit::{
    application::ApplicationHandler,
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    window::{Window, WindowId},
};

use renderer::{Renderer, get_window_size};
use screen::{CellAttribute, Screen};

struct App {
    window: Option<Arc<Window>>,
    pixels: Option<Pixels<'static>>,
    renderer: Option<Renderer>,
    screen: Screen,
}

impl App {
    fn new() -> Self {
        let mut screen = Screen::new();

        // Write sample text to the screen buffer
        screen.write_string(10, 10, "Hello World", 15, 1); // White on blue
        screen.write_string(10, 12, "QBasic IDE Renderer PoC", 14, 0); // Yellow on black
        screen.write_string_with_attributes(10, 14, "BLINKING TEXT!", 15, 0, &[CellAttribute::Blink]); // Blinking white on black
        screen.write_string(10, 16, "Regular and ", 11, 0); // Light cyan
        screen.write_string_with_attributes(22, 16, "BLINKING", 12, 4, &[CellAttribute::Blink]); // Blinking light red on red background

        Self {
            window: None,
            pixels: None,
            renderer: None,
            screen,
        }
    }

    fn render(&mut self) -> Result<()> {
        if let (Some(pixels), Some(renderer)) = (&mut self.pixels, &self.renderer) {
            renderer.render(pixels, &self.screen)?;
        }
        Ok(())
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_none() {
            let window_attributes = Window::default_attributes()
                .with_title("QBasic IDE - Renderer PoC")
                .with_inner_size(get_window_size())
                .with_resizable(false);

            match event_loop.create_window(window_attributes) {
                Ok(window) => {
                    let arc_window = Arc::new(window);
                    let window_size = arc_window.inner_size();
                    let surface_texture = SurfaceTexture::new(
                        window_size.width,
                        window_size.height,
                        arc_window.clone(),
                    );

                    match Pixels::new(get_window_size().width, get_window_size().height, surface_texture) {
                        Ok(pixels) => {
                            // Create renderer
                            match Renderer::new() {
                                Ok(renderer) => {
                                    self.pixels = Some(pixels);
                                    self.renderer = Some(renderer);
                                    self.window = Some(arc_window);

                                    // Trigger initial render
                                    if let Some(window) = &self.window {
                                        window.request_redraw();
                                    }
                                }
                                Err(e) => {
                                    eprintln!("Failed to create renderer: {}", e);
                                    event_loop.exit();
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("Failed to create pixels: {}", e);
                            event_loop.exit();
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Failed to create window: {}", e);
                    event_loop.exit();
                }
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::Resized(size) => {
                if let Some(pixels) = &mut self.pixels {
                    if let Err(e) = pixels.resize_surface(size.width, size.height) {
                        eprintln!("Failed to resize pixels: {}", e);
                    }
                }
            }
            WindowEvent::RedrawRequested => {
                if let Err(e) = self.render() {
                    eprintln!("Failed to render: {}", e);
                }

                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }
            _ => {}
        }
    }
}

fn main() -> Result<()> {
    let event_loop = EventLoop::new()?;
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = App::new();
    event_loop.run_app(&mut app)?;

    Ok(())
}
