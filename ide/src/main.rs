mod renderer;
mod screen;
mod ui;

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
use ui::{Event, Widget, button, label, panel};

struct App {
    window: Option<Arc<Window>>,
    pixels: Option<Pixels<'static>>,
    renderer: Option<Renderer>,
    screen: Screen,
    ui: ui::Panel,
}

impl App {
    fn new() -> Self {
        let screen = Screen::new();

        // Create a simple UI to demonstrate the widget system
        let mut ui = panel(80, 25)
            .add_child(
                Box::new(label("QBasic IDE - Widget System Demo", 14, 1)),
                20,
                2,
            )
            .add_child(
                Box::new(label(
                    "Use Tab to navigate, Space to activate buttons",
                    15,
                    0,
                )),
                15,
                4,
            )
            .add_child(Box::new(button("new", "New")), 10, 8)
            .add_child(Box::new(button("open", "Open")), 20, 8)
            .add_child(Box::new(button("save", "Save")), 30, 8)
            .add_child(Box::new(button("exit", "Exit")), 40, 8)
            .add_child(Box::new(label("Status: Ready", 11, 0)), 2, 23);

        // Initialize focus
        ui.set_focus(true);

        Self {
            window: None,
            pixels: None,
            renderer: None,
            screen,
            ui,
        }
    }

    fn render(&mut self) -> Result<()> {
        if let (Some(pixels), Some(renderer)) = (&mut self.pixels, &self.renderer) {
            // Clear screen and render UI
            self.screen.clear();
            self.ui.render(&mut self.screen, 0, 0);

            renderer.render(pixels, &self.screen)?;
        }
        Ok(())
    }

    fn handle_ui_event(&mut self, event: Event) {
        match event {
            Event::ButtonClick { button_id } => {
                println!("Button clicked: {}", button_id);
                match button_id.as_str() {
                    "exit" => {
                        // In a real app, this would trigger app shutdown
                        println!("Exit button clicked - in real app this would quit");
                    }
                    "new" => println!("New file requested"),
                    "open" => println!("Open file requested"),
                    "save" => println!("Save file requested"),
                    _ => println!("Unknown button: {}", button_id),
                }
            }
            _ => {
                println!("Unhandled UI event: {:?}", event);
            }
        }
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

                    match Pixels::new(
                        get_window_size().width,
                        get_window_size().height,
                        surface_texture,
                    ) {
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
            WindowEvent::KeyboardInput { event, .. } => {
                if event.state.is_pressed() {
                    // Convert winit keyboard event to our UI event
                    let ui_event = match event.logical_key {
                        winit::keyboard::Key::Character(ref s) if s.len() == 1 => {
                            let ch = s.chars().next().unwrap();
                            match ch {
                                ' ' => Some(Event::KeyPress { key: ' ' }),
                                '\r' | '\n' => Some(Event::KeySpecial {
                                    key: ui::SpecialKey::Enter,
                                }),
                                _ if ch.is_ascii() => Some(Event::KeyPress { key: ch }),
                                _ => None,
                            }
                        }
                        winit::keyboard::Key::Named(named_key) => match named_key {
                            winit::keyboard::NamedKey::Tab => Some(Event::KeySpecial {
                                key: ui::SpecialKey::Tab,
                            }),
                            winit::keyboard::NamedKey::Enter => Some(Event::KeySpecial {
                                key: ui::SpecialKey::Enter,
                            }),
                            winit::keyboard::NamedKey::Escape => Some(Event::KeySpecial {
                                key: ui::SpecialKey::Escape,
                            }),
                            _ => None,
                        },
                        _ => None,
                    };

                    if let Some(ui_event) = ui_event {
                        // Send event to UI and handle any resulting events
                        if let Some(result_event) = self.ui.handle_event(ui_event) {
                            self.handle_ui_event(result_event);
                        }

                        // Request redraw after UI state change
                        if let Some(window) = &self.window {
                            window.request_redraw();
                        }
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
