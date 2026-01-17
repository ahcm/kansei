use super::LibMap;
use crate::eval::Interpreter;
use crate::intern;
use crate::value::{HostFunction, MapValue, Value};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen};
use crossterm::{execute, cursor};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::Rect;
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, Paragraph};
use ratatui::Terminal;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::io::{self};
use std::rc::Rc;
use std::time::{Duration, Instant};

#[derive(Clone)]
enum TuiCommand
{
    Paragraph
    {
        rect: Rect,
        text: String,
        title: Option<String>,
    },
    List
    {
        rect: Rect,
        items: Vec<String>,
        title: Option<String>,
    },
}

thread_local! {
    static TUI_COMMANDS: RefCell<Vec<TuiCommand>> = RefCell::new(Vec::new());
    static TUI_SIZE: RefCell<(u16, u16)> = RefCell::new((0, 0));
}

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<i64, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) => Ok(*value as i64),
        Some(Value::Unsigned { value, .. }) => Ok(*value as i64),
        _ => Err(format!("{name} expects an integer")),
    }
}

fn string_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        Some(v) => Ok(v.to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn rect_from_args(args: &[Value], idx: usize, name: &str) -> Result<Rect, String>
{
    let x = int_arg(args, idx, name)?;
    let y = int_arg(args, idx + 1, name)?;
    let w = int_arg(args, idx + 2, name)?;
    let h = int_arg(args, idx + 3, name)?;
    if x < 0 || y < 0 || w <= 0 || h <= 0
    {
        return Err(format!("{name} expects positive rect values"));
    }
    Ok(Rect::new(x as u16, y as u16, w as u16, h as u16))
}

fn ui_clear(_interp: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    TUI_COMMANDS.with(|commands| commands.borrow_mut().clear());
    Ok(Value::Nil)
}

fn ui_size(_interp: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    let (w, h) = TUI_SIZE.with(|size| *size.borrow());
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("width"),
        Value::Integer {
            value: w as i128,
            kind: crate::ast::IntKind::I32,
        },
    );
    map.insert(
        intern::intern("height"),
        Value::Integer {
            value: h as i128,
            kind: crate::ast::IntKind::I32,
        },
    );
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

fn ui_paragraph(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let rect = rect_from_args(args, 0, "Tui.paragraph")?;
    let text = string_arg(args, 4, "Tui.paragraph")?;
    let title = match args.get(5)
    {
        Some(Value::Nil) | None => None,
        Some(v) => Some(v.to_string()),
    };
    TUI_COMMANDS.with(|commands| {
        commands.borrow_mut().push(TuiCommand::Paragraph { rect, text, title });
    });
    Ok(Value::Nil)
}

fn ui_list(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let rect = rect_from_args(args, 0, "Tui.list")?;
    let items_val = args.get(4).ok_or_else(|| "Tui.list expects items".to_string())?;
    let items = match items_val
    {
        Value::Array(arr) =>
        {
            arr.borrow().iter().map(|v| v.to_string()).collect()
        }
        _ => return Err("Tui.list expects an array".to_string()),
    };
    let title = match args.get(5)
    {
        Some(Value::Nil) | None => None,
        Some(v) => Some(v.to_string()),
    };
    TUI_COMMANDS.with(|commands| {
        commands.borrow_mut().push(TuiCommand::List { rect, items, title });
    });
    Ok(Value::Nil)
}

fn make_ui_map() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("clear"), Value::HostFunction(ui_clear as HostFunction));
    map.insert(intern::intern("size"), Value::HostFunction(ui_size as HostFunction));
    map.insert(
        intern::intern("paragraph"),
        Value::HostFunction(ui_paragraph as HostFunction),
    );
    map.insert(intern::intern("list"), Value::HostFunction(ui_list as HostFunction));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn modifiers_map(mods: KeyModifiers) -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("shift"), Value::Boolean(mods.contains(KeyModifiers::SHIFT)));
    map.insert(intern::intern("ctrl"), Value::Boolean(mods.contains(KeyModifiers::CONTROL)));
    map.insert(intern::intern("alt"), Value::Boolean(mods.contains(KeyModifiers::ALT)));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn key_event_map(event: KeyEvent) -> Value
{
    let mut map = FxHashMap::default();
    let code_str = match event.code
    {
        KeyCode::Char(c) =>
        {
            map.insert(intern::intern("char"), Value::String(intern::intern_owned(c.to_string())));
            "Char"
        }
        KeyCode::Enter => "Enter",
        KeyCode::Esc => "Esc",
        KeyCode::Backspace => "Backspace",
        KeyCode::Left => "Left",
        KeyCode::Right => "Right",
        KeyCode::Up => "Up",
        KeyCode::Down => "Down",
        KeyCode::Home => "Home",
        KeyCode::End => "End",
        KeyCode::PageUp => "PageUp",
        KeyCode::PageDown => "PageDown",
        KeyCode::Tab => "Tab",
        KeyCode::BackTab => "BackTab",
        KeyCode::Delete => "Delete",
        KeyCode::Insert => "Insert",
        KeyCode::F(n) =>
        {
            map.insert(
                intern::intern("fn"),
                Value::Integer {
                    value: n as i128,
                    kind: crate::ast::IntKind::I32,
                },
            );
            "F"
        }
        _ => "Other",
    };
    map.insert(
        intern::intern("code"),
        Value::String(intern::intern_owned(code_str.to_string())),
    );
    map.insert(intern::intern("modifiers"), modifiers_map(event.modifiers));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn event_value(event: Event) -> Value
{
    let mut map = FxHashMap::default();
    match event
    {
        Event::Key(key) =>
        {
            map.insert(
                intern::intern("type"),
                Value::String(intern::intern_owned("key".to_string())),
            );
            map.insert(intern::intern("key"), key_event_map(key));
        }
        Event::Resize(w, h) =>
        {
            map.insert(
                intern::intern("type"),
                Value::String(intern::intern_owned("resize".to_string())),
            );
            map.insert(
                intern::intern("width"),
                Value::Integer {
                    value: w as i128,
                    kind: crate::ast::IntKind::I32,
                },
            );
            map.insert(
                intern::intern("height"),
                Value::Integer {
                    value: h as i128,
                    kind: crate::ast::IntKind::I32,
                },
            );
        }
        Event::Mouse(mouse) =>
        {
            map.insert(
                intern::intern("type"),
                Value::String(intern::intern_owned("mouse".to_string())),
            );
            map.insert(
                intern::intern("column"),
                Value::Integer {
                    value: mouse.column as i128,
                    kind: crate::ast::IntKind::I32,
                },
            );
            map.insert(
                intern::intern("row"),
                Value::Integer {
                    value: mouse.row as i128,
                    kind: crate::ast::IntKind::I32,
                },
            );
        }
        _ => {}
    }
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

struct TuiGuard;

impl Drop for TuiGuard
{
    fn drop(&mut self)
    {
        let _ = disable_raw_mode();
        let mut stdout = io::stdout();
        let _ = execute!(stdout, LeaveAlternateScreen, cursor::Show);
    }
}

fn tui_run(interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let (_title, tick_ms, callback) = match args.len()
    {
        1 => ("Kansei".to_string(), 16u64, args[0].clone()),
        2 =>
        {
            if matches!(args[0], Value::String(_))
            {
                (args[0].to_string(), 16u64, args[1].clone())
            }
            else
            {
                (String::from("Kansei"), int_arg(args, 0, "Tui.run")? as u64, args[1].clone())
            }
        }
        _ =>
        {
            (
                args.get(0).map(|v| v.to_string()).unwrap_or_else(|| "Kansei".to_string()),
                int_arg(args, 1, "Tui.run")? as u64,
                args.get(2).cloned().ok_or_else(|| "Tui.run expects a callback".to_string())?,
            )
        }
    };

    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, cursor::Hide).map_err(|e| e.to_string())?;
    enable_raw_mode().map_err(|e| e.to_string())?;
    let _guard = TuiGuard;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend).map_err(|e| e.to_string())?;
    terminal.clear().ok();

    let ui_value = make_ui_map();
    let tick = Duration::from_millis(tick_ms.max(1));
    let mut last_tick = Instant::now();

    loop
    {
        let size = terminal.size().map_err(|e| e.to_string())?;
        TUI_SIZE.with(|s| *s.borrow_mut() = (size.width, size.height));
        TUI_COMMANDS.with(|commands| commands.borrow_mut().clear());

        let timeout = tick.checked_sub(last_tick.elapsed()).unwrap_or(Duration::from_secs(0));
        let mut event_val = Value::Nil;
        if event::poll(timeout).map_err(|e| e.to_string())?
        {
            if let Ok(evt) = event::read()
            {
                event_val = event_value(evt);
            }
        }
        if last_tick.elapsed() >= tick
        {
            last_tick = Instant::now();
        }

        let result = interp.call_value_from_host(
            callback.clone(),
            vec![ui_value.clone(), event_val],
        )?;
        if matches!(result, Value::Boolean(false))
        {
            break;
        }

        let commands = TUI_COMMANDS.with(|commands| commands.borrow().clone());
        terminal
            .draw(|frame| {
                let area = frame.size();
                frame.render_widget(Clear, area);
                for command in commands
                {
                    match command
                    {
                        TuiCommand::Paragraph { rect, text, title } =>
                        {
                            let mut block = Block::default();
                            if let Some(title) = title
                            {
                                block = block.title(title).borders(Borders::ALL);
                            }
                            let widget = Paragraph::new(text).block(block);
                            frame.render_widget(widget, rect);
                        }
                        TuiCommand::List { rect, items, title } =>
                        {
                            let items = items.into_iter().map(ListItem::new).collect::<Vec<_>>();
                            let mut block = Block::default().borders(Borders::ALL);
                            if let Some(title) = title
                            {
                                block = block.title(title);
                            }
                            let widget = List::new(items).block(block);
                            frame.render_widget(widget, rect);
                        }
                    }
                }
            })
            .map_err(|e| e.to_string())?;
    }

    Ok(Value::Nil)
}

pub fn build_tui_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("run"), Value::HostFunction(tui_run as HostFunction));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Tui"), build_tui_module());
}
