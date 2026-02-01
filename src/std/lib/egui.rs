use super::LibMap;
use crate::eval::Interpreter;
use crate::intern;
use crate::value::{HostFunction, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[cfg(feature = "lib-egui")]
use eframe::egui;

#[cfg(feature = "lib-egui")]
thread_local! {
    static EGUI_UI: RefCell<Option<*mut egui::Ui>> = RefCell::new(None);
}

#[cfg(feature = "lib-egui")]
fn with_egui_ui<F, T>(name: &str, f: F) -> Result<T, String>
where
    F: FnOnce(&mut egui::Ui) -> Result<T, String>,
{
    EGUI_UI.with(|cell| {
        let mut slot = cell.borrow_mut();
        let ui_ptr = slot
            .take()
            .ok_or_else(|| format!("{name} requires an active UI"))?;
        let result = unsafe { f(&mut *ui_ptr) };
        *slot = Some(ui_ptr);
        result
    })
}

#[cfg(feature = "lib-egui")]
fn to_string_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        Some(v) => Ok(v.to_string()),
        None => Err(format!("{name} expects a string")),
    }
}

#[cfg(feature = "lib-egui")]
fn to_f64_arg(args: &[Value], idx: usize, name: &str) -> Result<f64, String>
{
    match args.get(idx)
    {
        Some(Value::Float { value, .. }) => Ok(*value),
        Some(Value::Integer { value, .. }) => Ok(*value as f64),
        Some(Value::Unsigned { value, .. }) => Ok(*value as f64),
        _ => Err(format!("{name} expects a number")),
    }
}

#[cfg(feature = "lib-egui")]
fn set_ref_value(target: &Value, value: Value, name: &str) -> Result<(), String>
{
    match target
    {
        Value::Reference(r) =>
        {
            *r.borrow_mut() = value;
            Ok(())
        }
        _ => Err(format!("{name} expects a reference")),
    }
}

#[cfg(feature = "lib-egui")]
fn egui_ui_label(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let text = to_string_arg(args, 0, "egui.ui.label")?;
    with_egui_ui("egui.ui.label", |ui| {
        ui.label(text);
        Ok(Value::Nil)
    })
}

#[cfg(feature = "lib-egui")]
fn egui_ui_heading(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let text = to_string_arg(args, 0, "egui.ui.heading")?;
    with_egui_ui("egui.ui.heading", |ui| {
        ui.heading(text);
        Ok(Value::Nil)
    })
}

#[cfg(feature = "lib-egui")]
fn egui_ui_separator(_interp: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    with_egui_ui("egui.ui.separator", |ui| {
        ui.separator();
        Ok(Value::Nil)
    })
}

#[cfg(feature = "lib-egui")]
fn egui_ui_button(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let text = to_string_arg(args, 0, "egui.ui.button")?;
    with_egui_ui("egui.ui.button", |ui| Ok(Value::Boolean(ui.button(text).clicked())))
}

#[cfg(feature = "lib-egui")]
fn egui_ui_checkbox(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let label = to_string_arg(args, 0, "egui.ui.checkbox")?;
    let value = args
        .get(1)
        .ok_or_else(|| "egui.ui.checkbox expects a reference".to_string())?;
    let mut checked = match value
    {
        Value::Reference(r) => match &*r.borrow()
        {
            Value::Boolean(b) => *b,
            _ => false,
        },
        _ => return Err("egui.ui.checkbox expects a reference".to_string()),
    };

    let changed =
        with_egui_ui("egui.ui.checkbox", |ui| Ok(ui.checkbox(&mut checked, label).changed()))?;
    if changed
    {
        set_ref_value(value, Value::Boolean(checked), "egui.ui.checkbox")?;
    }
    Ok(Value::Boolean(changed))
}

#[cfg(feature = "lib-egui")]
fn egui_ui_slider(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let value = args
        .get(0)
        .ok_or_else(|| "egui.ui.slider expects a reference".to_string())?;
    let mut current = match value
    {
        Value::Reference(r) => match &*r.borrow()
        {
            Value::Float { value, .. } => *value,
            Value::Integer { value, .. } => *value as f64,
            Value::Unsigned { value, .. } => *value as f64,
            _ => 0.0,
        },
        _ => return Err("egui.ui.slider expects a reference".to_string()),
    };
    let min = to_f64_arg(args, 1, "egui.ui.slider")?;
    let max = to_f64_arg(args, 2, "egui.ui.slider")?;
    let label = args
        .get(3)
        .map(|_| to_string_arg(args, 3, "egui.ui.slider"))
        .transpose()?;

    let changed = with_egui_ui("egui.ui.slider", |ui| {
        let mut slider = egui::Slider::new(&mut current, min..=max);
        if let Some(label) = label.clone()
        {
            slider = slider.text(label);
        }
        Ok(ui.add(slider).changed())
    })?;
    if changed
    {
        set_ref_value(
            value,
            Value::Float {
                value: current,
                kind: crate::ast::FloatKind::F64,
            },
            "egui.ui.slider",
        )?;
    }
    Ok(Value::Boolean(changed))
}

#[cfg(feature = "lib-egui")]
fn egui_ui_text_input(_interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let value = args
        .get(0)
        .ok_or_else(|| "egui.ui.text_input expects a reference".to_string())?;
    let mut text = match value
    {
        Value::Reference(r) => match &*r.borrow()
        {
            Value::String(s) => s.as_str().to_string(),
            _ => String::new(),
        },
        _ => return Err("egui.ui.text_input expects a reference".to_string()),
    };
    let changed =
        with_egui_ui("egui.ui.text_input", |ui| Ok(ui.text_edit_singleline(&mut text).changed()))?;
    if changed
    {
        set_ref_value(value, Value::String(intern::intern_owned(text)), "egui.ui.text_input")?;
    }
    Ok(Value::Boolean(changed))
}

#[cfg(feature = "lib-egui")]
fn egui_run(interp: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let title = args
        .get(0)
        .map(|v| v.to_string())
        .unwrap_or_else(|| "Kansei".to_string());
    let width = args
        .get(1)
        .map(|v| v.to_string().parse::<f32>().ok())
        .flatten()
        .unwrap_or(640.0);
    let height = args
        .get(2)
        .map(|v| v.to_string().parse::<f32>().ok())
        .flatten()
        .unwrap_or(480.0);
    let callback = args
        .get(3)
        .cloned()
        .ok_or_else(|| "egui.run expects a callback".to_string())?;

    let mut ui_map = FxHashMap::default();
    ui_map.insert(intern::intern("label"), Value::HostFunction(egui_ui_label as HostFunction));
    ui_map.insert(intern::intern("heading"), Value::HostFunction(egui_ui_heading as HostFunction));
    ui_map.insert(
        intern::intern("separator"),
        Value::HostFunction(egui_ui_separator as HostFunction),
    );
    ui_map.insert(intern::intern("button"), Value::HostFunction(egui_ui_button as HostFunction));
    ui_map
        .insert(intern::intern("checkbox"), Value::HostFunction(egui_ui_checkbox as HostFunction));
    ui_map.insert(intern::intern("slider"), Value::HostFunction(egui_ui_slider as HostFunction));
    ui_map.insert(
        intern::intern("text_input"),
        Value::HostFunction(egui_ui_text_input as HostFunction),
    );
    let ui_value = Value::Map(Rc::new(RefCell::new(MapValue::new(ui_map))));

    let interp_ptr: *mut Interpreter = interp as *mut _;
    let app = KanseiEguiApp {
        interp_ptr,
        callback,
        ui_value,
    };

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title(title)
            .with_inner_size([width, height]),
        ..Default::default()
    };

    eframe::run_native("Kansei", options, Box::new(|_cc| Ok(Box::new(app))))
        .map_err(|e| format!("egui.run failed: {e}"))?;

    Ok(Value::Nil)
}

#[cfg(feature = "lib-egui")]
struct KanseiEguiApp
{
    interp_ptr: *mut Interpreter,
    callback: Value,
    ui_value: Value,
}

#[cfg(feature = "lib-egui")]
impl eframe::App for KanseiEguiApp
{
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame)
    {
        egui::CentralPanel::default().show(ctx, |ui| {
            EGUI_UI.with(|cell| {
                *cell.borrow_mut() = Some(ui as *mut egui::Ui);
            });
            let result = unsafe {
                let interp = &mut *self.interp_ptr;
                interp.call_value_from_host(self.callback.clone(), vec![self.ui_value.clone()])
            };
            if let Err(message) = result
            {
                eprintln!("egui callback error: {message}");
            }
            EGUI_UI.with(|cell| {
                *cell.borrow_mut() = None;
            });
        });
    }
}

#[cfg(feature = "lib-egui")]
pub fn build_egui_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("run"), Value::HostFunction(egui_run as HostFunction));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

#[cfg(feature = "lib-egui")]
pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("egui"), build_egui_module());
}

#[cfg(not(feature = "lib-egui"))]
pub fn build_egui_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("run"),
        Value::NativeFunction(|_| Err("std::lib::egui requires --features egui".to_string())),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

#[cfg(not(feature = "lib-egui"))]
pub fn register(_map: &mut LibMap) {}
