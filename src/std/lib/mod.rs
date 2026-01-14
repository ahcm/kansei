pub mod polars;

use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub use polars::build_polars_module;

pub fn build_lib_module() -> Value
{
    let mut lib_map = FxHashMap::default();
    lib_map.insert(intern::intern("Polars"), build_polars_module());
    Value::Map(Rc::new(RefCell::new(MapValue::new(lib_map))))
}
