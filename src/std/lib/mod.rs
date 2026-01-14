pub mod polars;
pub mod clap;
pub mod serde;
pub mod regex;
pub mod datetime;
pub mod crypto;
pub mod http;
pub mod csv;
pub mod path;
pub mod math;

use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub use polars::build_polars_module;
pub use clap::build_clap_module;
pub use serde::build_serde_module;
pub use regex::build_regex_module;
pub use datetime::build_datetime_module;
pub use crypto::build_crypto_module;
pub use http::build_http_module;
pub use csv::build_csv_module;
pub use path::build_path_module;
pub use math::build_math_module;

pub fn build_lib_module() -> Value
{
    let mut lib_map = FxHashMap::default();
    lib_map.insert(intern::intern("Polars"), build_polars_module());
    lib_map.insert(intern::intern("clap"), build_clap_module());
    lib_map.insert(intern::intern("Serde"), build_serde_module());
    lib_map.insert(intern::intern("Regex"), build_regex_module());
    lib_map.insert(intern::intern("DateTime"), build_datetime_module());
    lib_map.insert(intern::intern("Crypto"), build_crypto_module());
    lib_map.insert(intern::intern("Http"), build_http_module());
    lib_map.insert(intern::intern("Csv"), build_csv_module());
    lib_map.insert(intern::intern("Path"), build_path_module());
    lib_map.insert(intern::intern("Math"), build_math_module());
    Value::Map(Rc::new(RefCell::new(MapValue::new(lib_map))))
}
