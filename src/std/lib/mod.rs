pub mod base64;
pub mod bytes;
pub mod clap;
pub mod crypto;
pub mod csv;
pub mod datetime;
pub mod flate2;
pub mod http;
pub mod image;
pub mod math;
pub mod mmap;
pub mod path;
pub mod polars;
pub mod regex;
pub mod serde;
pub mod sqlite;
pub mod tests;
pub mod toml;
pub mod uuid;
pub mod yaml;

use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub use base64::build_base64_module;
pub use bytes::build_bytes_module;
pub use clap::build_clap_module;
pub use crypto::build_crypto_module;
pub use csv::build_csv_module;
pub use datetime::build_datetime_module;
pub use flate2::build_flate2_module;
pub use http::build_http_module;
pub use image::build_image_module;
pub use math::build_math_module;
pub use mmap::build_mmap_module;
pub use path::build_path_module;
pub use polars::build_polars_module;
pub use regex::build_regex_module;
pub use serde::build_serde_module;
pub use sqlite::build_sqlite_module;
pub use tests::build_tests_module;
pub use toml::build_toml_module;
pub use uuid::build_uuid_module;
pub use yaml::build_yaml_module;

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
    lib_map.insert(intern::intern("Base64"), build_base64_module());
    lib_map.insert(intern::intern("Uuid"), build_uuid_module());
    lib_map.insert(intern::intern("Toml"), build_toml_module());
    lib_map.insert(intern::intern("Yaml"), build_yaml_module());
    lib_map.insert(intern::intern("Flate2"), build_flate2_module());
    lib_map.insert(intern::intern("Image"), build_image_module());
    lib_map.insert(intern::intern("Sqlite"), build_sqlite_module());
    lib_map.insert(intern::intern("Bytes"), build_bytes_module());
    lib_map.insert(intern::intern("Mmap"), build_mmap_module());
    lib_map.insert(intern::intern("tests"), build_tests_module());
    Value::Map(Rc::new(RefCell::new(MapValue::new(lib_map))))
}
