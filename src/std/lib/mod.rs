#[cfg(feature = "lib-base64")]
pub mod base64;
#[cfg(feature = "lib-bytes")]
pub mod bytes;
#[cfg(feature = "lib-clap")]
pub mod clap;
#[cfg(feature = "lib-crypto")]
pub mod crypto;
#[cfg(feature = "lib-csv")]
pub mod csv;
#[cfg(feature = "lib-datetime")]
pub mod datetime;
#[cfg(feature = "lib-egui")]
pub mod egui;
#[cfg(feature = "lib-flate2")]
pub mod flate2;
#[cfg(feature = "lib-http")]
pub mod http;
#[cfg(feature = "lib-image")]
pub mod image;
#[cfg(feature = "lib-math")]
pub mod math;
#[cfg(feature = "lib-mmap")]
pub mod mmap;
#[cfg(feature = "lib-path")]
pub mod path;
#[cfg(feature = "lib-polars")]
pub mod polars;
#[cfg(feature = "lib-regex")]
pub mod regex;
#[cfg(feature = "lib-serde")]
pub mod serde;
#[cfg(feature = "lib-sqlite")]
pub mod sqlite;
#[cfg(feature = "lib-tests")]
pub mod tests;
#[cfg(feature = "lib-toml")]
pub mod toml;
#[cfg(feature = "lib-uuid")]
pub mod uuid;
#[cfg(feature = "lib-yaml")]
pub mod yaml;

use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type LibMap = FxHashMap<Rc<String>, Value>;

pub fn build_lib_module() -> Value
{
    let mut lib_map: LibMap = FxHashMap::default();
    #[cfg(feature = "lib-base64")]
    base64::register(&mut lib_map);
    #[cfg(feature = "lib-bytes")]
    bytes::register(&mut lib_map);
    #[cfg(feature = "lib-clap")]
    clap::register(&mut lib_map);
    #[cfg(feature = "lib-crypto")]
    crypto::register(&mut lib_map);
    #[cfg(feature = "lib-csv")]
    csv::register(&mut lib_map);
    #[cfg(feature = "lib-datetime")]
    datetime::register(&mut lib_map);
    #[cfg(feature = "lib-egui")]
    egui::register(&mut lib_map);
    #[cfg(feature = "lib-flate2")]
    flate2::register(&mut lib_map);
    #[cfg(feature = "lib-http")]
    http::register(&mut lib_map);
    #[cfg(feature = "lib-image")]
    image::register(&mut lib_map);
    #[cfg(feature = "lib-math")]
    math::register(&mut lib_map);
    #[cfg(feature = "lib-mmap")]
    mmap::register(&mut lib_map);
    #[cfg(feature = "lib-path")]
    path::register(&mut lib_map);
    #[cfg(feature = "lib-polars")]
    polars::register(&mut lib_map);
    #[cfg(feature = "lib-regex")]
    regex::register(&mut lib_map);
    #[cfg(feature = "lib-serde")]
    serde::register(&mut lib_map);
    #[cfg(feature = "lib-sqlite")]
    sqlite::register(&mut lib_map);
    #[cfg(feature = "lib-tests")]
    tests::register(&mut lib_map);
    #[cfg(feature = "lib-toml")]
    toml::register(&mut lib_map);
    #[cfg(feature = "lib-uuid")]
    uuid::register(&mut lib_map);
    #[cfg(feature = "lib-yaml")]
    yaml::register(&mut lib_map);
    Value::Map(Rc::new(RefCell::new(MapValue::new(lib_map))))
}
