#![feature(associated_type_defaults)]

pub mod error;
pub mod input;
#[macro_use]
pub mod parser;

pub use error::{Error, ParseResult};
pub use input::Input;
pub use parser::Parser;
