#![feature(pattern)]

pub mod error;
pub mod parser;
pub mod stream;
pub mod traits;

pub use error::{Error, ParseResult};
pub use parser::Parser;
pub use stream::Stream;
