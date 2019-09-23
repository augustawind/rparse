//! Parsers that parse the same thing repeatedly and collect the results.

pub mod many;
pub mod until;

pub use self::many::*;
pub use self::until::*;
