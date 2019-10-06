//! Parsers that parse the same thing repeatedly and collect the results.

pub mod many;
pub mod sep_by;
pub mod take;

pub use self::many::*;
pub use self::sep_by::*;
pub use self::take::*;
