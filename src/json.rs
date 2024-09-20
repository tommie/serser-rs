//! Parses and serializes [JSON](https://json.org/).

mod from;
pub use from::json_into_tokens;
pub use from::ParseError;

mod into;
pub use into::json_from_tokens;
pub use into::WriteError;
