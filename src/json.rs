//! Parses and serializes [JSON](https://json.org/).

mod from;
pub use from::json_into_tokens;
pub use from::ParseError;
