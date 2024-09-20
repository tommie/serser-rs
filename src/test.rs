//! Utilities for testing and debugging.

mod expecting;
pub use expecting::ExpectingTokenSink;

mod printsink;
pub use printsink::PrintError;
pub use printsink::PrintingTokenSink;
