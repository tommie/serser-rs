# Serser – the Rust serialization engine

[![crates.io](https://img.shields.io/crates/v/serser)](https://crates.io/crates/serser)
[![docs.rs](https://img.shields.io/badge/serser-66c2a5?style=flat&logo=docsdotrs&label=docs.rs)](https://docs.rs/serser)
[![GitHub last commit](https://img.shields.io/github/last-commit/tommie/serser-rs)](https://github.com/tommie/serser-rs)
[![Test status](https://img.shields.io/github/actions/workflow/status/tommie/serser-rs/test.yml)](https://github.com/tommie/serser-rs/actions/workflows/test.yml)

serser is a serialization/deserialization framework for [Rust](https://rust-lang.org/).
It uses a stream of tokens (e.g. U32, Seq, and EndSeq) as the intermediate representation.

## Examples

### Rust To JSON

```rust
use serser::derive::*;                // The derive macros.
use serser::json::json_from_tokens;
use serser::prelude::*;               // Includes the {From,Into]Tokens traits.

# fn main() -> Result<(), serser::json::WriteError> {
#[derive(FromTokens, IntoTokens)]
struct A {
  b: u32
}

let original = A { b: 42 };
let mut json = String::new();
json_from_tokens(&mut json, original)?;

assert_eq!(r#"{"b":42}"#, json);
# Ok(())
# }
```

### JSON To Rust

```rust
use serser::derive::*;                     // The derive macros.
use serser::json::json_into;
use serser::prelude::*;                    // Includes the {From,Into]Tokens traits.

# fn main() -> Result<(), serser::json::ParseError<serser::TokenError>> {
#[derive(Debug, Eq, PartialEq)]            // For the assert_eq.
#[derive(FromTokens, IntoTokens)]
struct A {
  b: u32
}

let reader = r#"{ "b": 42 }"#.as_bytes();
let got = json_into::<A, _>(reader)?;      // Takes a std::io::Read.

assert_eq!(got, A { b: 42 });
# Ok(())
# }
```

### Rust To Rust

This is emulating [Clone](https://doc.rust-lang.org/stable/core/clone/trait.Clone.html):

```rust
use serser::derive::*;                // The derive macros.
use serser::prelude::*;               // Includes the {From,Into]Tokens traits.
use serser::TokenVec;

# fn main() -> Result<(), serser::TokenError> {
#[derive(Debug, Eq, PartialEq)]       // For the assert_eq.
#[derive(FromTokens, IntoTokens)]
struct A {
  b: u32
}

let mut tokens = TokenVec::new();     // A buffer, useful for testing.

let original = A { b: 42 };
original.into_tokens(&mut tokens)?;   // Writes to `tokens`.
let clone = A::from_tokens(tokens)?;  // Reads from `tokens`.

assert_eq!(clone, original);
# Ok(())
# }
```

## Compared To serde

The [serde](https://docs.rs/serde/latest/serde/) crate is the defacto standard for serialization, both for JSON and as used in wasm-bindgen for communicating with the host from WebAssembly.

This crate aims to improve on the serde design in these ways:

* A smaller API, making it easier to write custom (de)serializers.
  Only one function needs to be implemented for a sink: `yield_token`.
  Another function can be overridden to provide hints for the next expected token.
* A bufferable intermediate representation that can be shared between threads, or stored for later use.
  Primarily lightweight, reference-based, `Token` objects are used.
  An `OwningToken` is provided for easy storage and IPC.
* A push-based pipeline, inverting the `Iterator` pattern to allow correct lifetimes to be expressed more easily.
  Tokens only live during the [yield_token](TokenSink::yield_token) invocation;
  if the callee needs to store it, it has to make a copy.
  This is similar to serde, but avoids the ping-pong between
  [Deserializer](https://docs.rs/serde/latest/serde/de/trait.Deserializer.html),
  [\*Access](https://docs.rs/serde/latest/serde/de/trait.SeqAccess.html)
  and [Visitor](https://docs.rs/serde/latest/serde/de/trait.Visitor.html).
* No reliance on `'static` for metadata like struct field names.

The aim is for the implementation to be on a par with serde in terms of performance.

The name serser comes from the idea that serialization and deserialization are fundamentally the same thing, just switching perspectives of the source and sink.
It is a conversion from one representation to another, through a common data model.

See the [documentation](https://docs.rs/serser) for more information.
