use std::fmt;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use serser::json::*;
use serser::FromTokenSink;

fn bench_basic<T: FromTokenSink + fmt::Debug + PartialEq>(json: &str, want: T) {
    if cfg!(debug_assertions) {
        assert_eq!(json_into::<T, _>(json.as_bytes()).unwrap(), want);
    } else {
        json_into::<T, _>(json.as_bytes()).unwrap();
    }
}

fn criterion_basic(c: &mut Criterion) {
    macro_rules! basic_json_into (
        ($ty:ty, $json:literal => $want:expr) => {
            c.bench_function(concat!(stringify!($ty), " ", $json), |b| b.iter(|| bench_basic::<$ty>(black_box($json), $want)));
        }
    );

    basic_json_into!(bool, "true" => true);
    basic_json_into!(bool, "false" => false);

    basic_json_into!(u8,   "42" => 42);
    basic_json_into!(u32,  "42" => 42);
    basic_json_into!(u64,  "42" => 42);
    basic_json_into!(u128, "42" => 42);

    basic_json_into!(i8,   "-42" => -42);
    basic_json_into!(i32,  "-42" => -42);
    basic_json_into!(i64,  "-42" => -42);
    basic_json_into!(i128, "-42" => -42);

    basic_json_into!(f64, "42.5" => 42.5);

    basic_json_into!(char, r#""H""# => 'H');
    basic_json_into!(Vec<u8>, r#""AA==""# => b"\x00".to_vec());
    basic_json_into!(String, r#""Hello""# => "Hello".to_owned());
    basic_json_into!(String, "\"\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\"" => "\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}".to_owned());
}

criterion_group!(benches, criterion_basic);
criterion_main!(benches);
