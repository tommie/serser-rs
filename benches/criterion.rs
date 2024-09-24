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

fn criterion_benches(c: &mut Criterion) {
    macro_rules! json_into (
        ($ty:ty, $json:literal => $want:expr) => {
            c.bench_function(concat!(stringify!($ty), " ", $json), |b| b.iter(|| bench_basic::<$ty>(black_box($json), $want)));
        }
    );

    json_into!(bool, "true" => true);
    json_into!(bool, "false" => false);

    json_into!(u8,   "42" => 42);
    json_into!(u32,  "4294967295" => !0);
    json_into!(u64,  "18446744073709551615" => !0);
    json_into!(u128, "340282366920938463463374607431768211455" => !0);

    json_into!(i8,   "-42" => -42);
    json_into!(i32,  "-1073741824" => -(1 << 30));
    json_into!(i64,  "-4611686018427387904" => -(1 << 62));
    json_into!(i128, "-85070591730234615865843651857942052864" => -(1 << 126));

    json_into!(f64, "4294967295.5" => 4294967295.5);

    json_into!(char, r#""H""# => 'H');
    json_into!(Vec<u8>, r#""AA==""# => b"\x00".to_vec());
    json_into!(String, r#""Hello""# => "Hello".to_owned());
    json_into!(String, "\"\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\"" => "\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}\u{10FFFF}".to_owned());

    json_into!(Vec<bool>, "[true]" => vec![true]);
    json_into!(Vec<u32>, "[4294967295]" => vec![4294967295]);
    json_into!(Vec<f64>, "[4294967295.5]" => vec![4294967295.5]);
    json_into!(Vec<Vec<String>>, r#"[["Hello"],["World"]]"# => vec![vec!["Hello".to_owned()], vec!["World".to_owned()]]);
}

criterion_group!(benches, criterion_benches);
criterion_main!(benches);
