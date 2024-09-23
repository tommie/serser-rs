#[cfg(test)]
mod tests {
    use serser::derive::IntoTokens;
    use serser::token::*;
    use serser::*;

    #[test]
    fn into_tokens_struct_empty() {
        #[derive(IntoTokens)]
        struct AStruct {}

        let mut got = TokenVec::new();
        AStruct {}.into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Struct(OwningStructMeta {
                    fields: Some(vec![])
                }),
                OwningToken::EndStruct,
            ]
        );
    }

    #[test]
    fn into_tokens_struct() {
        #[derive(IntoTokens)]
        struct AStruct<'a> {
            _aunit: (),
            abool: bool,

            au8: u8,
            au16: u16,
            au32: u32,
            au64: u64,
            au128: u128,
            ausize: usize,

            ani8: i8,
            ani16: i16,
            ani32: i32,
            ani64: i64,
            ani128: i128,
            anisize: isize,

            anf32: f32,
            anf64: f64,
            achar: char,
            astr: &'a str,
        }

        let mut got = TokenVec::new();
        AStruct {
            _aunit: (),
            abool: true,

            au8: 42,
            au16: 43,
            au32: 44,
            au64: 45,
            au128: 46,
            ausize: 47,

            ani8: 52,
            ani16: 53,
            ani32: 54,
            ani64: 55,
            ani128: 56,
            anisize: 57,

            anf32: 62.0,
            anf64: 63.0,
            achar: 'W',
            astr: "Hello",
        }
        .into_tokens(&mut got)
        .unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Struct(OwningStructMeta {
                    fields: Some(
                        vec![
                            "_aunit", "abool", "au8", "au16", "au32", "au64", "au128", "ausize",
                            "ani8", "ani16", "ani32", "ani64", "ani128", "anisize", "anf32",
                            "anf64", "achar", "astr"
                        ]
                        .into_iter()
                        .map(|s| s.to_owned())
                        .collect()
                    )
                }),
                OwningToken::Field("_aunit".to_owned()),
                OwningToken::Unit,
                OwningToken::Field("abool".to_owned()),
                OwningToken::Bool(true),
                OwningToken::Field("au8".to_owned()),
                OwningToken::U8(42),
                OwningToken::Field("au16".to_owned()),
                OwningToken::U16(43),
                OwningToken::Field("au32".to_owned()),
                OwningToken::U32(44),
                OwningToken::Field("au64".to_owned()),
                OwningToken::U64(45),
                OwningToken::Field("au128".to_owned()),
                OwningToken::U128(46),
                OwningToken::Field("ausize".to_owned()),
                OwningToken::Usize(47),
                OwningToken::Field("ani8".to_owned()),
                OwningToken::I8(52),
                OwningToken::Field("ani16".to_owned()),
                OwningToken::I16(53),
                OwningToken::Field("ani32".to_owned()),
                OwningToken::I32(54),
                OwningToken::Field("ani64".to_owned()),
                OwningToken::I64(55),
                OwningToken::Field("ani128".to_owned()),
                OwningToken::I128(56),
                OwningToken::Field("anisize".to_owned()),
                OwningToken::Isize(57),
                OwningToken::Field("anf32".to_owned()),
                OwningToken::F32(62.0),
                OwningToken::Field("anf64".to_owned()),
                OwningToken::F64(63.0),
                OwningToken::Field("achar".to_owned()),
                OwningToken::Char('W'),
                OwningToken::Field("astr".to_owned()),
                OwningToken::Str("Hello".to_owned()),
                OwningToken::EndStruct,
            ]
        );
    }

    #[test]
    fn into_tokens_unit_struct() {
        #[derive(IntoTokens)]
        struct AStruct();

        let mut got = TokenVec::new();
        AStruct().into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(0) }),
                OwningToken::EndTuple,
            ]
        );
    }

    #[test]
    fn into_tokens_tuple_struct() {
        #[derive(IntoTokens)]
        struct AStruct(bool, u32);

        let mut got = TokenVec::new();
        AStruct(true, 42).into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
                OwningToken::Bool(true),
                OwningToken::U32(42),
                OwningToken::EndTuple,
            ]
        );
    }

    #[test]
    fn into_tokens_enum_tuple() {
        #[derive(IntoTokens)]
        enum AnEnum {
            A,
            B(u32),
            C(bool, u32),
        }
        let meta = OwningEnumMeta {
            variants: Some(vec![
                OwningEnumVariant::Str("A".to_string()),
                OwningEnumVariant::Str("B".to_string()),
                OwningEnumVariant::Str("C".to_string()),
            ]),
            kind: Some(EnumKind::Tuple),
        };

        let cases = vec![
            (
                AnEnum::A,
                vec![
                    OwningToken::Enum(OwningEnumMeta {
                        kind: None,
                        ..meta.clone()
                    }),
                    OwningToken::Variant(OwningEnumVariant::Str("A".to_owned())),
                    OwningToken::EndEnum,
                ],
            ),
            (
                AnEnum::B(42),
                vec![
                    OwningToken::Enum(meta.clone()),
                    OwningToken::Variant(OwningEnumVariant::Str("B".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
                    OwningToken::U32(42),
                    OwningToken::EndTuple,
                    OwningToken::EndEnum,
                ],
            ),
            (
                AnEnum::C(true, 42),
                vec![
                    OwningToken::Enum(meta.clone()),
                    OwningToken::Variant(OwningEnumVariant::Str("C".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
                    OwningToken::Bool(true),
                    OwningToken::U32(42),
                    OwningToken::EndTuple,
                    OwningToken::EndEnum,
                ],
            ),
        ];

        for (input, want) in cases {
            let mut got = TokenVec::new();
            input.into_tokens(&mut got).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }

    #[test]
    fn into_tokens_enum_struct() {
        #[derive(IntoTokens)]
        enum AnEnum {
            A,
            B { b: u32 },
            C { c: bool, d: u32 },
        }
        let meta = OwningEnumMeta {
            variants: Some(vec![
                OwningEnumVariant::Str("A".to_string()),
                OwningEnumVariant::Str("B".to_string()),
                OwningEnumVariant::Str("C".to_string()),
            ]),
            kind: Some(EnumKind::Struct),
        };

        let cases = vec![
            (
                AnEnum::A,
                vec![
                    OwningToken::Enum(OwningEnumMeta {
                        kind: None,
                        ..meta.clone()
                    }),
                    OwningToken::Variant(OwningEnumVariant::Str("A".to_owned())),
                    OwningToken::EndEnum,
                ],
            ),
            (
                AnEnum::B { b: 42 },
                vec![
                    OwningToken::Enum(meta.clone()),
                    OwningToken::Variant(OwningEnumVariant::Str("B".to_owned())),
                    OwningToken::Struct(OwningStructMeta {
                        fields: Some(vec!["b".to_owned()]),
                    }),
                    OwningToken::Field("b".to_owned()),
                    OwningToken::U32(42),
                    OwningToken::EndStruct,
                    OwningToken::EndEnum,
                ],
            ),
            (
                AnEnum::C { c: true, d: 42 },
                vec![
                    OwningToken::Enum(meta.clone()),
                    OwningToken::Variant(OwningEnumVariant::Str("C".to_owned())),
                    OwningToken::Struct(OwningStructMeta {
                        fields: Some(vec!["c".to_owned(), "d".to_owned()]),
                    }),
                    OwningToken::Field("c".to_owned()),
                    OwningToken::Bool(true),
                    OwningToken::Field("d".to_owned()),
                    OwningToken::U32(42),
                    OwningToken::EndStruct,
                    OwningToken::EndEnum,
                ],
            ),
        ];

        for (input, want) in cases {
            let mut got = TokenVec::new();
            input.into_tokens(&mut got).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }
}
