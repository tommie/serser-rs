#[cfg(test)]
mod tests {
    use serser::derive::FromTokens;
    use serser::token::*;
    use serser::*;

    #[test]
    fn from_tokens_struct_named_empty() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct {}

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Struct(OwningStructMeta {
                fields: Some(vec![]),
            }),
            OwningToken::EndStruct,
        ]))
        .unwrap();
        assert_eq!(got, AStruct {});
    }

    #[test]
    fn from_tokens_struct_named() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct {
            a: bool,
            b: u32,
        }

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Struct(OwningStructMeta {
                fields: Some(vec!["a".to_owned(), "b".to_owned()]),
            }),
            OwningToken::Field("a".to_owned()),
            OwningToken::Bool(true),
            OwningToken::Field("b".to_owned()),
            OwningToken::U32(42),
            OwningToken::EndStruct,
        ]))
        .unwrap();
        assert_eq!(got, AStruct { a: true, b: 42 });
    }

    #[test]
    fn from_tokens_struct_named_nested() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct {
            a: bool,
        }

        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AnotherStruct {
            b: AStruct,
            c: u32,
        }

        let got = AnotherStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Struct(OwningStructMeta {
                fields: Some(vec!["b".to_owned(), "c".to_owned()]),
            }),
            OwningToken::Field("b".to_owned()),
            OwningToken::Struct(OwningStructMeta {
                fields: Some(vec!["a".to_owned()]),
            }),
            OwningToken::Field("a".to_owned()),
            OwningToken::Bool(true),
            OwningToken::EndStruct,
            OwningToken::Field("c".to_owned()),
            OwningToken::U32(42),
            OwningToken::EndStruct,
        ]))
        .unwrap();
        assert_eq!(
            got,
            AnotherStruct {
                b: AStruct { a: true },
                c: 42,
            }
        );
    }

    #[test]
    fn from_tokens_struct_named_partial() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct {
            a: bool,
            b: u32,
        }

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Struct(OwningStructMeta {
                fields: Some(vec!["a".to_owned()]),
            }),
            OwningToken::Field("a".to_owned()),
            OwningToken::Bool(true),
            OwningToken::EndStruct,
        ]))
        .unwrap_err();
        assert_eq!(got, TokenError::MissingFields(vec!["b".to_owned()]));
    }

    #[test]
    fn from_tokens_struct_unit() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct();

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(0) }),
            OwningToken::EndTuple,
        ]))
        .unwrap();
        assert_eq!(got, AStruct());
    }

    #[test]
    fn from_tokens_struct_tuple() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct(bool, u32);

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(0) }),
            OwningToken::Bool(true),
            OwningToken::U32(42),
            OwningToken::EndTuple,
        ]))
        .unwrap();
        assert_eq!(got, AStruct(true, 42));
    }

    #[test]
    fn from_tokens_struct_tuple_nested() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct(bool);

        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AnotherStruct(AStruct, u32);

        let got = AnotherStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
            OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
            OwningToken::Bool(true),
            OwningToken::EndTuple,
            OwningToken::U32(42),
            OwningToken::EndTuple,
        ]))
        .unwrap();
        assert_eq!(got, AnotherStruct(AStruct(true), 42,));
    }

    #[test]
    fn from_tokens_struct_tuple_partial() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        struct AStruct(bool, u32);

        let got = AStruct::from_tokens(TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(0) }),
            OwningToken::Bool(true),
            OwningToken::EndTuple,
        ]))
        .unwrap_err();
        assert_eq!(
            got,
            TokenError::InvalidToken(OwningToken::EndTuple, Some(TokenTypes::new(TokenType::U32)))
        );
    }

    #[test]
    fn from_tokens_enum_empty() {
        // An empty enum should be valid, even though there is nothing
        // we can do with it.
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        enum AnEnum {}
    }

    #[test]
    fn from_tokens_enum() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
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
            kind: None,
        };

        let cases = vec![
            (
                AnEnum::A,
                vec![
                    OwningToken::Enum(meta.clone()),
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

        for (want, input) in cases {
            let got = AnEnum::from_tokens(TokenVec::from(input)).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn from_tokens_enum_nested() {
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        enum AnEnum {
            A(bool),
        }
        #[derive(Debug, Eq, FromTokens, PartialEq)]
        enum AnotherEnum {
            B(AnEnum),
        }

        let cases = vec![(
            AnotherEnum::B(AnEnum::A(true)),
            vec![
                OwningToken::Enum(OwningEnumMeta {
                    variants: Some(vec![OwningEnumVariant::Str("B".to_string())]),
                    kind: None,
                }),
                OwningToken::Variant(OwningEnumVariant::Str("B".to_owned())),
                OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
                OwningToken::Enum(OwningEnumMeta {
                    variants: Some(vec![OwningEnumVariant::Str("A".to_string())]),
                    kind: None,
                }),
                OwningToken::Variant(OwningEnumVariant::Str("A".to_owned())),
                OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
                OwningToken::Bool(true),
                OwningToken::EndTuple,
                OwningToken::EndEnum,
                OwningToken::EndTuple,
                OwningToken::EndEnum,
            ],
        )];

        for (want, input) in cases {
            let got = AnotherEnum::from_tokens(TokenVec::from(input)).unwrap();
            assert_eq!(got, want);
        }
    }
}
