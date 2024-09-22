#[cfg(test)]
mod tests {
    use serser::derive::FromTokens;
    use serser::token::*;
    use serser::*;

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
                    OwningToken::U32(42),
                    OwningToken::EndEnum,
                ],
            ),
            (
                AnEnum::C(true, 42),
                vec![
                    OwningToken::Enum(meta.clone()),
                    OwningToken::Variant(OwningEnumVariant::Str("C".to_owned())),
                    OwningToken::Bool(true),
                    OwningToken::U32(42),
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
                }),
                OwningToken::Variant(OwningEnumVariant::Str("B".to_owned())),
                OwningToken::Enum(OwningEnumMeta {
                    variants: Some(vec![OwningEnumVariant::Str("A".to_string())]),
                }),
                OwningToken::Variant(OwningEnumVariant::Str("A".to_owned())),
                OwningToken::Bool(true),
                OwningToken::EndEnum,
                OwningToken::EndEnum,
            ],
        )];

        for (want, input) in cases {
            let got = AnotherEnum::from_tokens(TokenVec::from(input)).unwrap();
            assert_eq!(got, want);
        }
    }
}
