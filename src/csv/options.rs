/// Options for csv_from_tokens.
pub struct CsvOptions<'a> {
    /// The separators, from outermost to innermost. I.e. the first
    /// element is usually '\n'. The last element is used for all
    /// levels below that level too.
    pub separators: &'a [char],

    /// The character used to quote the innermost data. If not
    /// specified, the values are never quoted, and the output may be
    /// ambiguous. Any occurrence of the quote character in a string
    /// is serialized as double quote character.
    pub quote: Option<char>,
}

/// A colon-separated value without quoting.
pub const COLON: &CsvOptions = &CsvOptions {
    separators: &['\n', ':'],
    quote: None,
};

/// A comma-separated value with quoting.
pub const COMMA: &CsvOptions = &CsvOptions {
    separators: &['\n', ','],
    quote: Some('"'),
};

/// A semi-colon-separated value with quoting.
pub const SEMICOLON: &CsvOptions = &CsvOptions {
    separators: &['\n', ';'],
    quote: Some('"'),
};

/// A tab-separated value with quoting.
pub const TAB: &CsvOptions = &CsvOptions {
    separators: &['\n', '\t'],
    quote: Some('"'),
};
