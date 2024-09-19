#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SeqMeta {
    pub size_hint: Option<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TupleMeta {
    pub size_hint: Option<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructMeta<'a> {
    pub size_hint: Option<usize>,
    pub fields: Option<&'a [&'a str]>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct OwningStructMeta {
    pub size_hint: Option<usize>,
    pub fields: Option<Vec<String>>,
}

impl<'a> From<StructMeta<'a>> for OwningStructMeta {
    fn from(v: StructMeta<'a>) -> Self {
        Self {
            size_hint: v.size_hint,
            fields: v
                .fields
                .map(|fields| fields.into_iter().map(|k| (*k).to_owned()).collect()),
        }
    }
}

impl<'a> From<&'a OwningStructMeta> for StructMeta<'a> {
    fn from(v: &'a OwningStructMeta) -> Self {
        Self {
            size_hint: v.size_hint,
            fields: None, // TODO
        }
    }
}
