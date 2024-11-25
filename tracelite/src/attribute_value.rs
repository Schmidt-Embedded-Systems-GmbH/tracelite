use std::fmt::{Debug, Display};

use erased_serde::Serialize;

#[non_exhaustive]
pub enum AttributeValue<'a> {
    // attribute can be dropped
    NotPresent,
    Unit,
    Bool(bool),
    Char(char),
    U64(u64),
    I64(i64),
    F64(f64),

    // I128(i128),
    // U128(u128),

    Str(&'a str),
    Bytes(&'a [u8]),

    // PathBuf(PathBuf)
    // Url(Url)
    // Duration(std::time::Duration),
    // Instant(std::time::Instant),

    DynDisplay(&'a dyn Display),
    DynDebug(&'a dyn Debug),
    DynSerialize(&'a dyn Serialize),
}

impl<'a> AttributeValue<'a> {
    // we need these cuz of funny coercion rules.... sometimes constructing values from macros just doesnt work
    pub fn display(d: &'a impl Display) -> Self {
        Self::DynDisplay(d)
    }
    pub fn debug(d: &'a impl Debug) -> Self {
        Self::DynDebug(d)
    }
    pub fn serialize(d: &'a impl Serialize) -> Self {
        Self::DynSerialize(d)
    }
}

impl<'a> Debug for AttributeValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DisplayAsDebug<D>(D);

        impl<D: Display> Debug for DisplayAsDebug<D> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
        }

        match self {
            Self::NotPresent => write!(f, "NotPresent"),
            Self::Unit => write!(f, "Unit"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Char(arg0) => f.debug_tuple("Char").field(arg0).finish(),
            Self::U64(arg0) => f.debug_tuple("U64").field(arg0).finish(),
            Self::I64(arg0) => f.debug_tuple("I64").field(arg0).finish(),
            Self::F64(arg0) => f.debug_tuple("F64").field(arg0).finish(),
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
            Self::Bytes(arg0) => f.debug_tuple("Bytes").field(arg0).finish(),
            Self::DynDisplay(arg0) => f.debug_tuple("DynDisplay").field(&DisplayAsDebug(arg0)).finish(),
            // TODO(https://github.com/rust-lang/rust/issues/117729): this replaces DisplayAsDebug workaround
            // Self::DynDisplay(arg0) => f.debug_tuple("DynDisplay").field_with(|f| arg0.fmt(f)).finish(),
            Self::DynDebug(arg0) => f.debug_tuple("DynDebug").field(arg0).finish(),
            // Self::DynError(arg0) => f.debug_tuple("DynError").field(arg0).finish(),
            Self::DynSerialize(_arg0) => f.debug_tuple("DynSerialize(TODO)").finish(), // TODO what to do here?
        }
    }
}

/* From: by-value types */
impl<'a> From<()     >           for AttributeValue<'a> { fn from(_    : ()   ) -> Self { Self::Unit } }
impl<'a> From<bool   >           for AttributeValue<'a> { fn from(value: bool ) -> Self { Self::Bool(value) } }
impl<'a> From<char   >           for AttributeValue<'a> { fn from(value: char ) -> Self { Self::Char(value) } }
impl<'a> From<u8     >           for AttributeValue<'a> { fn from(value: u8   ) -> Self { Self::U64 (value as u64) } }
impl<'a> From<u16    >           for AttributeValue<'a> { fn from(value: u16  ) -> Self { Self::U64 (value as u64) } }
impl<'a> From<u32    >           for AttributeValue<'a> { fn from(value: u32  ) -> Self { Self::U64 (value as u64) } }
impl<'a> From<u64    >           for AttributeValue<'a> { fn from(value: u64  ) -> Self { Self::U64 (value as u64) } }
impl<'a> From<usize  >           for AttributeValue<'a> { fn from(value: usize) -> Self { Self::U64 (value as u64) } }
impl<'a> From<i8     >           for AttributeValue<'a> { fn from(value: i8   ) -> Self { Self::I64 (value as i64) } }
impl<'a> From<i16    >           for AttributeValue<'a> { fn from(value: i16  ) -> Self { Self::I64 (value as i64) } }
impl<'a> From<i32    >           for AttributeValue<'a> { fn from(value: i32  ) -> Self { Self::I64 (value as i64) } }
impl<'a> From<i64    >           for AttributeValue<'a> { fn from(value: i64  ) -> Self { Self::I64 (value as i64) } }
impl<'a> From<isize  >           for AttributeValue<'a> { fn from(value: isize) -> Self { Self::I64 (value as i64) } }

impl<'a> From<f32    >           for AttributeValue<'a> { fn from(value: f32  ) -> Self { Self::F64 (value as f64) } }
impl<'a> From<f64    >           for AttributeValue<'a> { fn from(value: f64  ) -> Self { Self::F64 (value as f64) } }

/* From: reference types */
impl<'a> From<&'a str    >          for AttributeValue<'a> { fn from(value: &'a str    ) -> Self { Self::Str(value) } }
impl<'a> From<&'a String >          for AttributeValue<'a> { fn from(value: &'a String ) -> Self { Self::Str(value.as_str()) } }
impl<'a> From<&'a [u8]>             for AttributeValue<'a> { fn from(value: &'a [u8]   ) -> Self { Self::Bytes(value) } }
impl<'a> From<&'a Vec<u8>>          for AttributeValue<'a> { fn from(value: &'a Vec<u8>) -> Self { Self::Bytes(value.as_slice()) } }

impl<'a> From<&'a dyn Display  > for AttributeValue<'a> { fn from(value: &'a dyn Display  ) -> Self { Self::DynDisplay(value) } }
impl<'a> From<&'a dyn Debug    > for AttributeValue<'a> { fn from(value: &'a dyn Debug    ) -> Self { Self::DynDebug  (value) } }
impl<'a> From<&'a dyn Serialize> for AttributeValue<'a> { fn from(value: &'a dyn Serialize) -> Self { Self::DynSerialize(value) } }

/* From: &&T */

// impl<'a, T> From<&'a &'a T> for AttributeValue<'a>
//     where Self: From<&'a T>, T: ?Sized
// {
//     fn from(value: &'a &'a T) -> Self { From::from(&**value) }
// }

/* From: structured type */

// impl<'a, T> From<&'a T> for AttributeValue<'a>
//     where T: serde::Serialize
// {
//     fn from(value: &'a T) -> Self { Self::Structured(value) }
// }