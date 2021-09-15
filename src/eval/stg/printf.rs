//! Printf style %-formats
//!
//! This is a stopgap to maintain enough compatibility with the
//! Haskell implementation to pass tests for now.
//!
//! Largely lifted from
//! https://docs.rs/printf-compat/0.1.1/src/printf_compat/parser.rs.html#123-203
//!
//! This does not implement the full generality of printf - just the
//! subset of %-formatting specifications which makes sense.
//!
//! Only a single argument is passed and available and no dynamic
//! arguments may be specified.

// TODO: Move Native out of here - this should be testable without
// heap representation

use std::fmt::{self, Write};

use bitflags::bitflags;
use serde_json::Number;

use crate::eval::memory::{mutator::MutatorHeapView, syntax::Native};

bitflags! {
    /// Flags field.
    ///
    /// Definitions from
    /// [Wikipedia](https://en.wikipedia.org/wiki/Printf_format_string#Flags_field).
    pub struct Flags: u8 {
        /// Left-align the output of this placeholder. (The default is to
        /// right-align the output.)
        const LEFT_ALIGN = 0b00000001;
        /// Prepends a plus for positive signed-numeric types. positive =
        /// `+`, negative = `-`.
        ///
        /// (The default doesn't prepend anything in front of positive
        /// numbers.)
        const PREPEND_PLUS = 0b00000010;
        /// Prepends a space for positive signed-numeric types. positive = `
        /// `, negative = `-`. This flag is ignored if the
        /// [`PREPEND_PLUS`][Flags::PREPEND_PLUS] flag exists.
        ///
        /// (The default doesn't prepend anything in front of positive
        /// numbers.)
        const PREPEND_SPACE = 0b00000100;
        /// When the 'width' option is specified, prepends zeros for numeric
        /// types. (The default prepends spaces.)
        ///
        /// For example, `printf("%4X",3)` produces `   3`, while
        /// `printf("%04X",3)` produces `0003`.
        const PREPEND_ZERO = 0b00001000;
        /// The integer or exponent of a decimal has the thousands grouping
        /// separator applied.
        const THOUSANDS_GROUPING = 0b00010000;
        /// Alternate form:
        ///
        /// For `g` and `G` types, trailing zeros are not removed. \
        /// For `f`, `F`, `e`, `E`, `g`, `G` types, the output always
        /// contains a decimal point. \ For `o`, `x`, `X` types,
        /// the text `0`, `0x`, `0X`, respectively, is prepended
        /// to non-zero numbers.
        const ALTERNATE_FORM = 0b00100000;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DoubleFormat {
    /// `f`
    Normal,
    /// `F`
    UpperNormal,
    /// `e`
    Scientific,
    /// `E`
    UpperScientific,
    /// `g`
    Auto,
    /// `G`
    UpperAuto,
    /// `a`
    Hex,
    /// `A`
    UpperHex,
}

/// A [format specifier](https://en.wikipedia.org/wiki/Printf_format_string#Type_field).
#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub enum Specifier {
    /// `d`, `i`
    Int,
    /// `u`
    Uint,
    /// `o`
    Octal,
    /// `f`, `F`, `e`, `E`, `g`, `G`, `a`, `A`
    Double { format: DoubleFormat },
    /// string outside of formatting
    Bytes,
    /// `s`
    ///
    /// The same as [`Bytes`][Specifier::Bytes] but guaranteed to be
    /// null-terminated. This can be used for optimizations, where if you
    /// need to null terminate a string to print it, you can skip that step.
    String,
    /// `c`
    Char,
    /// `x`
    Hex,
    /// `X`
    UpperHex,
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Argument {
    pub flags: Flags,
    pub width: usize,
    pub precision: Option<usize>,
    pub length: Length,
    pub specifier: Specifier,
}

fn next_char(sub: &[u8]) -> &[u8] {
    sub.get(1..).unwrap_or(&[])
}

/// Parse the [Flags field](https://en.wikipedia.org/wiki/Printf_format_string#Flags_field).
fn parse_flags(mut sub: &[u8]) -> (Flags, &[u8]) {
    let mut flags: Flags = Flags::empty();
    while let Some(&ch) = sub.get(0) {
        flags.insert(match ch {
            b'-' => Flags::LEFT_ALIGN,
            b'+' => Flags::PREPEND_PLUS,
            b' ' => Flags::PREPEND_SPACE,
            b'0' => Flags::PREPEND_ZERO,
            b'\'' => Flags::THOUSANDS_GROUPING,
            b'#' => Flags::ALTERNATE_FORM,
            _ => break,
        });
        sub = next_char(sub)
    }
    (flags, sub)
}

/// Parse the [Width
/// field](https://en.wikipedia.org/wiki/Printf_format_string#Width_field).
///
/// Does not support the dynamic argument '*' specifier
fn parse_width(mut sub: &[u8]) -> (usize, &[u8]) {
    let mut width: usize = 0;
    while let Some(&ch) = sub.get(0) {
        match ch {
            // https://rust-malaysia.github.io/code/2020/07/11/faster-integer-parsing.html#the-bytes-solution
            b'0'..=b'9' => width = width * 10 + (ch & 0x0f) as usize,
            _ => break,
        }
        sub = next_char(sub);
    }
    (width, sub)
}

/// Parse the [Precision
/// field](https://en.wikipedia.org/wiki/Printf_format_string#Precision_field).
///
/// Does not support the dynamic argument '*' specifier.
fn parse_precision(sub: &[u8]) -> (Option<usize>, &[u8]) {
    match sub.get(0) {
        Some(&b'.') => {
            let (prec, sub) = parse_width(next_char(sub));
            (Some(prec), sub)
        }
        _ => (None, sub),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Length {
    Int,
    /// `hh`
    Char,
    /// `h`
    Short,
    /// `l`
    Long,
    /// `ll`
    LongLong,
    /// `z`
    Usize,
    /// `t`
    Isize,
}

/// Parse the [Length field](https://en.wikipedia.org/wiki/Printf_format_string#Length_field).
fn parse_length(sub: &[u8]) -> (Length, &[u8]) {
    match sub.get(0).copied() {
        Some(b'h') => match sub.get(1).copied() {
            Some(b'h') => (Length::Char, sub.get(2..).unwrap_or(&[])),
            _ => (Length::Short, next_char(sub)),
        },
        Some(b'l') => match sub.get(1).copied() {
            Some(b'l') => (Length::LongLong, sub.get(2..).unwrap_or(&[])),
            _ => (Length::Long, next_char(sub)),
        },
        Some(b'z') => (Length::Usize, next_char(sub)),
        Some(b't') => (Length::Isize, next_char(sub)),
        _ => (Length::Int, sub),
    }
}

/// Parse a format parameter and write it somewhere.
///
fn parse_format(format: &str) -> Option<Argument> {
    assert!(format.starts_with('%'));
    let sub = next_char(format.as_bytes());

    let (flags, sub) = parse_flags(sub);
    let (width, sub) = parse_width(sub);
    let (precision, sub) = parse_precision(sub);
    let (length, sub) = parse_length(sub);
    let ch = sub.get(0).unwrap_or(&0);

    let specifier = match ch {
        b'd' | b'i' => Specifier::Int,
        b'x' => Specifier::Hex,
        b'X' => Specifier::UpperHex,
        b'u' => Specifier::Uint,
        b'o' => Specifier::Octal,
        b'f' => Specifier::Double {
            format: DoubleFormat::Normal,
        },
        b'F' => Specifier::Double {
            format: DoubleFormat::UpperNormal,
        },
        b'e' => Specifier::Double {
            format: DoubleFormat::Scientific,
        },
        b'E' => Specifier::Double {
            format: DoubleFormat::UpperScientific,
        },
        b'g' => Specifier::Double {
            format: DoubleFormat::Auto,
        },
        b'G' => Specifier::Double {
            format: DoubleFormat::UpperAuto,
        },
        b'a' => Specifier::Double {
            format: DoubleFormat::Hex,
        },
        b'A' => Specifier::Double {
            format: DoubleFormat::UpperHex,
        },
        b's' => Specifier::String,
        b'c' => Specifier::Char,
        _ => return None,
    };

    Some(Argument {
        flags,
        width,
        precision,
        length,
        specifier,
    })
}

/// Error while processing a printf format
#[derive(Debug)]
pub enum PrintfError {
    /// Could not convert to appropriate numeric type
    ConversionError,
    /// fmt error
    FmtError(fmt::Error),
    /// Invalid format string
    InvalidFormatString(String),
}

fn write_str(
    w: &mut impl fmt::Write,
    flags: Flags,
    width: usize,
    precision: Option<usize>,
    string: &str,
) -> Result<(), PrintfError> {
    let precision = precision.unwrap_or(string.len() as usize);
    if flags.contains(Flags::LEFT_ALIGN) {
        write!(
            w,
            "{:1$.prec$}",
            string,
            width as usize,
            prec = precision as usize
        )
        .map_err(PrintfError::FmtError)
    } else {
        write!(
            w,
            "{:>1$.prec$}",
            string,
            width as usize,
            prec = precision as usize
        )
        .map_err(PrintfError::FmtError)
    }
}

fn write_numeric(
    w: &mut impl fmt::Write,
    flags: Flags,
    width: usize,
    precision: Option<usize>,
    specifier: Specifier,
    num: &Number,
) -> Result<(), PrintfError> {
    macro_rules! write_using {
        ($flags: expr, $type: expr, $prec: expr, $num: expr) => {{
            if let Some(p) = $prec {
                write!(
                    w,
                    concat!("{:", $flags, "width$.prec$", $type, "}"),
                    $num,
                    width = width as usize,
                    prec = p as usize
                )
                .map_err(PrintfError::FmtError)
            } else {
                write!(
                    w,
                    concat!("{:", $flags, "width$", $type, "}"),
                    $num,
                    width = width as usize,
                )
                .map_err(PrintfError::FmtError)
            }
        }};
    }

    macro_rules! write_flags {
        ($flags: expr) => {{
            match specifier {
                Specifier::Octal => {
                    if let Some(i) = num.as_i64() {
                        write_using!($flags, "o", precision, i)
                    } else {
                        Err(PrintfError::ConversionError)
                    }
                }
                Specifier::Hex => {
                    if let Some(i) = num.as_i64() {
                        write_using!($flags, "x", precision, i)
                    } else {
                        Err(PrintfError::ConversionError)
                    }
                }
                Specifier::UpperHex => {
                    if let Some(i) = num.as_i64() {
                        write_using!($flags, "X", precision, i)
                    } else {
                        Err(PrintfError::ConversionError)
                    }
                }
                Specifier::Double { format } => match format {
                    DoubleFormat::Scientific => {
                        if let Some(f) = num.as_f64() {
                            write_using!($flags, "e", precision, f)
                        } else {
                            Err(PrintfError::ConversionError)
                        }
                    }
                    DoubleFormat::UpperScientific => {
                        if let Some(f) = num.as_f64() {
                            write_using!($flags, "E", precision, f)
                        } else {
                            Err(PrintfError::ConversionError)
                        }
                    }
                    _ => {
                        if let Some(f) = num.as_f64() {
                            write_using!($flags, "", precision, f)
                        } else {
                            Err(PrintfError::ConversionError)
                        }
                    }
                },
                _ => {
                    if let Some(f) = num.as_f64() {
                        write_using!($flags, "", precision, f)
                    } else {
                        Err(PrintfError::ConversionError)
                    }
                }
            }
        }};
    }

    if flags.contains(Flags::LEFT_ALIGN) {
        if flags.contains(Flags::PREPEND_PLUS) {
            write_flags!("<+")
        } else {
            write_flags!("<")
        }
    } else if flags.contains(Flags::PREPEND_PLUS) {
        if flags.contains(Flags::PREPEND_ZERO) {
            write_flags!("+0")
        } else {
            write_flags!("+")
        }
    } else if flags.contains(Flags::PREPEND_ZERO) {
        write_flags!("0")
    } else {
        write_flags!("")
    }
}

/// Format a single value according to a printf-style % spec
pub fn fmt<'guard>(
    view: MutatorHeapView<'guard>,
    fmt_string: &str,
    nat: &Native,
) -> Result<String, PrintfError> {
    let mut output = String::new();
    if let Some(printf_spec) = parse_format(fmt_string) {
        match nat {
            Native::Sym(s) | Native::Str(s) => {
                let string = (*view.scoped(*s)).as_str().to_string();

                write_str(
                    &mut output,
                    printf_spec.flags,
                    printf_spec.width,
                    printf_spec.precision,
                    string.as_str(),
                )?
            }
            Native::Num(num) => write_numeric(
                &mut output,
                printf_spec.flags,
                printf_spec.width,
                printf_spec.precision,
                printf_spec.specifier,
                num,
            )?,
            Native::Zdt(dt) => write!(&mut output, "{}", dt).map_err(PrintfError::FmtError)?,
        }
        Ok(output)
    } else {
        Err(PrintfError::InvalidFormatString(fmt_string.to_string()))
    }
}
