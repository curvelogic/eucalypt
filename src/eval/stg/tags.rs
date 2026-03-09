//! Predefined data tags and arities

use std::convert::TryFrom;
use std::fmt;

/// Datatype tag
pub type Tag = u8;

/// Predefined data type tags
#[derive(Copy, Clone)]
pub enum DataConstructor {
    Unit = 0,
    BoolTrue = 1,
    BoolFalse = 2,
    BoxedNumber = 3,
    BoxedSymbol = 4,
    BoxedString = 5,
    /// Empty list
    ListNil = 6,
    /// Propert list cons cell
    ListCons = 7,
    /// Default blocks are constructed as, but LOOKUP is polymorphic
    /// and works on alternative structures too.
    ///
    /// BLOCK (LIST_CONS (BLOCK_PAIR x y) (LIST_CONS (BLOCK_PAIR x y) LIST_NIL))
    Block = 8,
    /// BLOCK_PAIR is a pair of *unboxed* symbol and value
    BlockPair = 9,
    /// BLOCK_KV_LIST marks a list of which the first two elements are
    /// interpreted as KV
    BlockKvList = 10,
    /// Boxed zoned datetime
    BoxedZdt = 11,
    /// IO monad: pure value wrapper — (world, value)
    IoReturn = 12,
    /// IO monad: sequenced action — (world, action, continuation)
    IoBind = 13,
    /// IO monad: primitive action — (world, spec_block)
    IoAction = 14,
    /// IO monad: failure — (world, error)
    IoFail = 15,
}

impl fmt::Display for DataConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataConstructor::Unit => write!(f, "null"),
            DataConstructor::BoolTrue => write!(f, "true"),
            DataConstructor::BoolFalse => write!(f, "false"),
            DataConstructor::BoxedNumber => write!(f, "number"),
            DataConstructor::BoxedSymbol => write!(f, "symbol"),
            DataConstructor::BoxedString => write!(f, "string"),
            DataConstructor::ListNil => write!(f, "empty list"),
            DataConstructor::ListCons => write!(f, "list"),
            DataConstructor::Block => write!(f, "block"),
            DataConstructor::BlockPair => write!(f, "key-value pair"),
            DataConstructor::BlockKvList => write!(f, "key-value list"),
            DataConstructor::BoxedZdt => write!(f, "datetime"),
            DataConstructor::IoReturn => write!(f, "io-return"),
            DataConstructor::IoBind => write!(f, "io-bind"),
            DataConstructor::IoAction => write!(f, "io-action"),
            DataConstructor::IoFail => write!(f, "io-fail"),
        }
    }
}

impl DataConstructor {
    pub fn tag(self) -> Tag {
        self as Tag
    }

    pub fn arity(self) -> usize {
        match self {
            DataConstructor::Unit => 0,
            DataConstructor::BoolTrue => 0,
            DataConstructor::BoolFalse => 0,
            DataConstructor::BoxedNumber => 1,
            DataConstructor::BoxedSymbol => 1,
            DataConstructor::BoxedString => 1,
            DataConstructor::ListNil => 0,
            DataConstructor::ListCons => 2,
            DataConstructor::Block => 2,
            DataConstructor::BlockPair => 2,
            DataConstructor::BlockKvList => 2,
            DataConstructor::BoxedZdt => 1,
            DataConstructor::IoReturn => 2,
            DataConstructor::IoBind => 3,
            DataConstructor::IoAction => 2,
            DataConstructor::IoFail => 2,
        }
    }
}

impl DataConstructor {
    /// Returns true if `tag` is an IO monad constructor (tags 12–15).
    ///
    /// The `io-run` driver loop uses this to recognise IO constructors
    /// that should cause the machine to yield rather than terminate.
    pub fn is_io_constructor(tag: Tag) -> bool {
        tag == DataConstructor::IoReturn as Tag
            || tag == DataConstructor::IoBind as Tag
            || tag == DataConstructor::IoAction as Tag
            || tag == DataConstructor::IoFail as Tag
    }
}

impl TryFrom<Tag> for DataConstructor {
    type Error = ();

    fn try_from(value: Tag) -> Result<Self, Self::Error> {
        match value {
            value if value == DataConstructor::Unit as Tag => Ok(DataConstructor::Unit),
            value if value == DataConstructor::BoolTrue as Tag => Ok(DataConstructor::BoolTrue),
            value if value == DataConstructor::BoolFalse as Tag => Ok(DataConstructor::BoolFalse),
            value if value == DataConstructor::BoxedNumber as Tag => {
                Ok(DataConstructor::BoxedNumber)
            }
            value if value == DataConstructor::BoxedSymbol as Tag => {
                Ok(DataConstructor::BoxedSymbol)
            }
            value if value == DataConstructor::BoxedString as Tag => {
                Ok(DataConstructor::BoxedString)
            }
            value if value == DataConstructor::ListNil as Tag => Ok(DataConstructor::ListNil),
            value if value == DataConstructor::ListCons as Tag => Ok(DataConstructor::ListCons),
            value if value == DataConstructor::Block as Tag => Ok(DataConstructor::Block),
            value if value == DataConstructor::BlockPair as Tag => Ok(DataConstructor::BlockPair),
            value if value == DataConstructor::BlockKvList as Tag => {
                Ok(DataConstructor::BlockKvList)
            }
            value if value == DataConstructor::BoxedZdt as Tag => Ok(DataConstructor::BoxedZdt),
            value if value == DataConstructor::IoReturn as Tag => Ok(DataConstructor::IoReturn),
            value if value == DataConstructor::IoBind as Tag => Ok(DataConstructor::IoBind),
            value if value == DataConstructor::IoAction as Tag => Ok(DataConstructor::IoAction),
            value if value == DataConstructor::IoFail as Tag => Ok(DataConstructor::IoFail),
            _ => Err(()),
        }
    }
}
