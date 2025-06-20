//! InfoTable is the static part of a closure including code, arity,
//! annotation etc.

use crate::common::sourcemap::Smid;

pub trait InfoTable {
    fn update(&self) -> bool;
    fn arity(&self) -> u8;
    fn annotation(&self) -> Smid;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct InfoFlags(u64);

impl InfoFlags {
    pub fn new(update: bool, arity: u8, annotation: Smid) -> Self {
        let smid: u32 = annotation.into();
        let mut value = ((smid as u64) << 32) | ((arity as u64) << 24);
        if update {
            value += 1;
        }
        InfoFlags(value)
    }
}

impl InfoTable for InfoFlags {
    fn update(&self) -> bool {
        self.0 & 1 == 1
    }

    fn arity(&self) -> u8 {
        ((self.0 & 0xff000000) >> 24) as u8
    }

    fn annotation(&self) -> Smid {
        Smid::from((self.0 >> 32) as u32)
    }
}

/// The static part of a closure which combines with an environment to
/// become a closure.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct InfoTagged<L>
where
    L: Copy,
{
    info: InfoFlags,
    body: L,
}

impl<L> Default for InfoTagged<L>
where
    L: Default,
    L: Copy,
{
    fn default() -> Self {
        Self {
            info: Default::default(),
            body: Default::default(),
        }
    }
}

impl<L> InfoTable for InfoTagged<L>
where
    L: Copy,
{
    /// Source annotation to stamp on environment
    fn annotation(&self) -> Smid {
        self.info.annotation()
    }

    /// The arity of the the lambda form
    fn arity(&self) -> u8 {
        self.info.arity()
    }

    /// Whether lambda form is a thunk to be updated in place
    fn update(&self) -> bool {
        self.info.update()
    }
}

impl<L> InfoTagged<L>
where
    L: Copy,
{
    /// Create new lambda form - local vars < `bound` are bound vars.
    pub fn new(bound: u8, body: L, annotation: Smid) -> Self {
        InfoTagged {
            info: InfoFlags::new(false, bound, annotation),
            body,
        }
    }

    /// A lambda form that will be updated after evaluation
    pub fn thunk(body: L) -> Self {
        InfoTagged {
            info: InfoFlags::new(true, 0, Smid::default()),
            body,
        }
    }

    /// A lambda form that is effectively a value - not worth updating
    pub fn value(body: L) -> Self {
        InfoTagged {
            info: InfoFlags::new(false, 0, Smid::default()),
            body,
        }
    }

    /// Reference the body of the lambda form
    pub fn body(&self) -> L {
        self.body
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        common::sourcemap::Smid,
        eval::memory::infotable::{InfoFlags, InfoTable},
    };

    #[test]
    pub fn test_info_flags() {
        let info = InfoFlags::new(true, 12, Smid::from(99));
        assert!(info.update());
        assert_eq!(info.arity(), 12);
        assert_eq!(info.annotation(), Smid::from(99));
    }
}
