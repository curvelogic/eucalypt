//! Dependencies that core processing has on runtime (intrinsics
//! etc.)
use crate::core::expr::acore;
use crate::core::expr::RcExpr;

pub fn join() -> RcExpr {
    acore::bif("JOIN")
}

pub fn fmt() -> RcExpr {
    acore::bif("FMT")
}

pub fn str() -> RcExpr {
    acore::bif("STR")
}
