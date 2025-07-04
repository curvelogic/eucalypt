//! Types for intrinsic functions
use crate::core::expr::{Expr, Primitive, RcExpr};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntrinsicType {
    Unknown,
    Any,
    Unit,
    Bool,
    String,
    Symbol,
    Number,
    ZonedDateTime,
    List(Box<IntrinsicType>),
    Function(Box<IntrinsicType>, Box<IntrinsicType>),
    Record(HashMap<String, IntrinsicType>),
}

impl fmt::Display for IntrinsicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntrinsicType::Unknown => write!(f, "?"),
            IntrinsicType::Any => write!(f, "⊤"),
            IntrinsicType::Unit => write!(f, "()"),
            IntrinsicType::Bool => write!(f, "Bool"),
            IntrinsicType::String => write!(f, "Str"),
            IntrinsicType::Symbol => write!(f, "Sym"),
            IntrinsicType::Number => write!(f, "Num"),
            IntrinsicType::ZonedDateTime => write!(f, "Zdt"),
            IntrinsicType::List(t) => write!(f, "[{}]", *t),
            IntrinsicType::Function(i, o) => write!(f, "{i} -> {o}"),
            IntrinsicType::Record(hm) => {
                write!(f, "{{")?;
                let mut it = hm.iter();
                if let Some((k, t)) = it.next() {
                    write!(f, "{}: {}", k, *t)?;
                }
                for (k, t) in it {
                    write!(f, " ,")?;
                    write!(f, "{}: {}", k, *t)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl IntrinsicType {
    pub fn guess(expr: RcExpr) -> Box<Self> {
        match &*expr.inner {
            Expr::Literal(_, Primitive::Null) => unit(),
            Expr::Literal(_, Primitive::Bool(_)) => bool_(),
            Expr::Literal(_, Primitive::Sym(_)) => sym(),
            Expr::Literal(_, Primitive::Str(_)) => str_(),
            Expr::Literal(_, Primitive::Num(_)) => num(),
            Expr::List(..) => list(),
            Expr::Block(..) => block(),
            Expr::Lam(..) | Expr::Intrinsic(_, _) => arrow(unk(), unk()),
            _ => unk(),
        }
    }

    pub fn arity(&self) -> Option<usize> {
        match self {
            IntrinsicType::Function(_, o) => o.arity().map(|a| a + 1),
            _ => Some(0),
        }
    }

    pub fn arg(&self, index: usize) -> Option<&Self> {
        match self {
            IntrinsicType::Function(i, o) => {
                if index == 0 {
                    Some(i)
                } else {
                    Some(o.arg(index - 1)?)
                }
            }
            _ => None,
        }
    }

    pub fn ret(&self, index: usize) -> Option<&Self> {
        match self {
            IntrinsicType::Function(_, o) => {
                if index == 0 {
                    Some(o)
                } else {
                    Some(o.ret(index - 1)?)
                }
            }
            _ => None,
        }
    }
}

pub fn record() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Record(HashMap::new()))
}

pub fn arrow(from: Box<IntrinsicType>, to: Box<IntrinsicType>) -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Function(from, to))
}

pub fn function(xs: impl IntoIterator<Item = Box<IntrinsicType>>) -> Option<Box<IntrinsicType>> {
    let mut iter = xs.into_iter();
    if let Some(i) = iter.next() {
        if let Some(o) = function(iter) {
            Some(arrow(i, o))
        } else {
            Some(i)
        }
    } else {
        None
    }
}

pub fn bool_() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Bool)
}

pub fn sym() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Symbol)
}

pub fn str_() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::String)
}

pub fn num() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Number)
}

pub fn zdt() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::ZonedDateTime)
}

pub fn unk() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Unknown)
}

pub fn unit() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Unit)
}

pub fn any() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Any)
}

pub fn list() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::List(any()))
}

pub fn block() -> Box<IntrinsicType> {
    Box::new(IntrinsicType::Record(HashMap::new()))
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn arity_tests() {
        assert_eq!(
            function(vec![record(), unk(), unk()])
                .unwrap()
                .arity()
                .unwrap(),
            2
        );
    }

    #[test]
    pub fn arg_tests() {
        assert_eq!(
            function(vec![record(), num(), num()])
                .unwrap()
                .arg(0)
                .unwrap(),
            record().as_ref()
        );

        assert_eq!(
            function(vec![num(), num(), num()]).unwrap().arg(1).unwrap(),
            num().as_ref()
        )
    }

    #[test]
    pub fn ret_tests() {
        assert_eq!(
            function(vec![record(), num(), num()])
                .unwrap()
                .ret(1)
                .unwrap(),
            num().as_ref()
        );

        assert_eq!(
            function(vec![num(), num(), bool_()])
                .unwrap()
                .ret(1)
                .unwrap(),
            bool_().as_ref()
        );
    }
}
