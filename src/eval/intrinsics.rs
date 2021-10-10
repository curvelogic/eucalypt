//! Intrinsic info
use std::convert::TryInto;

use crate::common::sourcemap::Smid;
use crate::core::expr::{acore, core, RcExpr};
use crate::eval::types::*;

pub struct Intrinsic {
    name: &'static str,
    ty: Box<IntrinsicType>,
    strict: Vec<usize>,
}

impl Intrinsic {
    pub fn new(name: &'static str, ty: Box<IntrinsicType>, strict: Vec<usize>) -> Self {
        Intrinsic { name, ty, strict }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.ty.arity().unwrap()
    }

    pub fn strict_args(&self) -> &Vec<usize> {
        &self.strict
    }

    pub fn ty(&self) -> &IntrinsicType {
        self.ty.as_ref()
    }

    pub fn acore(&self) -> RcExpr {
        acore::bif(self.name)
    }

    pub fn core(&self, smid: Smid) -> RcExpr {
        core::bif(smid, self.name)
    }
}

lazy_static! {
    static ref INTRINSICS: Vec<Intrinsic> = vec![
        Intrinsic { // 0
            name: "LOOKUP", // allows str or sym?
            ty: function(vec![block(), unk(), unk()]).unwrap(),
            strict: vec![0, 1],
        },
        Intrinsic { // 1
            name: "LOOKUPOR", // boxed sym
            ty: function(vec![block(), unk(), unk(), unk()]).unwrap(),
            strict: vec![0, 1],
        },
        Intrinsic { // 2
            name: "RENDER",
            ty: arrow(any(), unit()),
            strict: vec![0],
        },
        Intrinsic { // 3
            name: "EMIT0",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 4
            name: "EMITT",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 5
            name: "EMITF",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 6
            name: "EMITx",
            ty: function(vec![any(), unit()]).unwrap(),
            strict: vec![0],
        },
        Intrinsic { // 7
            name: "NV.EMIT[*]",
            ty: unit(),
            strict: vec![0], // special case handling
        },
        Intrinsic { // 8
            name: "NV.EMIT{*}",
            ty: unit(),
            strict: vec![0], // special case handling
        },
        Intrinsic { // 9
            name: "Emit.RenderKV",
            ty: function(vec![sym(), any(), unit()]).unwrap(),
            strict: vec![0, 1],
        },
        Intrinsic { // 10
            name: "IF",
            ty: function(vec![bool_(), any(), any(), any()]).unwrap(),
            strict: vec![0],
        },
        Intrinsic { // 11
            name: "EQ",
            ty: function(vec![any(), any(), bool_()]).unwrap(),
            strict: vec![0, 1],
        },
        Intrinsic { // 12
            name: "NV.ALL[*]",
            ty: unit(),
            strict: vec![], // special case handling
        },
    Intrinsic { //13
            name: "NULL",
            ty: unit(),
            strict: vec![],
    },
    Intrinsic { // 14
            name: "ADD",
            ty: function(vec![num(), num(), num()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 15
            name: "SUB",
            ty: function(vec![num(), num(), num()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 16
            name: "MUL",
            ty: function(vec![num(), num(), num()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 17
            name: "DIV",
            ty: function(vec![num(), num(), num()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 18
            name: "PANIC",
            ty: function(vec![str_(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 19
            name: "BLOCK",
            ty: function(vec![list(), block()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { // 20
            name: "KV", // KV can take block KV or list and return block KV
            ty: function(vec![unk(), unk()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { // 21
            name: "MATCHES_KEY",
            ty: function(vec![unk(), sym()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 22
            name: "EXTRACT_VALUE", // can take any key-value form
            ty: function(vec![unk(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 23
            name: "AND",
            ty: function(vec![bool_(), bool_(), bool_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 24
            name: "OR",
            ty: function(vec![bool_(), bool_(), bool_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 25
            name: "NOT",
            ty: function(vec![bool_(), bool_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 26
            name: "TRUE",
            ty: bool_(),
            strict: vec![],
    },
    Intrinsic { // 27
            name: "FALSE",
            ty: bool_(),
            strict: vec![],
    },
        Intrinsic { // 28
            name: "RENDER_ITEMS",
            ty: arrow(any(), unit()),
            strict: vec![0],
        },
        Intrinsic { // 29
            name: "RENDER_BLOCK_ITEMS",
            ty: arrow(any(), unit()),
            strict: vec![0],
        },
        Intrinsic { // 30
            name: "RENDER_KV",
            ty: arrow(any(), unit()),
            strict: vec![0],
         },
        Intrinsic { // 31
            name: "EMIT[",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 32
            name: "EMIT]",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 33
            name: "EMIT{",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 34
            name: "EMIT}",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 35
            name: "SATURATED",
            ty: arrow(any(), bool_()),
            strict: vec![0],
        },
        Intrinsic { // 36
            name: "RENDER_DOC",
            ty: arrow(any(), unit()),
            strict: vec![0],
        },
        Intrinsic { // 37
            name: "EMIT<",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 38
            name: "EMIT>",
            ty: unit(),
            strict: vec![],
        },
        Intrinsic { // 39
            name: "LOOKUPOR#",
            ty: function(vec![block(), unk(), unk(), unk()]).unwrap(),
            strict: vec![0, 1],
        },
        Intrinsic { // 40
            name: "CONS",
            ty: function(vec![any(), list(), list()]).unwrap(),
            strict: vec![],
        },
    Intrinsic { //41
            name: "NIL",
            ty: function(vec![list(), bool_()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { //42
            name: "HEAD",
            ty: function(vec![list(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { //43
            name: "TAIL",
            ty: function(vec![list(), list()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { //44
            name: "REVERSE",
            ty: function(vec![list(), list()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { //45
            name: "MERGE",
            ty: function(vec![record(), record(), record()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { //46
            name: "ELEMENTS",
            ty: function(vec![record(), list()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { //47
            name: "META",
            ty: function(vec![any(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { //48
            name: "WITHMETA",
            ty: function(vec![any(), any(), unk()]).unwrap(),
            strict: vec![],
    },
    Intrinsic { // 49
            name: "LT",
            ty: function(vec![num(), num(), bool_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 50
            name: "GT",
            ty: function(vec![num(), num(), bool_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 51
            name: "LTE",
            ty: function(vec![num(), num(), bool_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 52
            name: "GTE",
            ty: function(vec![num(), num(), bool_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 53
            name: "MOD",
            ty: function(vec![num(), num(), num()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 54
            name: "FLOOR",
            ty: function(vec![num(), num()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 55
            name: "CEILING",
            ty: function(vec![num(), num()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 56
            name: "STR",
            ty: function(vec![any(), str_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 57
            name: "SPLIT",
            ty: function(vec![str_(), str_(), list()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 58
            name: "MATCH",
            ty: function(vec![str_(), str_(), list()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 59
            name: "MATCHES",
            ty: function(vec![str_(), str_(), list()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 60
            name: "JOIN",
            ty: function(vec![list(), str_(), str_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 61
            name: "LETTERS",
            ty: function(vec![str_(), list()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 62
            name: "FMT",
            ty: function(vec![any(), str_(), str_()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 63
            name: "UPPER",
            ty: function(vec![str_(), str_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 64
            name: "LOWER",
            ty: function(vec![str_(), str_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 65
            name: "SYM",
            ty: function(vec![str_(), sym()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 66
            name: "seqStrList",
            ty: function(vec![list(), list()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 67
            name: "DEKV",
            ty: function(vec![unk(), list()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 68
            name: "EXTRACT_KEY", // can take any key-value form
            ty: function(vec![unk(), sym()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 69
            name: "PACK_PAIR", // can take any key-value form
            ty: function(vec![unk(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 70
            name: "BLOCK_PAIR", // can take any key-value form
            ty: function(vec![unk(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 71
            name: "MERGEWITH",
            ty: function(vec![record(), record(), unk(), record()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 72
            name: "DEEPMERGE",
            ty: function(vec![any(), any(), unk()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 73
            name: "NUMPARSE",
            ty: function(vec![str_(), num()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 74
            name: "ZDT",
            ty: function(vec![num(), num(), num(), num(), num(), num(), str_(), zdt()]).unwrap(),
            strict: vec![0, 1, 2, 3, 4, 5, 6],
    },
    Intrinsic { // 75
            name: "ZDT.FROM_EPOCH",
            ty: function(vec![num(), zdt()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 76
            name: "ZDT.FIELDS",
            ty: function(vec![zdt(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 77
            name: "IFIELDS",
            ty: function(vec![num(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 78
            name: "ZDT.PARSE",
            ty: function(vec![str_(), zdt()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 79
            name: "ZDT.FORMAT",
            ty: function(vec![zdt(), str_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 80
            name: "SUPPRESSES",
            ty: function(vec![record(), bool_()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 81
            name: "KNIL",
            ty: unit(),
            strict: vec![],
    },
    Intrinsic { // 82
            name: "K[]",
            ty: list(),
            strict: vec![],
    },
    Intrinsic { // 83
            name: "K{}",
            ty: record(),
            strict: vec![],
    },
    Intrinsic { // 84
            name: "DQ",
            ty: str_(),
            strict: vec![],
    },
    Intrinsic { // 85
            name: "EMITTAGx",
            ty: function(vec![str_(), any(), unk()]).unwrap(),
            strict: vec![0, 1],
    },
    Intrinsic { // 86
            name: "EMITTAG[",
            ty: function(vec![str_(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 87
            name: "EMITTAG{",
            ty: function(vec![str_(), unk()]).unwrap(),
            strict: vec![0],
    },
    Intrinsic { // 88
            name: "TAG",
            ty: function(vec![record(), str_()]).unwrap(),
            strict: vec![0],
    },

    ];
}

/// Find the index of the intrinisc with the specified name
pub fn index(name: &str) -> Option<usize> {
    INTRINSICS.iter().position(|i| i.name == name)
}

/// Force index into a u8
pub fn index_u8(name: &str) -> u8 {
    index(name).unwrap().try_into().unwrap()
}

/// Find the catalogue entry for the intrinsic at the given index
pub fn intrinsic(index: usize) -> &'static Intrinsic {
    &INTRINSICS[index]
}

/// Retrieve the whole ordered catalogue of intrinsics
pub fn catalogue() -> &'static [Intrinsic] {
    &INTRINSICS
}
