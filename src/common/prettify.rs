//! Basic pretty printing facilities

use pretty::{DocAllocator, DocBuilder, RcAllocator};

pub trait ToPretty {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone;
}

pub fn prettify<I>(expr: &I) -> String
where
    I: ToPretty,
{
    let allocator = RcAllocator;
    let doc = expr.pretty::<_, ()>(&allocator).append(allocator.line());
    let mut w = Vec::new();
    doc.1.render(80, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}
