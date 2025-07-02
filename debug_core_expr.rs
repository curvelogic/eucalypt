#!/usr/bin/env -S cargo +stable script --edition 2021 --manifest-path
//! ```cargo
//! [dependencies]
//! eucalypt = { path = "." }
//! codespan_reporting = "0.11"
//! ```

use eucalypt::driver::source::SourceLoader;
use eucalypt::syntax::input::{Input, Locator};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "{foo: 99}.foo";
    println!("=== LALRPOP Core Expression for: {} ===", input);
    
    let mut loader = SourceLoader::default();
    let loc = Locator::Cli(input.to_string());
    loader.load(&Input::from(loc.clone()))?;
    let unit = loader.translate(&Input::from(loc))?;
    
    println!("{:#?}", unit.expr);
    Ok(())
}