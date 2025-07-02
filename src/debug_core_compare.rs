//! Debug utility to compare LALRPOP and Rowan core expressions

#[cfg(test)]
mod tests {
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::{Input, Locator};

    #[test]
    fn debug_lalrpop_simple_lookup() {
        let input = "{foo: 99}.foo";
        println!("=== LALRPOP Core Expression for: {} ===", input);
        
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(input.to_string());
        loader.load(&Input::from(loc.clone())).unwrap();
        let unit = loader.translate(&Input::from(loc)).unwrap();
        
        println!("{:#?}", unit.expr);
    }
    
    #[test] 
    fn debug_lalrpop_multi_lookup() {
        let input = "{data: {foo: 99}}.data.foo";
        println!("=== LALRPOP Core Expression for: {} ===", input);
        
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(input.to_string());
        loader.load(&Input::from(loc.clone())).unwrap();
        let unit = loader.translate(&Input::from(loc)).unwrap();
        
        println!("{:#?}", unit.expr);
    }
}