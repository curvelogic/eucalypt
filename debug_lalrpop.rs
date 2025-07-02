// Debug program to show core expressions from LALRPOP parser
use eucalypt::driver::source::SourceLoader;
use eucalypt::syntax::input::{Input, Locator};

fn main() {
    let input = Input::from(Locator::Literal("{data: {foo: 99}}.data.foo".to_string()));
    let mut loader = SourceLoader::new(vec![]);
    
    // Load and parse
    loader.load(&input).unwrap();
    
    // Show AST
    let locator = input.locator().clone();
    println!("LALRPOP AST: {:#?}", loader.ast(&locator).unwrap());
    
    // Show core expression after desugaring
    let unit = loader.translate(&input).unwrap();
    println!("LALRPOP Core expression: {:#?}", unit.expr);
}