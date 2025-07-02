//! Tests for dotted lookup parsing and evaluation

#[cfg(test)]
mod tests {
    use crate::driver::source::SourceLoader;
    use crate::driver::options::EucalyptOptions;
    use crate::driver::prepare;
    use crate::driver::eval;
    use crate::driver::statistics::Timings;
    use crate::syntax::input::{Input, Locator};

    fn eval_expr(expr: &str) -> Result<String, Box<dyn std::error::Error>> {
        let input = Input::from(Locator::Literal(expr.to_string()));
        let opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![input.clone()])
            .with_export_type("text".to_string())
            .without_prelude()
            .build();
        
        let mut loader = SourceLoader::new(vec![]);
        let mut timings = Timings::default();
        prepare::prepare(&opts, &mut loader, &mut timings)?;
        
        let mut outbuf = Vec::new();
        let mut errbuf = Vec::new();
        
        {
            let out = Box::new(&mut outbuf);
            let err = Box::new(&mut errbuf);
            let mut executor = eval::Executor::from(loader);
            let mut stats = crate::driver::statistics::Statistics::default();
            executor.capture_output(out, err);
            executor.execute(&opts, &mut stats, "text".to_string())?;
        }
        
        let stderr_output = std::str::from_utf8(&errbuf)?;
        if !stderr_output.is_empty() {
            eprintln!("STDERR: {}", stderr_output);
        }
        Ok(std::str::from_utf8(&outbuf)?.trim().to_string())
    }

    #[test]
    fn test_basic_evaluation() {
        let result = eval_expr("42").unwrap();
        assert_eq!(result, "42");
    }

    #[test]
    fn test_simple_dotted_lookup() {
        let result = eval_expr("{foo: 99}.foo");
        match result {
            Ok(output) => {
                eprintln!("Got output: '{}'", output);
                assert_eq!(output, "99");
            }
            Err(e) => {
                panic!("Test failed with error: {}", e);
            }
        }
    }

    #[test]
    fn test_simple_dotted_lookup_parsing() {
        use crate::syntax::rowan::parse_expr;
        use rowan::ast::AstNode;
        use crate::syntax::rowan::ast::AstToken;
        
        let text = "{foo: 99}.foo";
        let parsed = parse_expr(text);
        assert!(parsed.errors().is_empty(), "Parse should succeed");
        
        let soup = parsed.tree();
        // Verify the AST structure is correct
        let syntax = soup.syntax();
        assert_eq!(syntax.kind(), crate::syntax::rowan::kind::SyntaxKind::SOUP);
        
        // Should have 3 elements: block, dot operator, name
        let elements: Vec<_> = soup.elements().collect();
        assert_eq!(elements.len(), 3);
        
        // First element should be a block
        assert!(matches!(elements[0], crate::syntax::rowan::ast::Element::Block(_)));
        
        // Second element should be a name with dot operator
        if let crate::syntax::rowan::ast::Element::Name(name) = &elements[1] {
            if let Some(id) = name.identifier() {
                if let crate::syntax::rowan::ast::Identifier::OperatorIdentifier(op) = id {
                    assert_eq!(op.text(), ".");
                }
            }
        }
        
        // Third element should be a name "foo"
        if let crate::syntax::rowan::ast::Element::Name(name) = &elements[2] {
            if let Some(id) = name.identifier() {
                if let crate::syntax::rowan::ast::Identifier::NormalIdentifier(normal) = id {
                    let full_id = crate::syntax::rowan::ast::Identifier::NormalIdentifier(normal.clone());
                    assert_eq!(full_id.name().unwrap_or(""), "foo");
                }
            }
        }
    }

    #[test]
    fn test_two_level_dotted_lookup() {
        let result = eval_expr("{data: {foo: 99}}.data.foo").unwrap();
        assert_eq!(result, "99");
    }

    #[test]
    fn test_two_level_dotted_lookup_parsing() {
        use crate::driver::source::SourceLoader;
        use crate::syntax::input::{Input, Locator};
        
        let input = Input::from(Locator::Literal("{data: {foo: 99}}.data.foo".to_string()));
        let mut loader = SourceLoader::new(vec![]);
        loader.load(&input).unwrap();
        
        // Check AST before desugaring  
        let locator = input.locator().clone();
        eprintln!("AST before desugaring: {:#?}", loader.ast(&locator).unwrap());
        
        let unit = loader.translate(&input).unwrap();
        eprintln!("Core expression: {:#?}", unit.expr);
    }

    #[test]
    fn test_three_level_dotted_lookup() {
        let result = eval_expr("{data: {foo: {bar: 99}}}.data.foo.bar").unwrap();
        assert_eq!(result, "99");
    }

    #[test]
    fn test_dotted_lookup_with_strings() {
        let result = eval_expr(r#"{data: {name: "world"}}.data.name"#).unwrap();
        assert_eq!(result, "world");
    }

    #[test]
    fn test_dotted_lookup_in_string_interpolation() {
        let result = eval_expr(r#"{data: {foo: {bar: 99}}}."{data.foo.bar}""#).unwrap();
        assert_eq!(result, "99");
    }

    #[test]
    fn test_dotted_lookup_with_multiple_fields() {
        let expr = r#"{
            user: {
                name: "Alice",
                address: {
                    city: "London",
                    country: "UK"
                }
            }
        }.user.address.city"#;
        let result = eval_expr(expr).unwrap();
        assert_eq!(result, "London");
    }

    #[test]
    fn test_dotted_lookup_in_assertion() {
        let expr = r#"{data: {foo: {bar: 99}}}. "{data.foo.bar}" //= "99""#;
        let result = eval_expr(expr).unwrap();
        assert_eq!(result, "true");
    }

    #[test]
    fn test_dotted_lookup_with_function_call() {
        let expr = r#"{get_data: {value: 42}}.get_data.value"#;
        let result = eval_expr(expr).unwrap();
        assert_eq!(result, "42");
    }

    #[test]
    fn test_static_lookup_syntax() {
        // The static lookup syntax {block}.(expr) should work
        let expr = r#"{a: 1, b: 2}.(a + b)"#;
        let result = eval_expr(expr).unwrap();
        assert_eq!(result, "3");
    }

    #[test]
    fn test_chained_static_lookups() {
        // Multiple static lookups should work
        let expr = r#"{data: {a: 1, b: 2}}.data.(a + b)"#;
        let result = eval_expr(expr).unwrap();
        assert_eq!(result, "3");
    }
}