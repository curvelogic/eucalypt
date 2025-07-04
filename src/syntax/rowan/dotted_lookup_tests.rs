//! Tests for dotted lookup parsing and evaluation

#[cfg(test)]
mod tests {
    use crate::driver::eval;
    use crate::driver::options::EucalyptOptions;
    use crate::driver::prepare;
    use crate::driver::source::SourceLoader;
    use crate::driver::statistics::Timings;
    use crate::syntax::input::{Input, Locator};

    fn eval_expr(expr: &str) -> Result<String, Box<dyn std::error::Error>> {
        let input = Input::from(Locator::Cli(expr.to_string()));
        let opts = EucalyptOptions::default()
            .with_explicit_inputs(vec![input.clone()])
            .with_export_type("text".to_string())
            .build();

        let mut loader = SourceLoader::new(vec![]);
        let mut timings = Timings::default();
        println!("About to prepare with expression: {}", expr);
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
        use crate::syntax::rowan::ast::AstToken;
        use crate::syntax::rowan::parse_expr;
        use rowan::ast::AstNode;

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
        assert!(matches!(
            elements[0],
            crate::syntax::rowan::ast::Element::Block(_)
        ));

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
                    let full_id =
                        crate::syntax::rowan::ast::Identifier::NormalIdentifier(normal.clone());
                    assert_eq!(full_id.name().unwrap_or(""), "foo");
                }
            }
        }
    }

    #[test]
    fn test_two_level_dotted_lookup() {
        println!("Starting test_two_level_dotted_lookup");
        let result = eval_expr("{data: {foo: 99}}.data.foo");
        match result {
            Ok(output) => {
                println!("Got output: '{}'", output);
                assert_eq!(output, "99");
            }
            Err(e) => {
                println!("Test failed with error: {}", e);
                panic!("Test failed: {}", e);
            }
        }
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
        eprintln!(
            "AST before desugaring: {:#?}",
            loader.ast(&locator).unwrap()
        );

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
    fn test_string_interpolation_complex() {
        // Test complex multi-level dotted lookup in string interpolation
        let result = eval_expr(r#"{data: {foo: {bar: 99}}}."{data.foo.bar}""#).unwrap();
        assert_eq!(result, "99");
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
        use crate::core::export::embed::quote_embed_core_unit;
        use crate::driver::source::SourceLoader;
        use crate::syntax::input::{Input, Locator};

        // The static lookup syntax {block}.(expr) should work
        let expr = r#"{a: 1, b: 2}.(a + b)"#;
        println!("\n=== Debug generalized lookup: {} ===", expr);

        // First check what core expression we get
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(expr.to_string());
        let input_obj = Input::from(loc);
        loader.load(&input_obj).unwrap();
        let unit = loader.translate(&input_obj).unwrap();

        println!("After desugaring:");
        println!("{}", quote_embed_core_unit(&unit.expr));

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

    #[test]
    fn debug_simple_lookup_failure() {
        use crate::core::export::embed::quote_embed_core_unit;
        use crate::driver::source::SourceLoader;
        use crate::syntax::input::{Input, Locator};

        let input = "{foo: 99}.foo";
        println!("\n=== Debug Simple Lookup: {} ===", input);

        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(input.to_string());
        let input_obj = Input::from(loc);
        loader.load(&input_obj).unwrap();

        // Check the AST structure
        if let Some(ast) = loader.ast(input_obj.locator()) {
            println!("\n--- AST Structure ---");
            println!("{:#?}", ast);
        }

        let unit = loader.translate(&input_obj).unwrap();

        println!("\n--- After desugaring ---");
        println!("{}", quote_embed_core_unit(&unit.expr));

        // Run through the pipeline
        loader.merge_units(&[input_obj]).unwrap();
        loader.cook().unwrap();
        println!("\n--- After cooking ---");
        println!("{}", quote_embed_core_unit(&loader.core().expr));

        // Try to run inline
        match loader.inline() {
            Ok(_) => println!("\n--- After inline: OK ---"),
            Err(e) => println!("\n--- After inline: ERROR: {} ---", e),
        }

        // Try to run eliminate
        match loader.eliminate() {
            Ok(_) => println!("\n--- After eliminate: OK ---"),
            Err(e) => println!("\n--- After eliminate: ERROR: {} ---", e),
        }

        // Check the final core
        println!("\n--- Final core ---");
        println!("{}", quote_embed_core_unit(&loader.core().expr));
    }

    #[test]
    fn debug_rowan_stg_generation() {
        use crate::driver::source::SourceLoader;
        use crate::eval::stg::{compiler::Compiler, RenderType};
        use crate::syntax::input::{Input, Locator};

        let input = "{data: {foo: 99}}.data.foo";
        println!("\n=== STG Generation for: {} ===", input);

        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(input.to_string());
        let input_obj = Input::from(loc);
        loader.load(&input_obj).unwrap();
        loader.translate(&input_obj).unwrap();
        loader.merge_units(&[input_obj]).unwrap();
        loader.cook().unwrap();

        // Try each pipeline step
        println!("\n--- After inline ---");
        match loader.inline() {
            Ok(_) => println!("OK"),
            Err(e) => println!("ERROR: {}", e),
        }

        println!("\n--- After eliminate ---");
        match loader.eliminate() {
            Ok(_) => {
                println!("OK");

                // Now try to compile to STG
                let core_expr = loader.core().expr.clone();
                let compiler =
                    Compiler::new(false, RenderType::Headless, false, false, false, vec![]);

                println!("\n--- STG Compilation ---");
                match compiler.compile(core_expr) {
                    Ok(stg) => {
                        println!("STG generated successfully!");
                        println!("{:#?}", stg);
                    }
                    Err(e) => {
                        println!("STG compilation failed: {}", e);
                    }
                }
            }
            Err(e) => println!("ERROR: {}", e),
        }
    }

    #[test]
    fn debug_rowan_core_expression() {
        use crate::core::export::embed::quote_embed_core_unit;
        use crate::driver::source::SourceLoader;
        use crate::syntax::input::{Input, Locator};

        let input = "{data: {foo: 99}}.data.foo";
        println!("\n=== ROWAN Core Expression for: {} ===", input);

        let mut loader = SourceLoader::default();
        let loc = Locator::Cli(input.to_string());
        let input_obj = Input::from(loc);
        loader.load(&input_obj).unwrap();
        let unit = loader.translate(&input_obj).unwrap();

        println!("\n--- After desugaring ---");
        println!("{}", quote_embed_core_unit(&unit.expr));

        // Also run cooking to see what happens
        loader.merge_units(&[input_obj]).unwrap();
        loader.cook().unwrap();
        let cooked = loader.core();

        println!("\n--- After cooking ---");
        println!("{}", quote_embed_core_unit(&cooked.expr));
        println!("=== End Core Expression ===\n");
    }

    #[test]
    fn test_interpolation_expression_capabilities() {
        println!("\n=== Testing String Interpolation Expression Support ===");

        // Test simple variable - should work
        println!("\n--- Simple variable ---");
        let result = eval_expr(r#"{x: 42}."{x}""#);
        println!("Result: {:?}", result.is_ok());
        assert!(result.is_ok());

        // Test dotted lookup - should work
        println!("\n--- Dotted lookup ---");
        let result = eval_expr(r#"{data: {foo: 99}}."{data.foo}""#);
        println!("Result: {:?}", result.is_ok());
        assert!(result.is_ok());

        // Test numeric positional (should fail - no such context)
        println!("\n--- Numeric positional ---");
        let result = eval_expr(r#"test."{0}""#);
        println!("Result: {:?}", result.is_ok());
        // This should fail because 'test' is undefined

        // Test what lexer produces for complex expressions
        use crate::syntax::rowan::string_lex::StringPatternLexer;

        println!("\n--- What gets lexed for '{{a + b}}' ---");
        let lexer = StringPatternLexer::new("{a + b}", codespan::ByteOffset(0));
        let tokens: Vec<_> = lexer.collect();
        println!("Tokens: {:?}", tokens);

        println!("\n--- What gets lexed for '{{a.b.c}}' ---");
        let lexer = StringPatternLexer::new("{a.b.c}", codespan::ByteOffset(0));
        let tokens: Vec<_> = lexer.collect();
        println!("Tokens: {:?}", tokens);

        println!("\n--- What gets lexed for '{{func()}}' ---");
        let lexer = StringPatternLexer::new("{func()}", codespan::ByteOffset(0));
        let tokens: Vec<_> = lexer.collect();
        println!("Tokens: {:?}", tokens);

        println!("\n--- What gets lexed for '{{0}}' ---");
        let lexer = StringPatternLexer::new("{0}", codespan::ByteOffset(0));
        let tokens: Vec<_> = lexer.collect();
        println!("Tokens: {:?}", tokens);
    }
}
