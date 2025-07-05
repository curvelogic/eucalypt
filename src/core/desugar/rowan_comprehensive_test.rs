//! Comprehensive tests for all core embedding functionality

#[cfg(test)]
mod tests {
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::{Input, Locator};

    #[test]
    fn test_comprehensive_core_embedding() {
        // Test all major core embedding types work together
        let source = r#"
        ` { embedding: :core }
        test_bif: [:c-bif, :__ADD]
        test_name: [:c-name, "hello"]
        test_lit_str: [:c-lit, "world"]
        test_lit_num: [:c-lit, 42]
        test_lit_sym: [:c-lit, :symbol]
        test_list: [:c-list, [:c-lit, 1], [:c-lit, 2], [:c-lit, 3]]
        test_args: [:c-args, [:c-lit, "a"], [:c-lit, "b"]]
        test_soup: [:c-soup, [:c-lit, 1], [:c-name, "add"], [:c-lit, 2]]
        test_meta: [:c-meta, [:c-lit, "value"], [:c-lit, "metadata"]]
        test_block: [:c-block, {x: [:c-lit, 1], y: [:c-lit, 2]}]
        
        # Test error types
        test_unresolved: [:e-unresolved, "unknown"]
        test_redeclaration: [:e-redeclaration, "dup"]
        test_eliminated: [:e-eliminated]
        test_pseudodot: [:e-pseudodot]
        test_pseudocall: [:e-pseudocall]
        test_pseudocat: [:e-pseudocat]
        "#;

        let input = Input::from(Locator::Literal(source.to_string()));
        let mut loader = SourceLoader::new(vec![]);

        // Add a variable to the environment for the c-var test
        // This is handled by the loader's desugaring process

        // Load and parse the source
        let result = loader.load(&input);
        assert!(result.is_ok(), "Should load successfully: {result:?}");

        // Translate to core
        let result = loader.translate(&input);
        if let Err(e) = &result {
            eprintln!("Translation error: {e:?}");
        }
        assert!(result.is_ok(), "Should translate successfully");

        println!("All core embedding types work correctly");
    }

    #[test]
    fn test_complex_core_embeddings() {
        // Test more complex embeddings like c-let, c-lam, c-app
        let source = r#"
        ` { embedding: :core }
        test_app: [:c-app, [:c-name, "func"], [[:c-lit, 1], [:c-lit, 2]]]
        test_lam: [:c-lam, ["x", "y"], [:c-name, "x"]]
        test_lookup: [:c-lookup, [:c-name, "obj"], "key"]
        test_op: [:c-op, :infix-left, 50, [:c-bif, :__ADD]]
        "#;

        let input = Input::from(Locator::Literal(source.to_string()));
        let mut loader = SourceLoader::new(vec![]);

        let result = loader.load(&input);
        assert!(
            result.is_ok(),
            "Should load complex embeddings: {result:?}"
        );

        let result = loader.translate(&input);
        assert!(result.is_ok(), "Should translate complex embeddings");

        println!("Complex core embeddings work correctly");
    }

    #[test]
    fn test_nested_core_embeddings() {
        // Test deeply nested core embeddings
        let source = r#"
        ` { embedding: :core }
        nested: [:c-app, 
                   [:c-lam, ["x"], 
                     [:c-soup, 
                       [:c-var, "x"], 
                       [:c-op, :infix-left, 40, [:c-bif, :__ADD]], 
                       [:c-lit, 1]]], 
                   [[:c-lit, 42]]]
        "#;

        let input = Input::from(Locator::Literal(source.to_string()));
        let mut loader = SourceLoader::new(vec![]);

        let result = loader.load(&input);
        assert!(
            result.is_ok(),
            "Should load nested embeddings: {result:?}"
        );

        let result = loader.translate(&input);
        assert!(result.is_ok(), "Should translate nested embeddings");

        println!("Nested core embeddings work correctly");
    }
}
