//! Integration test for Rowan core embedding functionality

#[cfg(test)]
mod tests {
    use crate::driver::source::SourceLoader;
    use crate::syntax::input::{Input, Locator};

    #[test]
    fn test_core_embedding_integration() {
        // Test a simple core embedding with the full pipeline
        let source = r#"
        ` { embedding: :core }
        test: [:c-name, "hello"]
        "#;

        let input = Input::from(Locator::Literal(source.to_string()));
        let mut loader = SourceLoader::new(vec![]);

        // Load and parse the source
        let result = loader.load(&input);
        assert!(result.is_ok(), "Should load successfully: {result:?}");

        // Translate to core
        let result = loader.translate(&input);
        assert!(result.is_ok(), "Should translate successfully");

        println!("Core embedding integration test passed");
    }

    #[test]
    fn test_basic_core_name_embedding() {
        // Test a very simple [:c-name, "test"] embedding
        let source = r#"
        ` { embedding: :core }
        simple: [:c-name, "test"]
        "#;

        let input = Input::from(Locator::Literal(source.to_string()));
        let mut loader = SourceLoader::new(vec![]);

        let result = loader.load(&input);
        assert!(result.is_ok(), "Should load: {result:?}");

        let result = loader.translate(&input);
        assert!(result.is_ok(), "Should translate");

        println!("Core embedding works correctly");
    }
}
