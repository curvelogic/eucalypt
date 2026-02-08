//! Provide access to resources baked into the executable.
//!
//! These are named by Locator::Resource locators.
use std::collections::HashMap;

/// A holder for resources included at compile time
pub struct Resources {
    content: HashMap<String, String>,
}

impl Default for Resources {
    fn default() -> Self {
        let mut content = HashMap::new();
        content.insert(
            "prelude".to_string(),
            String::from_utf8(include_bytes!("../../lib/prelude.eu").to_vec())
                .expect("prelude.eu is valid UTF-8"),
        );
        content.insert(
            "test".to_string(),
            String::from_utf8(include_bytes!("../../lib/test.eu").to_vec())
                .expect("test.eu is valid UTF-8"),
        );
        content.insert(
            "package".to_string(),
            String::from_utf8(include_bytes!("../../Cargo.lock").to_vec())
                .expect("Cargo.lock is valid UTF-8"),
        );
        content.insert(
            "build-meta".to_string(),
            String::from_utf8(include_bytes!("../../build-meta.yaml").to_vec())
                .expect("build-meta.yaml is valid UTF-8"),
        );

        Resources { content }
    }
}

impl Resources {
    /// Retrieve the resource identified by `name`, None if it doesn't
    /// exist.
    pub fn get<S: AsRef<str>>(&self, name: S) -> Option<&String> {
        self.content.get(name.as_ref())
    }
}
