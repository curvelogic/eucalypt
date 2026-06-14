//! Provide access to resources baked into the executable.
//!
//! These are named by Locator::Resource locators.
use std::collections::HashMap;

/// Raw bytes of the pre-compiled prelude blob, embedded at compile time.
///
/// Only present when `build.rs` set `cfg(prelude_blob_ok)` — i.e. when
/// `lib/prelude.blob` exists and its hash matches `lib/prelude.eu`.
#[cfg(prelude_blob_ok)]
pub static PRELUDE_BLOB_BYTES: &[u8] = include_bytes!("../../lib/prelude.blob");

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
            "lens".to_string(),
            String::from_utf8(include_bytes!("../../lib/lens.eu").to_vec())
                .expect("lens.eu is valid UTF-8"),
        );
        content.insert(
            "state".to_string(),
            String::from_utf8(include_bytes!("../../lib/state.eu").to_vec())
                .expect("state.eu is valid UTF-8"),
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
