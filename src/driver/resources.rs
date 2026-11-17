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

/// The eucalypt libraries embedded as resources, by name. A program may
/// import one by file name (`{ import: "lens.eu" }`) or as a resource
/// (`resource:lens`). This one table both stores them in [`Resources`]
/// and says which resources are importable libraries, so the CLI and the
/// WASM engine cannot disagree about them.
const EU_LIBRARIES: &[(&str, &[u8])] = &[
    ("test", include_bytes!("../../lib/test.eu")),
    ("lens", include_bytes!("../../lib/lens.eu")),
    ("state", include_bytes!("../../lib/state.eu")),
    ("do", include_bytes!("../../lib/do.eu")),
    ("reflect", include_bytes!("../../lib/reflect.eu")),
    ("contract", include_bytes!("../../lib/contract.eu")),
    ("markup", include_bytes!("../../lib/markup.eu")),
];

impl Default for Resources {
    fn default() -> Self {
        let mut content = HashMap::new();
        content.insert(
            "prelude".to_string(),
            String::from_utf8(include_bytes!("../../lib/prelude.eu").to_vec())
                .expect("prelude.eu is valid UTF-8"),
        );
        for (name, bytes) in EU_LIBRARIES {
            content.insert(
                name.to_string(),
                String::from_utf8(bytes.to_vec()).expect("embedded libraries are valid UTF-8"),
            );
        }
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
    /// The names of the embedded eucalypt libraries a program may import
    /// (`lens`, `state`, ...): not the prelude, nor the data resources.
    pub fn eu_library_names() -> impl Iterator<Item = &'static str> {
        EU_LIBRARIES.iter().map(|(name, _)| *name)
    }

    /// The embedded eucalypt library called `name` and its source, None if
    /// `name` is not one.
    pub fn eu_library(&self, name: &str) -> Option<(&'static str, &String)> {
        let name = Self::eu_library_names().find(|l| *l == name)?;
        Some((name, self.content.get(name)?))
    }

    /// Retrieve the resource identified by `name`, None if it doesn't
    /// exist.
    pub fn get<S: AsRef<str>>(&self, name: S) -> Option<&String> {
        self.content.get(name.as_ref())
    }

    /// Replace the resource `name`, so that a test can load an altered
    /// copy of an embedded library (the prelude, say) through the same
    /// path as the real one.
    #[cfg(test)]
    pub(crate) fn insert<S: Into<String>>(&mut self, name: S, text: String) {
        self.content.insert(name.into(), text);
    }
}
