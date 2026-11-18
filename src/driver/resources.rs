//! Provide access to resources baked into the executable.
//!
//! These are named by Locator::Resource locators.
use std::collections::HashMap;
use std::iter;

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

    /// The baked-in library a filesystem import path names, if it names
    /// one: the path must be exactly a library's canonical file name — a
    /// single component, with the `.eu` extension or none, whose stem is
    /// the prelude or an embedded eucalypt library (`lens.eu`, `test.eu`,
    /// or `prelude` as `{ import: "[prelude]" }` spells it).
    ///
    /// This is the only spelling that may fall back to the shipped library
    /// when no such file exists on disk (eu-7tlbg). Any other path that
    /// merely shares a library's stem — `lib/test.eu`, `./lens.eu`,
    /// `test.yaml` — means the user's own file, and a missing one
    /// is an error rather than a silent substitution. The data resources
    /// (`package`, `build-meta`) are reachable only as `resource:` locators.
    pub fn library_for_import_path(path: &std::path::Path) -> Option<&'static str> {
        use std::path::Component;
        let mut components = path.components();
        let (Some(Component::Normal(file)), None) = (components.next(), components.next()) else {
            return None;
        };
        let file = std::path::Path::new(file);
        if file.extension().is_some_and(|ext| ext != "eu") {
            return None;
        }
        let stem = file.file_stem()?.to_str()?;
        iter::once("prelude")
            .chain(Self::eu_library_names())
            .find(|name| *name == stem)
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// eu-7tlbg: only a shipped library's bare canonical name (with `.eu`
    /// or no extension) names it; a path that merely shares its stem does
    /// not. Pure path classification, independent of the filesystem.
    #[test]
    fn test_library_for_import_path_accepts_only_canonical_names() {
        for (path, library) in [
            ("lens.eu", "lens"),
            ("test.eu", "test"),
            ("prelude.eu", "prelude"),
            ("prelude", "prelude"),
            ("markup", "markup"),
        ] {
            assert_eq!(
                Resources::library_for_import_path(Path::new(path)),
                Some(library),
                "{path}"
            );
        }
        for path in [
            "sub/test.eu",
            "./lens.eu",
            "/abs/state.eu",
            "../lens.eu",
            "test.yaml",
            "lens.txt",
            "sub/lens",
            "package.eu",
            "build-meta.eu",
            "helpers.eu",
            "",
        ] {
            assert_eq!(
                Resources::library_for_import_path(Path::new(path)),
                None,
                "{path}"
            );
        }
    }
}
