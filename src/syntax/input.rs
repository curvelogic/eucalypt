//! A input or import descriptor, this may be a file a URL or another
//! source of source code.
use crate::syntax::error::SyntaxError;
use std::ffi::OsString;
use std::fmt::*;
use std::path::{Path, PathBuf};
use std::result::Result;
use std::str::FromStr;
use url::Url;

/// A `Locator` defines the various sources from which source code can be
/// read.
///
/// These include URLs, files, internal resources, stdin and command
/// line argument (`-e`).
#[derive(PartialEq, Debug, Clone, Eq, Hash, PartialOrd, Ord)]
pub enum Locator {
    /// Input from a URL (may be file:)
    Url(Url),
    /// Filesystem path (URLs are bad for relative file paths)
    Fs(PathBuf),
    /// Input comes from embedded resource
    Resource(String),
    /// Input comes from a pseudoblock (e.g. __io for env data)
    Pseudo(String),
    /// Input from stdin
    StdIn,
    /// Input from a CLI arg with literal text
    Cli(String),
    /// Literal for testing purposes
    Literal(String),
}

/// Any filename can be represented as a locator
///
/// TODO: fix panics on non UTF-8 file name
impl From<&str> for Locator {
    fn from(s: &str) -> Locator {
        // Chomp away surrounding square brackets
        let mut url_text: &str = s;
        if url_text.starts_with('[') {
            url_text = &url_text[1..(url_text.len() - 1)];
        }

        if url_text == "-" {
            Locator::StdIn
        } else if let Some(stripped) = url_text.strip_prefix("resource:") {
            Locator::Resource(stripped.trim().to_string())
        } else if let Some(stripped) = url_text.strip_prefix("pseudo:") {
            Locator::Pseudo(stripped.trim().to_string())
        } else {
            match Url::parse(url_text) {
                Ok(u) => Locator::Url(u).normalise(),
                Err(_) => Locator::Fs(PathBuf::from(url_text)),
            }
        }
    }
}

/// Convert to Locator from Path or PathBuf
impl From<&Path> for Locator {
    fn from(p: &Path) -> Self {
        Locator::Fs(p.to_path_buf())
    }
}

impl Display for Locator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Locator::Url(url) => write!(f, "{}", url),
            Locator::Fs(path) => write!(f, "{}", path.to_str().expect("Bad File")),
            Locator::Resource(s) => write!(f, "[{}]", s),
            Locator::Pseudo(s) => write!(f, "«{}»", s),
            Locator::StdIn => write!(f, "-"),
            Locator::Cli(text) => write!(f, "'{}'", text),
            Locator::Literal(text) => write!(f, "'''{}'''", text),
        }
    }
}

/// For storage in SourceLoader we need to provide a path string.
#[allow(clippy::from_over_into)]
impl Into<OsString> for Locator {
    fn into(self) -> OsString {
        match self {
            Locator::Fs(path) => path.into(),
            Locator::Cli(text) => format!("cli:'{}'", text).into(),
            _ => todo!(),
        }
    }
}

impl Locator {
    /// Ensure resource: urls are represented correctly
    pub fn normalise(&self) -> Locator {
        if let Locator::Url(u) = self {
            if u.scheme() == "resource" {
                return Locator::Resource(String::from(u.path()));
            } else if u.scheme() == "file" {
                return Locator::Fs(PathBuf::from(u.path()));
            }
        }
        self.clone()
    }

    fn ext_to_format(ext: &str) -> Option<String> {
        match ext {
            "json" => Some(String::from("json")),
            "txt" => Some(String::from("text")),
            "toml" => Some(String::from("toml")),
            "yaml" => Some(String::from("yaml")),
            "yml" => Some(String::from("yaml")),
            "csv" => Some(String::from("csv")),
            "eu" => Some(String::from("eu")),
            _ => None,
        }
    }

    /// Attempt to infer a format from the locator itself
    pub fn infer_format(&self) -> Option<String> {
        match self {
            Locator::Url(u) => Path::new(u.path())
                .extension()
                .and_then(|s| s.to_str())
                .and_then(Self::ext_to_format),
            Locator::Fs(pb) => pb
                .extension()
                .and_then(|s| s.to_str())
                .and_then(Self::ext_to_format),
            Locator::StdIn => Some(String::from("json")),
            Locator::Resource(_) => Some(String::from("eu")),
            Locator::Pseudo(_) => Some(String::from("core")),
            _ => Some(String::from("eu")),
        }
    }
}

/// A locator together with an optional name to assign it and a format
/// with which to interpret it
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct Input {
    /// Locator for the input source
    locator: Locator,
    /// Name to assign
    name: Option<String>,
    /// Format of input
    format: String,
}

impl Display for Input {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self.name {
            Some(ref name) => write!(f, "{}={}@{}", name, self.format, self.locator),
            None => write!(f, "{}@{}", self.format, self.locator),
        }
    }
}

impl Input {
    pub fn new<T: AsRef<str>>(locator: Locator, name: Option<String>, format: T) -> Self {
        Input {
            locator,
            name,
            format: format.as_ref().to_string(),
        }
    }

    pub fn locator(&self) -> &Locator {
        &self.locator
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn format(&self) -> &str {
        &self.format
    }

    pub fn with_name(self, name: &str) -> Self {
        Input {
            locator: self.locator,
            name: Some(name.to_string()),
            format: self.format,
        }
    }
}

impl From<Locator> for Input {
    fn from(locator: Locator) -> Self {
        let format = locator.infer_format().unwrap_or_else(|| "eu".to_string());
        Input {
            locator,
            name: None,
            format,
        }
    }
}

impl FromStr for Input {
    type Err = SyntaxError;

    /// Parse an `Input` from its string representation
    fn from_str(s: &str) -> Result<Input, Self::Err> {
        let mut remainder = s;

        let mut name: Option<String> = None;
        if let Some(i) = remainder.find('=') {
            let (n, rem) = remainder.split_at(i);
            name = Some(String::from(n.trim()));
            remainder = &rem[1..];
        }

        let mut fmt: Option<String> = None;
        if let Some(i) = remainder.find('@') {
            let (f, rem) = remainder.split_at(i);
            fmt = Some(String::from(f.trim()));
            remainder = &rem[1..];
        }

        let locator = Locator::from(remainder.trim());
        let format = fmt
            .or_else(|| locator.infer_format())
            .unwrap_or_else(|| String::from("eu"));

        Ok(Input {
            locator,
            name,
            format,
        })
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn test_format_inference() {
        assert_eq!(
            Locator::from("data.json").infer_format(),
            Some(String::from("json"))
        );
        assert_eq!(
            Locator::from("data.toml").infer_format(),
            Some(String::from("toml"))
        );
        assert_eq!(
            Locator::from("data.csv").infer_format(),
            Some(String::from("csv"))
        );
    }

    #[test]
    pub fn test_parse_locators() {
        assert_eq!(
            Locator::from("../dir/test.eu"),
            Locator::Fs(PathBuf::from("../dir/test.eu"))
        );
    }

    #[test]
    pub fn test_parse_inputs() {
        assert_eq!(
            Input::from_str("simple.eu"),
            Ok(Input {
                locator: Locator::from("simple.eu"),
                name: None,
                format: String::from("eu")
            })
        );
        assert_eq!(
            Input::from_str("simple.json"),
            Ok(Input {
                locator: Locator::from("simple.json"),
                name: None,
                format: String::from("json")
            })
        );
        assert_eq!(
            Input::from_str("json@simple.txt"),
            Ok(Input {
                locator: Locator::from("simple.txt"),
                name: None,
                format: String::from("json")
            })
        );
        assert_eq!(
            Input::from_str("data=yaml@data.txt"),
            Ok(Input {
                locator: Locator::from("data.txt"),
                name: Some(String::from("data")),
                format: String::from("yaml")
            })
        );
        assert_eq!(
            Input::from_str("test.yaml"),
            Ok(Input {
                locator: Locator::from("test.yaml"),
                name: None,
                format: String::from("yaml")
            })
        );
        assert_eq!(
            Input::from_str("https://blah.com/blah.eu"),
            Ok(Input {
                locator: Locator::from("https://blah.com/blah.eu"),
                name: None,
                format: String::from("eu")
            })
        );
        assert_eq!(
            Input::from_str("k=yaml@https://blah.com/blah.txt"),
            Ok(Input {
                locator: Locator::from("https://blah.com/blah.txt"),
                name: Some(String::from("k")),
                format: String::from("yaml")
            })
        );
        assert_eq!(
            Input::from_str("eu@resource:prelude"),
            Ok(Input {
                locator: Locator::Resource(String::from("prelude")),
                name: None,
                format: String::from("eu")
            })
        );
        assert_eq!(
            Input::from_str("__io=eu@pseudo:io"),
            Ok(Input {
                locator: Locator::Pseudo(String::from("io")),
                name: Some(String::from("__io")),
                format: String::from("eu")
            })
        );
        assert_eq!(
            Input::from_str("eu@[resource:prelude]"),
            Ok(Input {
                locator: Locator::Resource(String::from("prelude")),
                name: None,
                format: String::from("eu")
            })
        );
    }
}
