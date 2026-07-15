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
    /// In-memory buffer with a filesystem path for source map
    /// registration.  Used by the LSP to feed document content from
    /// the editor buffer without requiring a save to disk.
    Buffer { path: PathBuf, text: String },
    /// A file fetched from a git repository at a specific commit.
    ///
    /// The repository is cloned bare to the local cache on first access;
    /// subsequent imports of the same (url, commit) pair are served from
    /// cache without network access.
    Git {
        url: String,
        commit: String,
        path: String,
    },
}

/// Any filename can be represented as a locator
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
        } else if is_windows_drive_path(url_text) {
            // A Windows absolute path (`C:\...` or `C:/...`) has a drive
            // letter that is *also* a syntactically valid single-letter URL
            // scheme under RFC 3986 (`scheme = ALPHA *( ALPHA / DIGIT / "+"
            // / "-" / "." )`), so `Url::parse` below would happily accept it
            // as `Locator::Url` instead of a filesystem path — and no code
            // path actually reads a `Locator::Url` back (see
            // `SourceLoader::load_source`'s catch-all), so every
            // fully-qualified Windows path would silently fail to load
            // (eu-2sa6.19). None of eucalypt's supported schemes are a
            // single letter (`resource:`/`pseudo:` are handled above as
            // literal prefixes, not via `Url::parse`; `Url::parse` here only
            // ever matters for genuine multi-letter schemes such as `file:`,
            // which `normalise()` converts to `Locator::Fs` anyway), so this
            // check cannot misclassify a real URL.
            Locator::Fs(PathBuf::from(url_text))
        } else {
            match Url::parse(url_text) {
                Ok(u) => Locator::Url(u).normalise(),
                Err(_) => Locator::Fs(PathBuf::from(url_text)),
            }
        }
    }
}

/// True if `s` starts with a Windows drive-letter prefix (`C:\` or `C:/`,
/// case-insensitive): a single ASCII letter, then `:`, then a path
/// separator. This is the shape that RFC 3986 would otherwise parse as a
/// (bogus) single-letter URL scheme — see the call site above.
fn is_windows_drive_path(s: &str) -> bool {
    let bytes = s.as_bytes();
    bytes.len() >= 3
        && bytes[0].is_ascii_alphabetic()
        && bytes[1] == b':'
        && (bytes[2] == b'\\' || bytes[2] == b'/')
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
            Locator::Url(url) => write!(f, "{url}"),
            Locator::Fs(path) => write!(f, "{}", path.to_string_lossy()),
            Locator::Resource(s) => write!(f, "[{s}]"),
            Locator::Pseudo(s) => write!(f, "«{s}»"),
            Locator::StdIn => write!(f, "-"),
            Locator::Cli(text) => write!(f, "'{text}'"),
            Locator::Literal(text) => write!(f, "'''{text}'''"),
            Locator::Buffer { path, .. } => write!(f, "buffer:{}", path.display()),
            Locator::Git { url, commit, path } => {
                write!(f, "git:{url}@{commit}:{path}")
            }
        }
    }
}

/// For storage in SourceLoader we need to provide a path string.
#[allow(clippy::from_over_into)]
impl Into<OsString> for Locator {
    fn into(self) -> OsString {
        match self {
            Locator::Fs(path) => path.into(),
            Locator::Cli(text) => format!("cli:'{text}'").into(),
            Locator::Buffer { path, .. } => path.into(),
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
            "jsonl" => Some(String::from("jsonl")),
            "txt" => Some(String::from("text")),
            "toml" => Some(String::from("toml")),
            "edn" => Some(String::from("edn")),
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
            Locator::Buffer { path, .. } => path
                .extension()
                .and_then(|s| s.to_str())
                .and_then(Self::ext_to_format),
            Locator::Git { path, .. } => Path::new(path)
                .extension()
                .and_then(|s| s.to_str())
                .and_then(Self::ext_to_format)
                .or(Some(String::from("eu"))),
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
            Locator::from("data.jsonl").infer_format(),
            Some(String::from("jsonl"))
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

    /// eu-2sa6.19: a Windows absolute path's drive letter (`C:\...`) is
    /// syntactically a valid single-letter URL scheme under RFC 3986, so
    /// `Locator::from` must recognise and route it to `Locator::Fs` before
    /// attempting `Url::parse` — otherwise every fully-qualified Windows
    /// path is misclassified as `Locator::Url`, which no downstream code
    /// path actually reads (`SourceLoader::load_source`'s catch-all returns
    /// `FileCouldNotBeRead("unsupported locator: ...")`for any `Url` locator
    /// that isn't `resource:`/`file:`).
    ///
    /// Deliberately not `#[cfg(windows)]`: this is pure string parsing with
    /// no filesystem interaction, and the regression must be caught on any
    /// platform's CI, not only a Windows runner.
    #[test]
    pub fn test_windows_drive_letter_path_is_filesystem_locator() {
        assert_eq!(
            Locator::from("C:\\foo\\bar.eu"),
            Locator::Fs(PathBuf::from("C:\\foo\\bar.eu")),
            "uppercase drive letter with backslashes must be Locator::Fs, not Locator::Url"
        );
        assert_eq!(
            Locator::from("c:\\foo\\bar.eu"),
            Locator::Fs(PathBuf::from("c:\\foo\\bar.eu")),
            "lowercase drive letter must also be Locator::Fs"
        );
        assert_eq!(
            Locator::from("C:/foo/bar.eu"),
            Locator::Fs(PathBuf::from("C:/foo/bar.eu")),
            "drive letter with forward slashes must also be Locator::Fs"
        );
    }

    /// Sanity check alongside the drive-letter fix above: genuine URLs
    /// (multi-letter schemes) must still parse as `Locator::Url` (or the
    /// `resource:`/`file:` locators `normalise()` converts them to), not be
    /// accidentally swept into the new drive-letter branch.
    #[test]
    pub fn test_real_urls_still_parse_as_url_locators() {
        assert!(matches!(
            Locator::from("https://example.com/x.eu"),
            Locator::Url(_)
        ));
        assert!(matches!(
            Locator::from("http://example.com/x.eu"),
            Locator::Url(_)
        ));
        assert_eq!(
            Locator::from("resource:prelude"),
            Locator::Resource("prelude".to_string())
        );
        // `file:` URLs normalise to `Locator::Fs`, same destination as the
        // drive-letter fix, via a different (genuine URL scheme) route.
        assert!(matches!(
            Locator::from("file:///foo/bar.eu"),
            Locator::Fs(_)
        ));
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
