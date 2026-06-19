//! Git import caching.
//!
//! On first access a repository is cloned bare to
//! `~/.eu/cache/git/<url-hash>/bare/`. Individual files are then extracted
//! with `git show <commit>:<path>` and written to
//! `~/.eu/cache/git/<url-hash>/<commit>/<path>`.
//!
//! Subsequent imports of the same (url, commit, path) triple are served
//! directly from the cache without any network access.

use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;
use std::process::Command;

use super::error::SourceError;

/// Compute a stable hex directory name from a URL string.
fn url_hash(url: &str) -> String {
    let mut hasher = DefaultHasher::new();
    url.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

/// Return the base cache directory: `~/.eu/cache/git/`.
fn git_cache_base() -> Result<PathBuf, SourceError> {
    let home = dirs::home_dir().ok_or_else(|| {
        SourceError::InvalidSource(
            "cannot determine home directory for git cache".to_string(),
            0,
        )
    })?;
    Ok(home.join(".eu").join("cache").join("git"))
}

/// Return the expected on-disk path for a cached git file.
///
/// This function is pure — it does not touch the filesystem or network.
/// Use it to compute the cache path when the file is known to exist already
/// (e.g. in `find_source_dir`).
pub fn cached_file_path(url: &str, commit: &str, path: &str) -> Result<PathBuf, SourceError> {
    Ok(git_cache_base()?
        .join(url_hash(url))
        .join(commit)
        .join(path))
}

/// Ensure the git import is cached and return the path to the local file.
///
/// 1. Clones the repository bare if not already present.
/// 2. Extracts the requested file with `git show <commit>:<path>`.
/// 3. Writes the extracted content to the cache and returns the path.
///
/// If the file is already cached, returns immediately without any git
/// or network operations.
pub fn resolve_git_import(url: &str, commit: &str, path: &str) -> Result<PathBuf, SourceError> {
    let target_file = cached_file_path(url, commit, path)?;

    // Fast path: already cached.
    if target_file.exists() {
        return Ok(target_file);
    }

    let bare_dir = git_cache_base()?.join(url_hash(url)).join("bare");

    // Clone the bare repository if needed.
    if !bare_dir.exists() {
        std::fs::create_dir_all(&bare_dir).map_err(|e| {
            SourceError::InvalidSource(format!("cannot create git cache directory: {e}"), 0)
        })?;

        let output = Command::new("git")
            .args(["clone", "--bare", url, "."])
            .current_dir(&bare_dir)
            .output()
            .map_err(|e| {
                SourceError::InvalidSource(format!("failed to run git clone for {url}: {e}"), 0)
            })?;

        if !output.status.success() {
            // Remove the incomplete bare directory so a retry can start fresh.
            std::fs::remove_dir_all(&bare_dir).ok();
            return Err(SourceError::InvalidSource(
                format!(
                    "git clone failed for {url}: {}",
                    String::from_utf8_lossy(&output.stderr).trim()
                ),
                0,
            ));
        }
    }

    // Extract the file content from the specified commit.
    let ref_arg = format!("{commit}:{path}");
    let output = Command::new("git")
        .args(["show", &ref_arg])
        .current_dir(&bare_dir)
        .output()
        .map_err(|e| SourceError::InvalidSource(format!("failed to run git show: {e}"), 0))?;

    if !output.status.success() {
        return Err(SourceError::InvalidSource(
            format!(
                "path {path:?} not found in {url} at commit {commit}: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            ),
            0,
        ));
    }

    // Write the extracted content to the cache.
    if let Some(parent) = target_file.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            SourceError::InvalidSource(format!("cannot create cache directory for {path}: {e}"), 0)
        })?;
    }

    std::fs::write(&target_file, &output.stdout).map_err(|e| {
        SourceError::InvalidSource(format!("cannot write cached file for {path}: {e}"), 0)
    })?;

    Ok(target_file)
}
