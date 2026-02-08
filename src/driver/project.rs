//! Locate a project file (Eufile) in the current path
use std::path::PathBuf;

/// Return the path to the nearest Eufile in an ancestor path
pub fn eufile() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok();

    while let Some(ref mut current) = dir {
        if let Some(path) = current
            .read_dir()
            .ok()?
            .filter_map(|r| r.ok().map(|e| e.path()))
            .find(|p| {
                p.file_name()
                    .and_then(|f| f.to_str())
                    .filter(|n| n.eq_ignore_ascii_case("eufile"))
                    .is_some()
            })
        {
            return Some(path);
        }

        if !current.pop() {
            break;
        }
    }

    None
}

/// Return the path to a .eucalypt in the users home directory if it
/// exists
pub fn dotfile() -> Option<PathBuf> {
    let home = dirs::home_dir()?;

    home.read_dir()
        .ok()?
        .filter_map(|r| r.ok().map(|e| e.path()))
        .find(|p| {
            p.file_name()
                .and_then(|f| f.to_str())
                .filter(|n| n == &".eucalypt")
                .is_some()
        })
}
