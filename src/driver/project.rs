//! Locate a project file (Eufile) in the current path
use std::path::PathBuf;

/// Return the path to the nearest Eufile in an ancestor path
pub fn eufile() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok();

    while dir.is_some() {
        if let Some(path) = dir
            .as_ref()
            .unwrap()
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

        if let Some(d) = dir.as_mut() {
            if !d.pop() {
                break;
            }
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
