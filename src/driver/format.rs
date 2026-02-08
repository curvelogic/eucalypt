//! Format command handler for `eu fmt`

use crate::driver::options::EucalyptOptions;
use crate::syntax::export::format::{format_source, needs_formatting, FormatterConfig};
use std::fs;
use std::io::{self, Read};
use std::path::Path;

/// Run the format command
pub fn format(opt: &EucalyptOptions) -> Result<i32, String> {
    let config = FormatterConfig::new(
        opt.format_width(),
        opt.format_indent(),
        opt.format_reformat(),
    );

    let files: Vec<_> = opt.explicit_inputs().iter().collect();

    if files.is_empty() {
        // Read from stdin
        return format_stdin(&config, opt);
    }

    let mut exit_code = 0;

    for input in files {
        let path_str = input.locator().to_string();

        // Handle stdin marker
        if path_str == "-" {
            if let Err(e) = format_stdin(&config, opt) {
                eprintln!("Error formatting stdin: {}", e);
                exit_code = 1;
            }
            continue;
        }

        let path = Path::new(&path_str);
        if !path.exists() {
            eprintln!("File not found: {}", path_str);
            exit_code = 1;
            continue;
        }

        match format_file(path, &config, opt) {
            Ok(file_result) => {
                if file_result != 0 {
                    exit_code = file_result;
                }
            }
            Err(e) => {
                eprintln!("Error formatting {}: {}", path_str, e);
                exit_code = 1;
            }
        }
    }

    Ok(exit_code)
}

/// Format a single file
fn format_file(
    path: &Path,
    config: &FormatterConfig,
    opt: &EucalyptOptions,
) -> Result<i32, String> {
    let source = fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;

    if opt.format_check() {
        // Check mode: exit with 1 if file would change
        let needs_fmt = needs_formatting(&source, config)?;
        if needs_fmt {
            eprintln!("Would reformat: {}", path.display());
            return Ok(1);
        }
        return Ok(0);
    }

    let formatted = format_source(&source, config)?;

    if opt.format_write() {
        // Write back to file if changed
        if formatted != source {
            fs::write(path, &formatted).map_err(|e| format!("Failed to write file: {}", e))?;
            eprintln!("Formatted: {}", path.display());
        }
    } else {
        // Output to stdout
        print!("{}", formatted);
    }

    Ok(0)
}

/// Format from stdin
fn format_stdin(config: &FormatterConfig, opt: &EucalyptOptions) -> Result<i32, String> {
    let mut source = String::new();
    io::stdin()
        .read_to_string(&mut source)
        .map_err(|e| format!("Failed to read stdin: {}", e))?;

    if opt.format_check() {
        let needs_fmt = needs_formatting(&source, config)?;
        if needs_fmt {
            eprintln!("Would reformat: <stdin>");
            return Ok(1);
        }
        return Ok(0);
    }

    let formatted = format_source(&source, config)?;
    print!("{}", formatted);

    Ok(0)
}
