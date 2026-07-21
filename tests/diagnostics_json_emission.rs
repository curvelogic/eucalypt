use std::process::Command;

fn run_json(src: &str) -> serde_json::Value {
    let dir = std::env::temp_dir().join(format!("eu-diag-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");
    // JSON may be on stdout or stderr depending on the current wiring — read the one that parses.
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    serde_json::from_str(stdout.trim())
        .or_else(|_| serde_json::from_str(stderr.trim()))
        .unwrap_or_else(|e| {
            panic!("no JSON diagnostic parsed: {e}\nstdout={stdout}\nstderr={stderr}")
        })
}

#[test]
fn json_error_carries_user_location_for_direct_type_error() {
    // Direct user-site type error: JSON must locate it in the user file, not lose the location.
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    let primary = &v["primary"];
    assert_eq!(
        primary["in_user_file"],
        serde_json::json!(true),
        "primary must be in the user file, got {v}"
    );
    assert_eq!(primary["line"], serde_json::json!(2));
}

#[test]
fn type_error_carries_a_stable_code() {
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    assert_eq!(
        v["code"],
        serde_json::json!("EU-EVAL-TYPE"),
        "type mismatch must carry the stable EU-EVAL-TYPE code, got {v}"
    );
}

#[test]
fn error_command_prints_catalogue_entry() {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["error", "EU-EVAL-TYPE"])
        .output()
        .expect("run eu error");
    assert!(
        out.status.success(),
        "eu error EU-EVAL-TYPE should exit 0, stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).to_lowercase();
    assert!(
        stdout.contains("type"),
        "catalogue entry should mention 'type', got: {stdout}"
    );
}
