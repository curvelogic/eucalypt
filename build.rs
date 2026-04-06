fn main() {
    // Ensure cargo recompiles when baked-in resource files change.
    // These are embedded via include_bytes! in src/driver/resources.rs
    // but cargo's incremental compilation does not automatically track
    // non-Rust files referenced by include_bytes!.
    println!("cargo:rerun-if-changed=lib/prelude.eu");
    println!("cargo:rerun-if-changed=lib/test.eu");
    println!("cargo:rerun-if-changed=lib/lens.eu");
    println!("cargo:rerun-if-changed=lib/state.eu");
    println!("cargo:rerun-if-changed=build-meta.yaml");
}
