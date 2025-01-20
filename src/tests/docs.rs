use std::fs;

#[test]
fn msrv_readme_updated() {
    let readme = fs::read_to_string("README.md").unwrap();
    let cargo_toml = fs::read_to_string("Cargo.toml").unwrap();

    let mut readme_msrv = None;
    let mut cargo_toml_msrv = None;

    for line in readme.lines() {
        if line.starts_with("Minimum Supported Rust Version:") {
            let msrv = line.split_whitespace().last().unwrap_or("");
            if msrv.is_empty() {
                break;
            }
            readme_msrv = Some(msrv);
            break;
        }
    }

    for line in cargo_toml.lines() {
        if line.starts_with("rust-version = ") {
            let msrv = line.split('"').nth(1).unwrap_or("");
            if msrv.is_empty() {
                break;
            }
            cargo_toml_msrv = Some(msrv);
        }
    }

    let readme_msrv = readme_msrv.expect("MSRV not found in README.md");
    let cargo_toml_msrv = cargo_toml_msrv.expect("MSRV not found in Cargo.toml");

    assert_eq!(
        readme_msrv, cargo_toml_msrv,
        "MSRV in README.md and Cargo.toml do not match"
    );
}
