//! Exercise the real CLI, native toolchain boundary, and the headless demo.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_DIRECTORY: AtomicUsize = AtomicUsize::new(0);

struct Workspace(PathBuf);

impl Workspace {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "cardamom driver {} {}",
            std::process::id(),
            NEXT_DIRECTORY.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        Self(path)
    }

    fn path(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    fn compiler(&self) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_cardamom"));
        command.current_dir(&self.0).env_remove("CXX");
        command
    }
}

impl Drop for Workspace {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn success(command: &mut Command) -> Output {
    let output = command.output().expect("command should start");
    assert!(
        output.status.success(),
        "{command:?} failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn fixture(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(relative)
}

#[test]
fn native_headers_libraries_and_paths_with_spaces_reach_the_compiler() {
    let workspace = Workspace::new();
    let native = workspace.path("native support");
    fs::create_dir(&native).unwrap();
    fs::write(native.join("answer.h"), "int linked_answer(int value);\n").unwrap();
    fs::write(
        native.join("answer.cpp"),
        "int linked_answer(int value) { return value * 2; }\n",
    )
    .unwrap();
    success(
        Command::new("g++")
            .arg("-c")
            .arg(native.join("answer.cpp"))
            .arg("-o")
            .arg(native.join("answer.o")),
    );
    success(
        Command::new("ar")
            .arg("crs")
            .arg(native.join("libanswer.a"))
            .arg(native.join("answer.o")),
    );

    let input = workspace.path("input with spaces.crdm");
    fs::write(
        &input,
        r#"fn main() -> int {
    @include("<answer.h>");
    @cpp("return linked_answer(CARDAMOM_TEST_INPUT) == 42 ? 0 : 1;");
}"#,
    )
    .unwrap();
    let binary = workspace.path("native demo");
    success(
        workspace
            .compiler()
            // An explicit compiler must take precedence over CXX.
            .env("CXX", workspace.path("missing compiler"))
            .arg(&input)
            .arg("-o")
            .arg(&binary)
            .args(["-out", "--cxx", "g++", "--", "-O2", "-I"])
            .arg(&native)
            .arg("-L")
            .arg(&native)
            .args(["-lanswer", "-DCARDAMOM_TEST_INPUT=21"]),
    );
    success(&mut Command::new(binary));
    assert!(workspace.path("native demo.cpp").is_file());
    assert!(!workspace.path("output").exists());
    assert!(!workspace.path("output.cpp").exists());
}

#[test]
fn default_output_and_borrow_copy_still_run() {
    let workspace = Workspace::new();
    success(
        workspace
            .compiler()
            .arg(fixture("tests/pass/reference_copy.crdm")),
    );
    success(&mut Command::new(workspace.path("output")));
    assert!(!workspace.path("output.cpp").exists());
}

#[test]
fn bad_options_are_rejected_before_compilation() {
    let workspace = Workspace::new();
    for args in [
        vec!["-o"],
        vec!["--cxx"],
        vec!["--output", "--help"],
        vec!["one.crdm", "two.crdm"],
        vec!["one.crdm", "-O2"],
        vec!["--", "-lraylib"],
    ] {
        let output = workspace.compiler().args(&args).output().unwrap();
        assert!(!output.status.success(), "{args:?} should fail");
        assert!(!String::from_utf8_lossy(&output.stderr).contains("panicked"));
    }
    assert!(!workspace.path("output.cpp").exists());
    success(workspace.compiler().arg("--help"));
    success(&mut workspace.compiler());
}

#[test]
fn toolchain_failures_keep_cpp_and_report_an_error() {
    let workspace = Workspace::new();
    let input = workspace.path("input.crdm");
    fs::write(&input, "fn main() {}\n").unwrap();
    let missing = workspace.path("missing compiler");
    let output = workspace
        .compiler()
        // Also verify the environment default, without an explicit --cxx.
        .env("CXX", &missing)
        .arg(&input)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success());
    assert!(stderr.contains("Could not run C++ compiler"), "{stderr}");
    assert!(stderr.contains("Generated C++:"), "{stderr}");
    assert!(workspace.path("output.cpp").is_file());
    assert!(!workspace.path("output").exists());

    let output = workspace
        .compiler()
        .arg(&input)
        .args(["--", "-lcardamom_library_that_does_not_exist"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success());
    assert!(stderr.contains("C++ compilation failed"), "{stderr}");
    assert!(workspace.path("output.cpp").is_file());
}

#[test]
fn output_paths_cannot_overwrite_source_files() {
    let workspace = Workspace::new();
    let source = "fn main() {}\n";
    // The root source need not have a .crdm extension. Cover both the executable
    // and its generated .cpp path colliding with the input.
    let input = workspace.path("app.cpp");
    fs::write(&input, source).unwrap();
    for output in [workspace.path("app.cpp"), workspace.path("app")] {
        let result = workspace
            .compiler()
            .arg(&input)
            .arg("-o")
            .arg(output)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("Refusing to overwrite"));
        assert_eq!(fs::read_to_string(&input).unwrap(), source);
    }
}

#[test]
fn boids_simulation_runs_without_raylib_or_a_display() {
    let workspace = Workspace::new();
    let binary = workspace.path("boids check");
    success(
        workspace
            .compiler()
            .arg(fixture("examples/boids/check.crdm"))
            .arg("--output")
            .arg(&binary)
            .args(["--", "-O2"]),
    );
    success(&mut Command::new(binary));
}
