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
fn dynamic_traits_compile_and_execute() {
    let workspace = Workspace::new();
    for name in ["dynamic_traits", "dynamic_imports", "dynamic_values"] {
        let binary = workspace.path(name);
        success(
            workspace
                .compiler()
                .arg(fixture(&format!("tests/pass/{name}.crdm")))
                .arg("-o")
                .arg(&binary)
                .arg("-out"),
        );
        let output = success(&mut Command::new(&binary));
        if name == "dynamic_traits" {
            assert_eq!(String::from_utf8_lossy(&output.stdout), "42\nCardamom\n");
            let cpp = fs::read_to_string(binary.with_extension("cpp")).unwrap();
            // Both concrete values enter the same unspecialised function, and
            // repeated casts share a single immutable table.
            assert_eq!(
                cpp.matches("void show(cardamom_dynamic_fmt_2ePrintable value)\n")
                    .count(),
                1
            );
            assert_eq!(cpp.matches("static const cardamom_dynamic_fmt_2ePrintable_vtable cardamom_dynamic_fmt_2ePrintable_for_int_impl_fmt_table =").count(), 1);
        }
    }
}

#[test]
fn invalid_dynamic_traits_are_rejected_before_codegen() {
    let workspace = Workspace::new();
    let prelude = r#"
import fmt.{Printable};
fn show(value: &dynamic Printable) { value.text(); }
"#;
    let cases = [
        ("fn bad(value: dynamic Printable) {}", "only supported as direct"),
        ("fn bad(value: &mut dynamic Printable) {}", "only supported as direct"),
        ("fn bad(value: & &dynamic Printable) {}", "only supported as direct"),
        ("fn bad(value: &dynamic Missing) {}", "Unknown dynamic trait"),
        ("class Thing {} fn bad(value: &dynamic Thing) {}", "Unknown dynamic trait"),
        ("fn bad(value: &dynamic Printable<int>) {}", "expects 0 type arguments"),
        ("trait Read<T> { read() -> T; } fn bad(value: &dynamic Read) {}", "expects 1 type arguments"),
        ("trait Read<T> { read() -> T; } fn bad<U>(value: &dynamic Read<U>) {}", "must be concrete owned types"),
        ("trait Read<T> { read() -> T; } fn bad(value: &dynamic Read<Missing>) {}", "must be concrete owned types"),
        ("trait Read<T> { read() -> T; } class Box<T> {} fn bad(value: &dynamic Read<Box>) {}", "must be concrete owned types"),
        ("trait Read<T> { read() -> T; } class Box<T> {} fn bad(value: &dynamic Read<Box<int, string>>) {}", "must be concrete owned types"),
        ("trait Read<T> { read() -> T; } fn bad(value: &dynamic Read<&int>) {}", "must be concrete owned types"),
        ("trait Bad { make() -> Self; } fn bad(value: &dynamic Bad) {}", "cannot use Self"),
        ("trait Bad { equal(value: Self) -> bool; } fn bad(value: &dynamic Bad) {}", "cannot use Self"),
        ("trait Bad { generic<T>(value: T) -> T; } fn bad(value: &dynamic Bad) {}", "generic methods"),
        ("trait Bad { static make() -> int; } fn bad(value: &dynamic Bad) {}", "static methods"),
        ("trait Bad { borrow() -> &int; } fn bad(value: &dynamic Bad) {}", "borrowed results"),
        ("trait Bad { read() -> Missing; } fn bad(value: &dynamic Bad) {}", "concrete valid signature types"),
        ("trait Bad { read(value: void); } fn bad(value: &dynamic Bad) {}", "parameters cannot have type void"),
        ("trait Bad { accept(value: &dynamic Printable); } fn bad(value: &dynamic Bad) {}", "other dynamic objects"),
        ("trait Bad { read() -> int where Self: Printable; } fn bad(value: &dynamic Bad) {}", "generic methods"),
        ("fn extern native(value: &dynamic Printable) {}", "only supported as direct"),
        ("class Holder(private value: &dynamic Printable) {}", "cannot be stored or returned"),
        ("fn bad(values: (&dynamic Printable)[]) {}", "only supported as direct"),
        ("fn bad(callback: fn(&dynamic Printable) -> string) {}", "only supported as direct"),
        ("fn leak(value: &dynamic Printable) -> &dynamic Printable { return value; }", "cannot be stored or returned"),
        ("fn bad(value: &dynamic Printable) { let saved: &dynamic Printable = value; }", "cannot be stored or returned"),
        ("fn bad(value: &dynamic Printable) { [value]; }", "cannot escape"),
        ("fn bad(value: &dynamic Printable) { (1, value); }", "cannot escape"),
        ("fn bad(value: &dynamic Printable) { let f: fn() -> string = || string -> { return value.text(); }; }", "cannot capture"),
        ("fn bad() { let n: int = 1; as &dynamic Printable (&n); }", "cannot escape"),
        ("fn bad() { let n: int = 1; show(&n); }", "mismatch"),
        ("fn bad() { let n: int = 1; show(as &dynamic Printable n); }", "explicit immutable borrow"),
        ("fn bad() { let n: int = 1; show(as &dynamic Printable (&mut n)); }", "explicit immutable borrow"),
        ("fn bad() { show(as &dynamic Printable (&1)); }", "not temporaries, fields or indexes"),
        ("fn make() -> int { return 1; } fn bad() { show(as &dynamic Printable (&make())); }", "not temporaries, fields or indexes"),
        ("fn bad(n: &int) { show(as &dynamic Printable (&n)); }", "owned named value with a concrete type"),
        ("fn bad(n: &mut int) { show(as &dynamic Printable (&n)); }", "owned named value with a concrete type"),
        ("fn bad<T>(n: T) where T: Printable { show(as &dynamic Printable (&n)); }", "owned named value with a concrete type"),
        ("fn bad() { let n: int[] = [1]; show(as &dynamic Printable (&n[0])); }", "not temporaries, fields or indexes"),
        ("class Box(public n: int) {} fn bad() { let b: Box = new Box(1); show(as &dynamic Printable (&b.n)); }", "not temporaries, fields or indexes"),
        ("class Box { public text() -> string { return \"box\"; } public bad() { show(as &dynamic Printable (&this)); } }", "owned named value with a concrete type"),
        ("fn bad(value: &dynamic Printable) { let f: fn() -> string = value.text; }", "must be called directly"),
        ("fn bad(value: &dynamic Printable) { value.text<int>(); }", "do not accept type arguments"),
        ("fn identity<T>(value: T) -> T { return value; } fn bad(value: &dynamic Printable) { show(identity(value)); }", "cannot escape"),
        ("fn bad() { let f: fn(&dynamic Printable) -> void = show; }", "cannot be stored or returned"),
        ("class Box<T>(private value: T) {} fn bad(value: &dynamic Printable) { new Box<&dynamic Printable>(value); }", "cannot escape"),
        ("class Empty {} fn bad() { let e: Empty = new Empty(); show(as &dynamic Printable (&e)); }", "missing method `text`"),
        ("class Private { private text() -> string { return \"x\"; } } fn bad() { let e: Private = new Private(); show(as &dynamic Printable (&e)); }", "missing method `text`"),
        ("class Wrong { public text() -> int { return 1; } } fn bad() { let e: Wrong = new Wrong(); show(as &dynamic Printable (&e)); }", "incompatible signature"),
        ("class Mutable(private n: int) { public text() -> string { this.n += 1; return \"x\"; } } fn bad() { let e: Mutable = new Mutable(0); show(as &dynamic Printable (&e)); }", "must be read-only"),
        ("class Mutable(private n: int) { private change<T>(value: T) { this.n += 1; } public text() -> string { this.change<int>(1); return \"x\"; } } fn bad() { let e: Mutable = new Mutable(0); show(as &dynamic Printable (&e)); }", "must be read-only"),
        ("fn change(n: &mut int) { n += 1; } class Mutable(private n: int) { public text() -> string { change(&mut this.n); return \"x\"; } } fn bad() { let e: Mutable = new Mutable(0); show(as &dynamic Printable (&e)); }", "must be read-only"),
        ("fn change(n: &mut int) { n += 1; } class Mutable(private n: int) { public text() -> string { change(this.n); return \"x\"; } } fn bad() { let e: Mutable = new Mutable(0); show(as &dynamic Printable (&e)); }", "must be read-only"),
        ("class Mutable(private n: int) { public text() -> string { let alias: &mut int = this.n; alias += 1; return \"x\"; } } fn bad() { let e: Mutable = new Mutable(0); show(as &dynamic Printable (&e)); }", "must be read-only"),
    ];
    let mut failures = Vec::new();
    for (index, (source, diagnostic)) in cases.iter().enumerate() {
        let input = workspace.path("input.crdm");
        fs::write(&input, format!("{prelude}\n{source}\nfn main() {{}}\n")).unwrap();
        let rejected = workspace.path(&format!("rejected_{index}"));
        let output = workspace
            .compiler()
            .arg(&input)
            .arg("--cxx")
            .arg(workspace.path("must not run"))
            .arg("-o")
            .arg(&rejected)
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        if output.status.success()
            || stderr.contains("panicked")
            || !stderr.contains(diagnostic)
            || stderr.contains("Could not run C++ compiler")
            || rejected.with_extension("cpp").exists()
        {
            failures.push(format!(
                "{source}: expected `{diagnostic}` before codegen:\n{stderr}"
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn selective_imports_compile_and_execute() {
    let workspace = Workspace::new();
    for name in ["selective_imports", "selective_import_types"] {
        let binary = workspace.path(name);
        success(
            workspace
                .compiler()
                .arg(fixture(&format!("tests/pass/{name}.crdm")))
                .arg("-o")
                .arg(&binary)
                .arg("-out"),
        );
        success(&mut Command::new(&binary));
        let cpp = fs::read_to_string(binary.with_extension("cpp")).unwrap();
        assert!(!cpp.contains("cardamom_unused_import"));
        assert!(!cpp.contains("cardamom_shadowed_import"));
    }
}

#[test]
fn invalid_selective_imports_are_rejected_before_codegen() {
    let workspace = Workspace::new();
    fs::write(
        workspace.path("library.crdm"),
        r#"
public fn visible(value: int) -> int { return value; }
fn hidden() -> int { return 0; }
public class Item {}
class HiddenItem {}
public trait Readable { read() -> int; }
trait HiddenTrait { read() -> int; }
"#,
    )
    .unwrap();
    fs::write(workspace.path("bridge.crdm"), "import library.{visible};\n").unwrap();
    for (source, diagnostic) in [
        (
            "import library.{missing}; fn main() {}",
            "No public export `missing`",
        ),
        (
            "import library.{hidden}; fn main() {}",
            "No public export `hidden`",
        ),
        (
            "import library.{HiddenItem}; fn main() {}",
            "No public export `HiddenItem`",
        ),
        (
            "import library.{HiddenTrait}; fn main() {}",
            "No public export `HiddenTrait`",
        ),
        (
            "import library.{visible, visible}; fn main() {}",
            "`visible` is imported more than once",
        ),
        (
            "import library.{visible as Item, Item}; fn main() {}",
            "`Item` is imported more than once",
        ),
        (
            "import library as visible; import library.{visible}; fn main() {}",
            "`visible` is imported more than once",
        ),
        (
            "import library.{visible}; import library.{visible}; fn main() {}",
            "`visible` is imported more than once",
        ),
        (
            "import library.{visible}; fn visible() {} fn main() {}",
            "Import `visible` conflicts with a top-level declaration",
        ),
        (
            "class Item {} import library.{Item}; fn main() {}",
            "Import `Item` conflicts with a top-level declaration",
        ),
        (
            "import library.{Readable}; trait Readable {} fn main() {}",
            "Import `Readable` conflicts with a top-level declaration",
        ),
        (
            "import library.{visible}; let visible: int = 1; fn main() {}",
            "Import `visible` conflicts with a top-level declaration",
        ),
        (
            "import library.{visible}; fn main() { library.visible(1); }",
            "Unknown variable `library`",
        ),
        (
            "import library.{visible}; fn main() { visible(); }",
            "expects 1 args",
        ),
        (
            "import library.{visible}; fn main() { visible<int>(1); }",
            "is not generic",
        ),
        (
            "import library.{Readable}; fn main() { Readable; }",
            "cannot be used as a value",
        ),
        (
            "import bridge.{visible}; fn main() {}",
            "No public export `visible`",
        ),
        (
            "fn main() { import library.{visible}; }",
            "Imports are only allowed at module scope",
        ),
        ("import library.{}; fn main() {}", "Expected an export name"),
        (
            "import library.{visible as}; fn main() {}",
            "Expected identifier after 'as'",
        ),
        (
            "import library.{visible Item}; fn main() {}",
            "Expected '}' after imported names",
        ),
        (
            "import library.{visible}; fn main() { let visible: int = 1; visible(2); }",
            "Cannot call non-function type `int`",
        ),
        (
            "import missing.{visible}; fn main() {}",
            "Cannot find module `missing`",
        ),
    ] {
        let input = workspace.path("input.crdm");
        fs::write(&input, source).unwrap();
        let output = workspace
            .compiler()
            .arg(&input)
            .arg("--cxx")
            .arg(workspace.path("must not run"))
            .arg("-o")
            .arg(workspace.path("rejected"))
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{source} unexpectedly passed");
        assert!(!stderr.contains("panicked"), "{source} panicked:\n{stderr}");
        assert!(
            stderr.contains(diagnostic),
            "{source}: missing {diagnostic}:\n{stderr}"
        );
        assert!(
            !stderr.contains("Could not run C++ compiler"),
            "{source} reached codegen"
        );
        assert!(
            !workspace.path("rejected.cpp").exists(),
            "{source} emitted C++ despite an error"
        );
    }
}

#[test]
fn trait_operators_compile_and_execute() {
    let workspace = Workspace::new();
    for name in ["operators_1.crdm", "operators_2.crdm"] {
        let binary = workspace.path(name);
        success(
            workspace
                .compiler()
                .arg(fixture(&format!("tests/pass/{name}")))
                .arg("-o")
                .arg(&binary),
        );
        success(&mut Command::new(&binary));
    }
}

#[test]
fn invalid_operators_are_rejected_before_cpp_compilation() {
    let workspace = Workspace::new();
    for (name, diagnostic) in [
        ("operator_missing_trait", "missing method `equals`"),
        ("operator_missing_bound", "missing `Eq` bound"),
        (
            "operator_forwarded_bound",
            "does not satisfy trait `cmp.Eq`",
        ),
        ("operator_wrong_rhs", "incompatible signature"),
        ("operator_private_method", "missing method `equals`"),
        ("operator_ambiguous", "ambiguous operator"),
        ("operator_invalid_builtin", "Operator `-` requires"),
        ("operator_compound_output", "cannot be assigned back"),
        (
            "operator_compound_borrow",
            "Cannot assign through immutable borrow",
        ),
        ("operator_wrong_impl_signature", "incompatible signature"),
        ("operator_bad_trait_arity", "expects 2 type arguments"),
        ("operator_duplicate_impl", "ambiguous implementations"),
    ] {
        let output = workspace
            .compiler()
            .arg(fixture(&format!("tests/fail/{name}.crdm")))
            .arg("--cxx")
            .arg(workspace.path("must not run"))
            .arg("-o")
            .arg(workspace.path("rejected"))
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{name} unexpectedly passed");
        assert!(
            stderr.contains(diagnostic),
            "{name}: missing {diagnostic}:\n{stderr}"
        );
        assert!(
            !stderr.contains("Could not run C++ compiler"),
            "{name} reached codegen"
        );
        assert!(
            !workspace.path("rejected.cpp").exists(),
            "{name} emitted C++ despite an error"
        );
    }
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
