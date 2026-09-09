//! Compiler driver options. Arguments after `--` go directly to the C++ compiler.

use std::ffi::OsString;
use std::path::PathBuf;

use crate::utils::flags::{FLAG_AST, FLAG_DEBUG, FLAG_NO_COLOR, FLAG_SHOW_OUTPUT};

pub const HELP: &str = "Usage: cardamom [options] <file> [-- <C++ compiler arguments>...]

Options:
  -o, --output <path>  Executable to write (default: output)
  --cxx <executable>   C++ compiler (default: $CXX, or g++)
  -out, --keep-cpp     Keep generated C++ at <output>.cpp
  -ast                Display the AST
  -d, --debug         Enable debug output
  -nc, --no-colour    Disable coloured output
  -h, --help          Display this help

Examples:
  cardamom hello.crdm
  cardamom app.crdm -o app -- -O2 -I/path/include -L/path/lib -lraylib
  cardamom app.crdm --cxx clang++ -- -framework Cocoa

C++ arguments are passed individually, without a shell. Quote paths with spaces.";

pub struct Options {
    pub input: PathBuf,
    pub output: PathBuf,
    pub cxx: Option<OsString>,
    pub cxx_args: Vec<OsString>,
    pub flags: u32,
}

pub enum Invocation {
    Help,
    Compile(Options),
}

pub fn parse(args: impl IntoIterator<Item = OsString>) -> Result<Invocation, String> {
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return Ok(Invocation::Help);
    }

    let mut input = None;
    let mut output = PathBuf::from("output");
    let mut cxx = None;
    let mut cxx_args = Vec::new();
    let mut flags = 0;

    while let Some(arg) = args.next() {
        match arg.to_str() {
            Some("-h" | "--help") => return Ok(Invocation::Help),
            Some("-o" | "--output") => {
                output = PathBuf::from(value(&mut args, "--output")?);
            }
            Some("--cxx") => cxx = Some(value(&mut args, "--cxx")?),
            Some("-out" | "--keep-cpp") => flags |= FLAG_SHOW_OUTPUT,
            Some("-ast") => flags |= FLAG_AST,
            Some("-d" | "--debug") => flags |= FLAG_DEBUG,
            Some("-nc" | "--no-colour") => flags |= FLAG_NO_COLOR,
            Some("--") => {
                cxx_args.extend(args);
                break;
            }
            Some(option) if option.starts_with('-') => {
                return Err(format!(
                    "Unknown option `{option}`. Put C++ compiler arguments after `--`."
                ));
            }
            _ => {
                if input.replace(PathBuf::from(arg)).is_some() {
                    return Err(
                        "Expected one input file. Put C++ compiler arguments after `--`.".into(),
                    );
                }
            }
        }
    }

    Ok(Invocation::Compile(Options {
        input: input.ok_or("No input file supplied. See `cardamom --help`.")?,
        output,
        cxx,
        cxx_args,
        flags,
    }))
}

fn value(args: &mut impl Iterator<Item = OsString>, option: &str) -> Result<OsString, String> {
    match args.next() {
        Some(value) if !value.is_empty() && !value.to_string_lossy().starts_with('-') => Ok(value),
        _ => Err(format!("Expected a value after `{option}`.")),
    }
}
