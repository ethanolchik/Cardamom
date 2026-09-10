#!/usr/bin/env python3
"""Build and run the Cardamom boids demo, keeping dependencies under target/boids."""

import argparse
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "target" / "boids"
RAYLIB_VERSION = "5.5"
RAYLIB_COMMIT = "c1ab645ca298a2801097931d1079b10ff7eb9df8"


def run(command, **kwargs):
    command = [str(part) for part in command]
    print("+ " + shlex.join(command), flush=True)
    return subprocess.run(command, cwd=ROOT, check=True, **kwargs)


def require(executable):
    if not shutil.which(executable):
        raise SystemExit(f"Missing {executable}. See examples/boids/README.md for setup.")


def raylib_prefix(existing):
    if existing:
        prefix = Path(existing).expanduser().resolve()
        if not (prefix / "include" / "raylib.h").is_file():
            raise SystemExit(f"No include/raylib.h under {prefix}")
        return prefix

    prefix = BUILD / f"raylib-{RAYLIB_VERSION}"
    if (prefix / "lib" / "libraylib.a").is_file() and (prefix / "include" / "raylib.h").is_file():
        return prefix

    require("git")
    require("cmake")
    source = BUILD / f"raylib-{RAYLIB_VERSION}-src"
    if not source.exists():
        run(["git", "clone", "--depth", "1", "--branch", RAYLIB_VERSION,
             "https://github.com/raysan5/raylib.git", source])
    revision = run(["git", "-C", source, "rev-parse", "HEAD"],
                   capture_output=True, text=True).stdout.strip()
    if revision != RAYLIB_COMMIT:
        raise SystemExit(f"Expected raylib {RAYLIB_COMMIT}, found {revision} in {source}")

    native_build = BUILD / f"raylib-{RAYLIB_VERSION}-build"
    run(["cmake", "-S", source, "-B", native_build,
         "-DCMAKE_BUILD_TYPE=Release", "-DBUILD_EXAMPLES=OFF", "-DBUILD_SHARED_LIBS=OFF",
         # raylib 5.5 and bundled GLFW predate CMake 4's minimum-policy change.
         "-DCMAKE_POLICY_VERSION_MINIMUM=3.5", "-DGLFW_BUILD_WAYLAND=OFF",
         "-DCMAKE_INSTALL_LIBDIR=lib", f"-DCMAKE_INSTALL_PREFIX={prefix}"])
    run(["cmake", "--build", native_build, "--parallel", str(min(os.cpu_count() or 2, 8))])
    run(["cmake", "--install", native_build])
    return prefix


def main(demo="boids"):
    programs = {
        "boids": ("main.crdm", "check.crdm", "check", "preview.png"),
        "forks": ("forks.crdm", "forks_check.crdm", "forks-check", "forks-preview.png"),
    }
    source, check_source, check_binary, image_name = programs[demo]
    description = __doc__ if demo == "boids" else "Build and run Forking paths, Cardamom's musical flock ensemble."
    parser = argparse.ArgumentParser(description=description)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--build-only", action="store_true", help="compile without opening a window")
    mode.add_argument("--check", action="store_true", help="run the simulation and score checks without raylib")
    mode.add_argument("--smoke", action="store_true", help=f"run briefly, save {image_name}, and exit")
    if demo == "forks":
        mode.add_argument("--render-audio", action="store_true",
                          help="render the selected ensemble to stereo WAV without raylib or a window")
    smoke_frames = 300 if demo == "forks" else 180
    parser.add_argument("--frames", type=int, default=smoke_frames,
                        help=f"number of frames for --smoke (default: {smoke_frames})")
    parser.add_argument("--raylib-prefix", default=os.environ.get("RAYLIB_PREFIX"),
                        help="use an existing raylib install instead of building the pinned version")
    parser.add_argument("--cxx", default=os.environ.get("CXX", "g++"), help="C++ compiler executable")
    if demo == "forks":
        parser.add_argument("--silent", action="store_true", help="run without opening an audio device")
        parser.add_argument("--scene", choices=("neon", "glasshouse", "orbital"), default="neon",
                            help="initial ensemble / audio-render preset (default: neon)")
    args = parser.parse_args()
    if args.frames < 1:
        parser.error("--frames must be positive")
    if demo == "forks" and args.smoke:
        if args.frames < 300:
            parser.error("the ensemble interaction smoke test needs at least 300 frames")
        if args.scene != "neon":
            parser.error("the interaction smoke starts with neon; use --scene for normal runs or rendering")
    offline_audio = demo == "forks" and args.render_audio
    if sys.platform not in ("darwin", "linux") and not (args.check or offline_audio):
        parser.error("the graphics build helper currently supports macOS and Linux")

    require("cargo")
    require(args.cxx)
    BUILD.mkdir(parents=True, exist_ok=True)
    # Pin the target directory even if the caller has a global CARGO_TARGET_DIR.
    run(["cargo", "build", "--release", "--target-dir", ROOT / "target"])
    compiler = ROOT / "target" / "release" / "cardamom"

    if offline_audio:
        binary = BUILD / "music-render"
        run([compiler, ROOT / "examples" / "boids" / "music_render.crdm",
             "-o", binary, "--cxx", args.cxx, "--", "-O2"])
        environment = os.environ.copy()
        environment["CARDAMOM_FORKS_WAV"] = str(BUILD / "forks-music.wav")
        environment["CARDAMOM_FORKS_SCENE"] = args.scene
        run([binary], env=environment)
        return

    if args.check:
        checks = [(check_source, check_binary)]
        if demo == "forks":
            checks.append(("music_check.crdm", "music-check"))
        for source_file, executable in checks:
            binary = BUILD / executable
            run([compiler, ROOT / "examples" / "boids" / source_file,
                 "-o", binary, "--cxx", args.cxx, "--", "-O2"])
            run([binary])
        return

    prefix = raylib_prefix(args.raylib_prefix)
    native_args = ["-O2", "-I", prefix / "include", "-L", prefix / "lib", "-lraylib"]
    # Also support shared installations supplied through --raylib-prefix.
    native_args.append(f"-Wl,-rpath,{prefix / 'lib'}")
    if sys.platform == "darwin":
        for framework in ("Cocoa", "IOKit", "CoreVideo", "OpenGL", "CoreAudio", "AudioToolbox"):
            native_args.extend(["-framework", framework])
    else:
        native_args.extend(["-lGL", "-lm", "-lpthread", "-ldl", "-lrt", "-lX11"])

    binary = BUILD / demo
    run([compiler, ROOT / "examples" / "boids" / source, "-o", binary,
         "--keep-cpp", "--cxx", args.cxx, "--", *native_args])
    if args.build_only:
        print(f"Ready: {binary}")
        return

    screenshot = BUILD / image_name
    previous_capture = screenshot.stat().st_mtime_ns if screenshot.exists() else None
    environment = os.environ.copy()
    setting_prefix = f"CARDAMOM_{demo.upper()}"
    environment.pop(f"{setting_prefix}_FRAMES", None)
    environment[f"{setting_prefix}_SCREENSHOT"] = str(screenshot)
    if demo == "forks":
        environment["CARDAMOM_FORKS_SCENE"] = args.scene
    if demo == "forks" and args.silent:
        environment["CARDAMOM_FORKS_SILENT"] = "1"
    if args.smoke:
        environment[f"{setting_prefix}_FRAMES"] = str(args.frames)
    run([binary], env=environment, timeout=max(30, args.frames // 10) if args.smoke else None)
    if args.smoke:
        if not screenshot.is_file() or screenshot.stat().st_mtime_ns == previous_capture:
            raise SystemExit("The smoke run did not produce a new screenshot.")
        print(f"Screenshot: {screenshot}")


def launch(demo="boids"):
    try:
        main(demo)
    except subprocess.CalledProcessError as error:
        raise SystemExit(error.returncode)
    except subprocess.TimeoutExpired:
        raise SystemExit("The smoke run timed out before the window closed.")


if __name__ == "__main__":
    launch()
