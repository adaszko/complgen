import contextlib
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Generator, Optional

import pytest


@pytest.fixture(scope="session")
def cargo_manifest_path() -> Path:
    this_file = Path(__file__)
    return this_file.parent.parent / "Cargo.toml"


@pytest.fixture(scope="session")
def usage_directory_path() -> Path:
    this_file = Path(__file__)
    return this_file.parent.parent / "usage"


@pytest.fixture(scope="session")
def complgen_binary_path(cargo_manifest_path: Path) -> Path:
    subprocess.run(
        ["cargo", "build", "--manifest-path", cargo_manifest_path, "--release"],
        cwd="..",
        stdout=sys.stdout,
        stderr=sys.stderr,
    )
    binary_path = cargo_manifest_path.parent / "target/release/complgen"
    assert binary_path.exists()
    return binary_path


@contextlib.contextmanager
def set_working_dir(path: Path):
    origin = Path().absolute()
    try:
        os.chdir(path)
        yield
    finally:
        os.chdir(origin)


@contextlib.contextmanager
def temp_file_with_contents(contents: str) -> Generator[Path, None, None]:
    with tempfile.NamedTemporaryFile() as f:
        f.write(contents.encode())
        f.flush()
        yield Path(f.name)


def get_sorted_bash_completions(completions_file_path: Path, input: str) -> list[str]:
    bash_process = subprocess.run(
        ["bash", "--noprofile", "--rcfile", completions_file_path, "-i"],
        input=input.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    )
    lines = bash_process.stdout.decode().splitlines()
    lines.sort()
    return lines


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split("\t", maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ""))
        else:
            result.append((fields[0], fields[1]))
    return result


def get_sorted_fish_completions(
    completions_script_path: Path, input: str
) -> list[tuple[str, str]]:
    completed_process = subprocess.run(
        [
            "fish",
            "--private",
            "--no-config",
            "--init-command",
            "source {}".format(completions_script_path),
            "--command",
            input,
        ],
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    )
    completions = completed_process.stdout.decode()
    parsed = fish_completions_from_stdout(completions)
    parsed.sort(key=lambda pair: pair[0])
    return parsed


@contextlib.contextmanager
def gen_fish_aot_completion_script_path(
    complgen_binary_path: Path, grammar: str
) -> Generator[Path, None, None]:
    fish_script = subprocess.run(
        [complgen_binary_path, "aot", "--fish-script", "-", "-"],
        input=grammar.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    ).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(fish_script)
        f.flush()
        yield Path(f.name)


@contextlib.contextmanager
def gen_zsh_capture_script_path(completion_script: str) -> Generator[Path, None, None]:
    this_file = Path(__file__)
    capture_preamble_path = this_file.parent / "capture_preamble.zsh"
    capture_postamble_path = this_file.parent / "capture_postamble.zsh"
    with tempfile.NamedTemporaryFile(mode="w") as f:
        f.write(capture_preamble_path.read_text())
        f.write("\n")
        f.write(completion_script.replace("'", "''"))
        f.write("\n")
        f.write(capture_postamble_path.read_text())
        f.flush()
        yield Path(f.name)


@contextlib.contextmanager
def gen_grammar_zsh_capture_script_path(
    complgen_binary_path: Path, grammar: str
) -> Generator[Path, None, None]:
    completion_script = subprocess.run(
        [complgen_binary_path, "aot", "--zsh-script", "-", "-"],
        input=grammar.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    ).stdout.decode()
    with gen_zsh_capture_script_path(completion_script) as path:
        yield path


def get_zsh_capture_script_sorted_lines(
    generated_script_path: Path, input: str
) -> list[str]:
    zsh_process = subprocess.run(
        ["zsh", generated_script_path, input],
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    )
    stdout = zsh_process.stdout.decode()
    completions = stdout.splitlines()
    completions.sort()
    return completions


def get_bash_completion_sh_path() -> str:
    if os.path.exists("/opt/homebrew/etc/profile.d/bash_completion.sh"):
        return "/opt/homebrew/etc/profile.d/bash_completion.sh"
    elif os.path.exists("/etc/bash_completion"):
        return "/etc/bash_completion"
    elif os.path.exists("/usr/share/bash-completion/bash_completion"):
        return "/usr/share/bash-completion/bash_completion"
    else:
        assert False, "Make sure OS package bash-completion is installed"
