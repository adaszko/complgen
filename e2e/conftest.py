import os
import sys
import subprocess
import contextlib
from pathlib import Path

import pytest


@pytest.fixture(scope='session')
def cargo_manifest_path() -> Path:
    this_file = Path(__file__)
    return this_file.parent.parent / 'Cargo.toml'


@pytest.fixture(scope='session')
def usage_directory_path() -> Path:
    this_file = Path(__file__)
    return this_file.parent.parent / 'usage'


@pytest.fixture(scope='session')
def complgen_binary_path(cargo_manifest_path: Path) -> Path:
    subprocess.run(['cargo', 'build', '--manifest-path', cargo_manifest_path, '--release'], cwd='..', stdout=sys.stdout, stderr=sys.stderr)
    binary_path = cargo_manifest_path.parent / 'target/release/complgen'
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


def get_sorted_bash_completions(completions_file_path: Path, input: str) -> list[str]:
    bash_process = subprocess.run(['bash', '--noprofile', '--rcfile', completions_file_path, '-i'], input=input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    lines = bash_process.stdout.decode().splitlines()
    lines.sort()
    return lines


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t', maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result
