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
