import glob
import subprocess
from pathlib import Path


def complgen_dump_regex(
    complgen_binary_path: Path, path: str
) -> subprocess.CompletedProcess:
    return subprocess.run(
        [complgen_binary_path, "--regex", "/dev/null", "--bash", "/dev/null", path],
        capture_output=True,
        text=True,
    )


def complgen_dump_dfa(
    complgen_binary_path: Path, path: str
) -> subprocess.CompletedProcess:
    return subprocess.run(
        [complgen_binary_path, "--dfa", "/dev/null", "--bash", "/dev/null", path],
        capture_output=True,
        text=True,
    )


def test_examples_dump_regex(complgen_binary_path: Path, examples_directory_path: Path):
    for usage_file_path in glob.glob(str(examples_directory_path / "*.usage")):
        r = complgen_dump_regex(complgen_binary_path, usage_file_path)
        assert r.returncode == 0, usage_file_path


def test_examples_dump_dfa(complgen_binary_path: Path, examples_directory_path: Path):
    for usage_file_path in glob.glob(str(examples_directory_path / "*.usage")):
        r = complgen_dump_dfa(complgen_binary_path, usage_file_path)
        assert r.returncode == 0, usage_file_path
