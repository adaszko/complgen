import os
import string
import tempfile
from pathlib import Path
from subprocess import CalledProcessError

import pytest
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR
from conftest import (
    get_sorted_jit_fish_completions,
    set_working_dir,
)
from hypothesis import given, settings
from hypothesis.strategies import text

SPECIAL_CHARACTERS = "?[^a]*{foo,*bar}"


def test_jit_completes_paths_fish(complgen_binary_path: Path):
    GRAMMAR = """cmd <PATH> [--help];"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path("filename with spaces").write_text("dummy")
            Path(SPECIAL_CHARACTERS).write_text("dummy")
            os.mkdir("dir with spaces")
            expected = sorted(
                [
                    (SPECIAL_CHARACTERS, ""),
                    ("filename with spaces", ""),
                    ("dir with spaces/", ""),
                ]
            )
            actual = get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR)
            assert actual == expected


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    GRAMMAR = """cmd <PATH>;"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir("subdir")
            (Path("subdir") / "file.txt").write_text("dummy")
            assert get_sorted_jit_fish_completions(
                complgen_binary_path, GRAMMAR, prefix="subdir/"
            ) == sorted([("subdir/file.txt", "")])


def test_jit_completes_directories_fish(complgen_binary_path: Path):
    GRAMMAR = """cmd <DIRECTORY> [--help];"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir("dir with spaces")
            os.mkdir(SPECIAL_CHARACTERS)
            Path("filename with spaces").write_text("dummy")
            assert get_sorted_jit_fish_completions(
                complgen_binary_path, GRAMMAR
            ) == sorted(
                [
                    (SPECIAL_CHARACTERS + "/", "Directory"),
                    ("dir with spaces/", "Directory"),
                ]
            )


def test_jit_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = (
        """cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@fish> ::= {{{ echo fish }}};"""
    )
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR) == sorted(
        [("fish", "")]
    )


def test_nontail_alternative(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> | right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="rig"
    ) == sorted([("fish", "")])


def test_nontail_fallback(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> || right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="rig"
    ) == sorted([("fish", "")])


def test_nontail_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT>right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="left"
    ) == sorted([("leftright", "")])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, ["+stable-aarch64-apple-darwin"]
    ) == sorted([("foo", "")])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="+"
    ) == sorted(
        [("+stable-aarch64-apple-darwin", ""), ("+stable-x86_64-apple-darwin", "")]
    )


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, STRACE_EXPR_GRAMMAR, ["-e"], prefix="trace="
    ) == sorted(
        [("trace=!", ""), ("trace=%file", ""), ("trace=file", ""), ("trace=all", "")]
    )


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, LSOF_FILTER_GRAMMAR, prefix="-sTCP:"
    ) == sorted([("-sTCP:^", ""), ("-sTCP:LISTEN", ""), ("-sTCP:CLOSED", "")])


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(arg1 "descr1" | arg2 "descr2");"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="--option="
    ) == sorted([("--option=arg1", "descr1"), ("--option=arg2", "descr2")])


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option={{{ echo -e "argument\tdescription" }}};"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="--option="
    ) == sorted([("--option=argument", "description")])


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@fish> ::= {{{ echo fish }}};
"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="--option="
    ) == sorted([("--option=fish", "")])


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""
mygrep (--color=<WHEN> || --colour=<WHEN>);
<WHEN> ::= always | never | auto;
"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix=""
    ) == sorted([("--color=", "")])


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""
mygrep (--color=<WHEN> || --colour=<WHEN>);
<WHEN> ::= always | never | auto;
"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="--colou"
    ) == sorted([("--colour=", "")])


def test_fallback_bug1(complgen_binary_path: Path):
    GRAMMAR = r"""lighthouse (account || --disable-log-timestamp);"""
    assert get_sorted_jit_fish_completions(
        complgen_binary_path, GRAMMAR, prefix="--"
    ) == sorted([("--disable-log-timestamp", "")])


def test_funky_spec_command_name(complgen_binary_path: Path):
    GRAMMAR = r"""// <NONTERM>; <NONTERM@fish> ::= {{{ echo dummy }}};"""
    with pytest.raises(CalledProcessError):
        get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR)


LITERALS_ALPHABET = string.ascii_letters + ":=/"


@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = """cmd {};""".format(literal)
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR) == sorted(
        [(literal, "")]
    )
