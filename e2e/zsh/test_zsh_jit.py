import os
import string
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Optional

from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR
from conftest import (
    gen_zsh_capture_script_path,
    get_zsh_capture_script_sorted_lines,
    set_working_dir,
)
from hypothesis import given, settings
from hypothesis.strategies import text


def get_complgen_jit_output(
    complgen_binary_path: Path,
    grammar: str,
    cmd: str,
    words_before_cursor: list[str] = [],
    prefix: Optional[str] = None,
) -> str:
    """
    words_before_cursor: shell words up to (but not including) the completed one
    prefix: if passed, means the completed word has this before the cursor
    """
    args = [complgen_binary_path, "jit", "--test", cmd, "-", "zsh"]
    if prefix is not None:
        args += ["--prefix={}".format(prefix)]
    args += ["--"]
    args += words_before_cursor
    process = subprocess.run(
        args,
        input=grammar.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    )
    return process.stdout.decode()


def get_sorted_jit_completions(
    complgen_binary_path: Path,
    grammar: str,
    cmd: str,
    words_before_cursor: list[str] = [],
    prefix: Optional[str] = None,
) -> list[str]:
    completion_script = get_complgen_jit_output(
        complgen_binary_path, grammar, cmd, words_before_cursor, prefix
    )
    with gen_zsh_capture_script_path(completion_script) as script_path:
        if words_before_cursor and prefix:
            input = "{} {} {}".format(cmd, " ".join(words_before_cursor), prefix)
        elif words_before_cursor:
            input = "{} {} ".format(cmd, " ".join(words_before_cursor))
        elif prefix:
            input = "{} {}".format(cmd, prefix)
        else:
            input = "{} ".format(cmd)
        sorted_completions = get_zsh_capture_script_sorted_lines(script_path, input)
        return sorted_completions


def test_jit_completes_paths_zsh(complgen_binary_path: Path):
    GRAMMAR = """cmd <PATH> [--help];"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path("filename with spaces").write_text("dummy")
            Path("?[^a]*{foo,*bar}").write_text("dummy")
            os.mkdir("dir with spaces")
            assert get_sorted_jit_completions(
                complgen_binary_path, GRAMMAR, "cmd"
            ) == sorted(
                [
                    "filename\\ with\\ spaces",
                    "\\?\\[\\^a\\]\\*\\{foo,\\*bar\\}",
                    "dir\\ with\\ spaces",
                ]
            )


def test_jit_completes_directories_zsh(complgen_binary_path: Path):
    GRAMMAR = """cmd <DIRECTORY> [--help];"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir("dir with spaces")
            os.mkdir("?[^a]*{foo,*bar}")
            Path("filename with spaces").write_text("dummy")
            assert get_sorted_jit_completions(
                complgen_binary_path, GRAMMAR, "cmd "
            ) == sorted(
                [
                    "\\?\\[\\^a\\]\\*\\{foo,\\*bar\\}",
                    "dir\\ with\\ spaces",
                ]
            )


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    GRAMMAR = """cmd <PATH>;"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir("subdir")
            (Path("subdir") / "file.txt").write_text("dummy")
            assert get_sorted_jit_completions(
                complgen_binary_path, GRAMMAR, "cmd", prefix="subdir/"
            ) == sorted(
                [
                    "subdir/file.txt",
                ]
            )


def test_jit_tcsh_directory_completion(complgen_binary_path: Path):
    GRAMMAR = """cmd <DIRECTORY>;"""
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path("foo/bar/baz").mkdir(parents=True)
            assert get_sorted_jit_completions(
                complgen_binary_path, GRAMMAR, "cmd", prefix="f/b/b"
            ) == sorted(
                [
                    "foo/bar/baz",
                ]
            )


def test_jit_specializes_for_zsh(complgen_binary_path: Path):
    GRAMMAR = (
        """cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@zsh> ::= {{{ compadd zsh }}};"""
    )
    assert get_sorted_jit_completions(complgen_binary_path, GRAMMAR, "cmd ") == sorted(
        [
            "zsh",
        ]
    )


def test_nontail_alternative(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> | right; <LEFT> ::= {{{ echo left }}}@zsh"left";"""
    assert get_sorted_jit_completions(
        complgen_binary_path, GRAMMAR, "cmd rig"
    ) == sorted(["right"])


def test_nontail_fallback(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> || right; <LEFT> ::= {{{ echo left }}}@zsh"left";"""
    assert get_sorted_jit_completions(
        complgen_binary_path, GRAMMAR, "cmd rig"
    ) == sorted(["right"])


def test_nontail_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT>right; <LEFT> ::= {{{ echo left }}}@zsh"left";"""
    assert get_sorted_jit_completions(
        complgen_binary_path, GRAMMAR, "cmd left"
    ) == sorted(["leftright"])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cmd +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    assert get_sorted_jit_completions(
        complgen_binary_path,
        GRAMMAR,
        "cmd",
        words_before_cursor=["+stable-aarch64-apple-darwin"],
    ) == sorted(
        [
            "foo",
        ]
    )


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cmd +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path, GRAMMAR, "cmd", prefix="+"
        )
    ]
    assert actual == sorted(
        [
            ["+stable-aarch64-apple-darwin", "stable-aarch64-apple-darwin"],
            ["+stable-x86_64-apple-darwin", "stable-x86_64-apple-darwin"],
        ]
    )


def test_jit_completes_in_word(complgen_binary_path: Path):
    GRAMMAR = """
cmd (prefix-infix-foo | prefix-infix-bar);
"""
    assert get_sorted_jit_completions(
        complgen_binary_path, GRAMMAR, "cmd prefix-"
    ) == sorted(
        [
            "prefix-infix-bar",
            "prefix-infix-foo",
        ]
    )


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path,
            STRACE_EXPR_GRAMMAR,
            "strace",
            words_before_cursor=["-e"],
            prefix="trace=",
        )
    ]
    assert actual == sorted(
        [
            ["trace=!", "!"],
            ["trace=%file", "%file"],
            ["trace=all", "all"],
            ["trace=file", "file"],
        ]
    )


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path, LSOF_FILTER_GRAMMAR, "lsof", prefix="-sTCP:"
        )
    ]
    assert actual == sorted(
        [
            ["-sTCP:CLOSED", "CLOSED"],
            ["-sTCP:LISTEN", "LISTEN"],
            ["-sTCP:^", "^"],
        ]
    )


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(arg1 "descr1" | arg2 "descr2");"""
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path, GRAMMAR, "cmd", prefix="--option="
        )
    ]
    assert actual == sorted(
        [
            ["--option=arg1", "--option=arg1", "--", "descr1"],
            ["--option=arg2", "--option=arg2", "--", "descr2"],
        ]
    )


def test_jit_subword_descriptions_bug(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --binary-files=<TYPE> "assume that binary files are <TYPE>";
<TYPE> ::= binary "Search binary files but do not print them"
         | text "Treat all files as text"
         | without-match "Do not search binary files";
"""
    actual = [
        s.split(maxsplit=2)
        for s in get_sorted_jit_completions(
            complgen_binary_path, GRAMMAR, "cmd", prefix="--binary-files="
        )
    ]
    assert actual == sorted(
        [
            [
                "--binary-files=binary",
                "--binary-files=binary",
                "-- Search binary files but do not print them",
            ],
            [
                "--binary-files=text",
                "--binary-files=text",
                "-- Treat all files as text",
            ],
            [
                "--binary-files=without-match",
                "--binary-files=without-match",
                "-- Do not search binary files",
            ],
        ]
    )


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option={{{ echo -e "argument\tdescription" }}};"""
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path, GRAMMAR, "cmd", prefix="--option="
        )
    ]
    assert actual == sorted(
        [
            ["--option=argument", "argument", "--", "description"],
        ]
    )


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@zsh> ::= {{{ echo zsh }}};
"""
    actual = [
        s.split()
        for s in get_sorted_jit_completions(
            complgen_binary_path, GRAMMAR, "cmd", prefix="--option="
        )
    ]
    assert actual == sorted(
        [
            ["--option=zsh"],  # TODO This should probably say just "zsh"
        ]
    )


def test_jit_sample_regression(complgen_binary_path: Path):
    GRAMMAR = r"""
trivial --color=<WHEN>;
<WHEN> ::= always | never | auto;
"""
    actual = [
        s.split()
        for s in get_sorted_jit_completions(complgen_binary_path, GRAMMAR, "trivial")
    ]
    assert actual == sorted(
        [
            ["--color=", "--color="],  # TODO This should probably say just ['--color=']
        ]
    )


def test_fallback_completion(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    assert get_sorted_jit_completions(complgen_binary_path, GRAMMAR, "cmd") == sorted(
        ["foo"]
    )


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    assert get_sorted_jit_completions(
        complgen_binary_path, GRAMMAR, "cmd", prefix="--"
    ) == sorted(["--bar"])


def test_funky_spec_command_name(complgen_binary_path: Path):
    GRAMMAR = r"""// <NONTERM>; <NONTERM@zsh> ::= {{{ echo dummy }}};"""
    assert get_sorted_jit_completions(complgen_binary_path, GRAMMAR, "// ") == sorted(
        ["dummy"]
    )


LITERALS_ALPHABET = string.ascii_letters + ":="


@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = """cmd {};""".format(literal)
    assert get_sorted_jit_completions(complgen_binary_path, GRAMMAR, "cmd") == [literal]
