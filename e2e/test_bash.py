import contextlib
import os
import string
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Generator

from hypothesis import given, settings
from hypothesis.strategies import text

from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR
from conftest import (
    get_bash_completion_sh_path,
    get_sorted_bash_completions,
    set_working_dir,
)


@contextlib.contextmanager
def completion_script_path(
    complgen_binary_path: Path, grammar: str
) -> Generator[Path, None, None]:
    bash_script = subprocess.run(
        [complgen_binary_path, "--bash", "-", "-"],
        input=grammar.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    ).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write("source {}\n".format(get_bash_completion_sh_path()).encode())
        f.write(bash_script)
        f.flush()
        yield Path(f.name)


def test_completes_paths(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["foo", "bar", "baz", "quux"])


def test_completes_subword_paths(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd --file=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["foo", "bar", "baz", "quux"])


def test_completes_path_prefix(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd b); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["bar", "baz"])


def test_completes_subword_path_prefix(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd --file=b); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["bar", "baz"])


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["foo", "bar", "baz", "quux"])


def test_completes_subword_directories(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd --dir=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["foo", "bar", "baz", "quux"])


def test_completes_directories_prefix(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd b); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["bar", "baz"])


def test_completes_subword_directories_prefix(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd --dir=b); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["bar", "baz"])


def test_bash_uses_correct_transition_with_duplicated_literals(
    complgen_binary_path: Path,
):
    GRAMMAR = """
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
"""

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        assert get_sorted_bash_completions(
            completions_file_path,
            r"""COMP_WORDS=(cmd remote); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi""",
        ) == sorted(["--help ", "rm "])
        assert get_sorted_bash_completions(
            completions_file_path,
            r"""COMP_WORDS=(cmd rm); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi""",
        ) == sorted([])


def test_bash_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd {{{ echo -e "completion\tdescription" }}};
"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted(["completion"])


def test_specializes_for_bash(complgen_binary_path: Path):
    GRAMMAR = (
        """cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@bash> ::= {{{ echo bash }}};"""
    )
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted(["bash"])


def test_specializes_for_bash_with_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd <FOO>bar; <FOO> ::= {{{ echo foo }}}; <FOO@bash> ::= {{{ echo bash }}}@bash"bash";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd bash); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["bashbar"])


def test_nontail_matching(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> right; <LEFT> ::= {{{ echo left }}}@bash"left";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd left); COMP_CWORD=2; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["right "])


def test_nontail_matching_alternative(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> | right; <LEFT> ::= {{{ echo left }}}@bash"left";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd rig); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["right "])


def test_nontail_matching_fallback(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> || right; <LEFT> ::= {{{ echo left }}}@bash"left";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd rig); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["right "])


def test_nontail_matching_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo left }}}@bash"left";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd left); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["left"])


def test_nontail_completion(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo left }}}@bash"left";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd left); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["left"])


def test_nontail_completion_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd left{{{ echo right }}}@bash"right";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd left); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["leftright"])


def test_nontail_completion_preserves_only_regex_matches(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo foo; echo bar; echo bar2; echo baz }}}@bash"bar";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted(["bar"])


def test_nontail_completion_subword_preserves_only_regex_matches(
    complgen_binary_path: Path,
):
    GRAMMAR = """cmd left{{{ echo foo; echo bar; echo bar2; echo baz }}}@bash"bar";"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd left); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["leftbar"])


def test_nontail_escapes_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo foo }}}@bash".*" bar;"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd foo); COMP_CWORD=2; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["bar "])


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r"""
mycargo test <TESTNAME>;
<TESTNAME> ::= {{{ echo foo; echo bar }}};
"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        assert get_sorted_bash_completions(
            completions_file_path,
            r"""COMP_WORDS=(mycargo test); COMP_CWORD=2; _mycargo; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi""",
        ) == sorted(["foo", "bar"])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cargo +stable-aarch64-apple-darwin); COMP_CWORD=2; _cargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["foo "])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cargo +); COMP_CWORD=1; _cargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(
            ["+stable-aarch64-apple-darwin", "+stable-x86_64-apple-darwin"]
        )


def test_completes_strace_expr(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR) as path:
        input = r'''COMP_WORDS=(strace -e trace=); COMP_CWORD=2; _strace; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(
            ["!", "%file", "file", "all"]
        )


def test_completes_lsof_filter(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR) as path:
        input = r'''COMP_WORDS=(lsf -sTCP:); COMP_CWORD=1; _lsf; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(
            ["LISTEN", "CLOSED", "^"]
        )


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(arg1 "descr1" | arg2 "descr2");"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["arg1", "arg2"])


def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option={{{ echo -e "argument\tdescription" }}};"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["argument"])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@bash> ::= {{{ echo bash }}};
"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["bash"])


# https://github.com/adaszko/complgen/issues/41
def test_respects_ignore_case_option(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --case-lower | --CASE-UPPER;"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --case-); COMP_CWORD=1; bind "set completion-ignore-case on"; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(
            ["--case-lower ", "--CASE-UPPER "]
        )

        input = r'''COMP_WORDS=(cmd --case-); COMP_CWORD=1; bind "set completion-ignore-case off"; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["--case-lower "])


def test_respects_ignore_case_option_subwords(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(lower | UPPER);"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=LO); COMP_CWORD=1; bind "set completion-ignore-case on"; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["lower"])

        input = r'''COMP_WORDS=(cmd --option=LO); COMP_CWORD=1; bind "set completion-ignore-case off"; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted([""])


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted(["foo "])


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["--bar "])


def test_subword_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["primary"])


def test_subword_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=sec); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["secondary"])


def test_handles_quotes(complgen_binary_path: Path):
    GRAMMAR = r"""cmd <ANYTHING> baz;"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd "foo bar"); COMP_CWORD=2; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(["baz "])


def test_empty_command(complgen_binary_path: Path):
    """Bash errors out on empty function body.  It needs to be handled"""
    GRAMMAR = r"""cmd {{{}}};"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd ); COMP_CWORD=1; _cmd; printf '%s' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted([])


def test_multiple_matching_subwords(complgen_binary_path: Path):
    """Bash errors out on empty function body.  It needs to be handled"""
    GRAMMAR = """cmd (--[no-]ahead-behind | --[no-]renames)"""
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --no-); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(path, input) == sorted(
            ["--no-ahead-behind", "--no-renames"]
        )


def test_bug1(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path, """cmd <PATH>..."""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                completions = get_sorted_bash_completions(
                    completions_file_path,
                    '''COMP_WORDS=(cmd foo); COMP_CWORD=2; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
                )
                assert completions == sorted(["foo", "bar"])


def test_bug2(complgen_binary_path: Path):
    with completion_script_path(
        complgen_binary_path,
        """cmd --pretty=(full | fuller);""",
    ) as completions_file_path:
        completions = get_sorted_bash_completions(
            completions_file_path,
            '''COMP_WORDS=(cmd --pretty=fulle); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''',
        )
        assert completions == sorted(["fuller"])


LITERALS_ALPHABET = string.ascii_letters + ":="


@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = """cmd {};""".format(literal)
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = (
            r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        )
        assert get_sorted_bash_completions(path, input) == sorted([literal + " "])
