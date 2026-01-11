import os
import string
import tempfile
from pathlib import Path

from hypothesis import given, settings
from hypothesis.strategies import text

from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR
from conftest import (
    gen_pwsh_completion_script_path,
    get_sorted_pwsh_completions,
    set_working_dir,
)


def test_pwsh_uses_correct_description_with_duplicated_literals(
    complgen_binary_path: Path,
):
    GRAMMAR = """
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
"""

    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == sorted(
            [("rm", "Remove a project"), ("remote", "Manage a project's remotes")],
            key=lambda pair: pair[0],
        )


def test_pwsh_uses_correct_description_with_duplicated_descriptions(
    complgen_binary_path: Path,
):
    GRAMMAR = """
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
"""

    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == sorted(
            [
                ("--color", "use markers to highlight the matching strings"),
                ("--colour", "use markers to highlight the matching strings"),
            ],
            key=lambda pair: pair[0],
        )


def test_pwsh_external_command_produces_description(complgen_binary_path: Path):
    # PowerShell uses Write-Output with tab separator for description
    GRAMMAR = r"""
cmd {{{ Write-Output "completion`tdescription" }}};
"""

    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == [("completion", "description")]


def test_completes_paths(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd ", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                assert sorted(completion_texts) == sorted(["foo", "bar", "baz", "quux"])


def test_completes_subword_paths(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd --file=", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                assert sorted(completion_texts) == sorted(
                    ["--file=foo", "--file=bar", "--file=baz", "--file=quux"]
                )


def test_completes_path_prefix(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd b", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                assert sorted(completion_texts) == sorted(["bar", "baz"])


def test_completes_subword_path_prefix(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd --file=b", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                assert sorted(completion_texts) == sorted(["--file=bar", "--file=baz"])


def test_completes_directories(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd ", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                # PowerShell may or may not add trailing slash
                expected = ["foo", "bar", "baz", "quux"]
                expected_with_slash = ["foo/", "bar/", "baz/", "quux/"]
                assert sorted(completion_texts) == sorted(expected) or sorted(
                    completion_texts
                ) == sorted(expected_with_slash)


def test_completes_subword_directories(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd --dir=", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                expected = ["--dir=foo", "--dir=bar", "--dir=baz", "--dir=quux"]
                expected_with_slash = [
                    "--dir=foo/",
                    "--dir=bar/",
                    "--dir=baz/",
                    "--dir=quux/",
                ]
                assert sorted(completion_texts) == sorted(expected) or sorted(
                    completion_texts
                ) == sorted(expected_with_slash)


def test_completes_directories_prefix(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd b", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                expected = ["bar", "baz"]
                expected_with_slash = ["bar/", "baz/"]
                assert sorted(completion_texts) == sorted(expected) or sorted(
                    completion_texts
                ) == sorted(expected_with_slash)


def test_completes_subword_directories_prefix(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                completions = get_sorted_pwsh_completions(
                    completions_file_path, "cmd --dir=b", cwd=Path(dir)
                )
                completion_texts = [c[0] for c in completions]
                expected = ["--dir=bar", "--dir=baz"]
                expected_with_slash = ["--dir=bar/", "--dir=baz/"]
                assert sorted(completion_texts) == sorted(expected) or sorted(
                    completion_texts
                ) == sorted(expected_with_slash)


def test_specializes_for_pwsh(complgen_binary_path: Path):
    GRAMMAR = """cmd <FOO>; <FOO> ::= {{{ Write-Output foo }}}; <FOO@pwsh> ::= {{{ Write-Output pwsh }}};"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == [("pwsh", "")]


def test_specializes_for_pwsh_with_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd <FOO>bar; <FOO> ::= {{{ Write-Output foo }}}; <FOO@pwsh> ::= {{{ Write-Output pwsh }}}@pwsh"pwsh";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd pwsh")
        assert completions == [("pwshbar", "")]


def test_nontail_matching(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> right; <LEFT> ::= {{{ Write-Output left }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd left ")
        assert completions == [("right", "")]


def test_nontail_matching_alternative(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> | right; <LEFT> ::= {{{ Write-Output left }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd rig")
        assert completions == [("right", "")]


def test_nontail_matching_fallback(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> || right; <LEFT> ::= {{{ Write-Output left }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd rig")
        assert completions == [("right", "")]


def test_nontail_matching_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT>right; <LEFT> ::= {{{ Write-Output left }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd left")
        assert completions == [("leftright", "")]


def test_nontail_completion(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ Write-Output left }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == [("left", "")]


def test_nontail_completion_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ Write-Output left }}}@pwsh"left"right;"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd left")
        assert completions == [("leftright", "")]


def test_nontail_completion_truncates_to_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ Write-Output leftspam }}}@pwsh"left";"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == [("left", "")]


def test_nontail_completion_subword_truncates_to_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ Write-Output leftspam }}}@pwsh"left"right;"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd left")
        assert completions == [("leftright", "")]


def test_nontail_escapes_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ Write-Output foo }}}@pwsh".*" bar;"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd foo ")
        assert completions == [("bar", "")]


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cmd +<toolchain> foo;
cmd test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd +stable-aarch64-apple-darwin "
        )
        assert completions == sorted([("foo", "")])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cargo +")
        assert completions == sorted(
            [("+stable-aarch64-apple-darwin", ""), ("+stable-x86_64-apple-darwin", "")]
        )


def test_completes_strace_expr(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, STRACE_EXPR_GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "strace -e ")
        assert completions == sorted(
            [
                ("!", ""),
                ("%file", ""),
                ("file", ""),
                ("all", ""),
                ("read", ""),
                ("trace", ""),
                ("write", ""),
                ("fault", ""),
            ]
        )


def test_completes_lsof_filter(complgen_binary_path: Path):
    with gen_pwsh_completion_script_path(
        complgen_binary_path, LSOF_FILTER_GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "lsf ")
        assert completions == sorted([("-s", "")])


def test_issue_59(complgen_binary_path: Path):
    GRAMMAR = """hello [ id=<ID> | foo ]...;"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "hello id=42 "
        )
        assert completions == sorted([("foo", ""), ("id=", "")])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(arg1 "descr1" | arg2 "descr2");"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd --option="
        )
        assert completions == [
            ("--option=arg1", "descr1"),
            ("--option=arg2", "descr2"),
        ]


def test_completes_subword_external_command(complgen_binary_path: Path):
    # PowerShell uses backtick-t for tab in double-quoted strings
    GRAMMAR = r"""cmd --option={{{ Write-Output "argument`tdescription" }}};"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd --option="
        )
        assert completions == [("--option=argument", "description")]


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ Write-Output generic }}};
<FOO@pwsh> ::= {{{ Write-Output pwsh }}};
"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd --option="
        )
        assert completions == [("--option=pwsh", "")]


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == sorted([("foo", "")])


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd --")
        assert completions == sorted([("--bar", "")])


def test_subword_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd --option="
        )
        assert completions == sorted([("--option=primary", "")])


def test_subword_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "cmd --option=sec"
        )
        assert completions == sorted([("--option=secondary", "")])


def test_handles_quotes(complgen_binary_path: Path):
    GRAMMAR = r"""cmd <ANYTHING> baz;"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, 'cmd "foo bar" '
        )
        assert completions == sorted([("baz", "")])


def test_bug1(complgen_binary_path: Path):
    GRAMMAR = r"""
mygrep <OPTION>...;

<OPTION> ::= --color=[<WHEN>] || --colour=[<WHEN>];

<WHEN> ::= always | never | auto;
"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(
            completions_file_path, "mygrep --color=always --col"
        )
        assert completions == sorted([("--color=", "")])


def test_multiple_matching_subwords(complgen_binary_path: Path):
    GRAMMAR = """cmd (--[no-]ahead-behind | --[no-]renames)"""
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd --no-")
        assert completions == sorted([("--no-ahead-behind", ""), ("--no-renames", "")])


def test_completes_literals(complgen_binary_path: Path):
    GRAMMAR = "cmd --help | --version;"
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd --h")
        assert ("--help", "") in completions


def test_completes_options_after_arg(complgen_binary_path: Path):
    GRAMMAR = "cmd <ARG> --verbose;"
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd foo --")
        assert completions == [("--verbose", "")]


LITERALS_ALPHABET = string.ascii_letters + ":="


@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = """cmd {};""".format(literal)
    with gen_pwsh_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        completions = get_sorted_pwsh_completions(completions_file_path, "cmd ")
        assert completions == sorted([(literal, "")])
