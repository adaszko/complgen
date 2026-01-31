import os
import string
import tempfile
from pathlib import Path

from hypothesis import given, settings
from hypothesis.strategies import text

from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR
from conftest import (
    gen_fish_aot_completion_script_path,
    get_sorted_fish_completions,
    set_working_dir,
)


def test_fish_uses_correct_description_with_duplicated_literals(
    complgen_binary_path: Path,
):
    GRAMMAR = """
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
"""

    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("rm", "Remove a project"), ("remote", "Manage a project's remotes")],
            key=lambda pair: pair[0],
        )


def test_fish_uses_correct_description_with_duplicated_descriptions(
    complgen_binary_path: Path,
):
    GRAMMAR = """
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
"""

    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [
                ("--color", "use markers to highlight the matching strings"),
                ("--colour", "use markers to highlight the matching strings"),
            ],
            key=lambda pair: pair[0],
        )


def test_fish_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd {{{ echo -e "completion\tdescription" }}};
"""

    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("completion", "description")
        ]


def test_completes_paths(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                input = 'complete --do-complete "cmd "'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("foo", ""),
                        ("bar", ""),
                        ("baz", ""),
                        ("quux", ""),
                    ]
                )


def test_completes_subword_paths(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                input = 'complete --do-complete "cmd --file="'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("--file=foo", ""),
                        ("--file=bar", ""),
                        ("--file=baz", ""),
                        ("--file=quux", ""),
                    ]
                )


def test_completes_path_prefix(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd <PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                input = 'complete --do-complete "cmd b"'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("bar", ""),
                        ("baz", ""),
                    ]
                )


def test_completes_subword_path_prefix(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd --file=<PATH>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                Path("baz").touch()
                Path("quux").touch()
                input = 'complete --do-complete "cmd --file=b"'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("--file=bar", ""),
                        ("--file=baz", ""),
                    ]
                )


def test_completes_directories(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                input = 'complete --do-complete "cmd "'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("foo/", "Directory"),
                        ("bar/", "Directory"),
                        ("baz/", "Directory"),
                        ("quux/", "Directory"),
                    ]
                )


def test_completes_subword_directories(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                input = 'complete --do-complete "cmd --dir="'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("--dir=foo/", "Directory"),
                        ("--dir=bar/", "Directory"),
                        ("--dir=baz/", "Directory"),
                        ("--dir=quux/", "Directory"),
                    ]
                )


def test_completes_directories_prefix(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd <DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                input = 'complete --do-complete "cmd b"'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("bar/", "Directory"),
                        ("baz/", "Directory"),
                    ]
                )


def test_completes_subword_directories_prefix(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd --dir=<DIRECTORY>"""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir("foo")
                os.mkdir("bar")
                os.mkdir("baz")
                os.mkdir("quux")
                input = 'complete --do-complete "cmd --dir=b"'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("--dir=bar/", "Directory"),
                        ("--dir=baz/", "Directory"),
                    ]
                )


def test_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = (
        """cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@fish> ::= {{{ echo fish }}};"""
    )
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("fish", "")
        ]


def test_specializes_for_fish_with_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd <FOO>bar; <FOO> ::= {{{ echo foo }}}; <FOO@fish> ::= {{{ echo fish }}}@fish"fish";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd fish"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("fishbar", "")
        ]


def test_nontail_matching(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd left "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("right", "")
        ]


def test_nontail_matching_alternative(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> | right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd rig"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("right", "")
        ]


def test_nontail_matching_fallback(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT> || right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd rig"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("right", "")
        ]


def test_nontail_matching_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd <LEFT>right; <LEFT> ::= {{{ echo left }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd left"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("leftright", "")
        ]


def test_nontail_completion(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo left }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("left", "")
        ]


def test_nontail_completion_subword(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo left }}}@fish"left"right;"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd left"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("leftright", "")
        ]


def test_nontail_completion_truncates_to_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo leftspam }}}@fish"left";"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("left", "")
        ]


def test_nontail_completion_subword_truncates_to_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo leftspam }}}@fish"left"right;"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd left"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("leftright", "")
        ]


def test_nontail_escapes_regex(complgen_binary_path: Path):
    GRAMMAR = """cmd {{{ echo foo }}}@fish".*" bar;"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd foo "'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("bar", "")
        ]


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cmd +<toolchain> foo;
cmd test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd +stable-aarch64-apple-darwin "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([("foo", "")])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = """
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cargo +"'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted(
            [("+stable-aarch64-apple-darwin", ""), ("+stable-x86_64-apple-darwin", "")]
        )


def test_completes_strace_expr(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, STRACE_EXPR_GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "strace -e "'
        completions = get_sorted_fish_completions(completions_file_path, input)
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
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, LSOF_FILTER_GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "lsf "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([("-s", "")])


def test_issue_59(complgen_binary_path: Path):
    GRAMMAR = """hello [ id=<ID> | foo ]...;"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "hello id=42 "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([("foo", ""), ("id=", "")])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(arg1 "descr1" | arg2 "descr2");"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("--option=arg1", "descr1"),
            ("--option=arg2", "descr2"),
        ]


def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option={{{ echo -e "argument\tdescription" }}};"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("--option=argument", "description")
        ]


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@fish> ::= {{{ echo fish }}};
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("--option=fish", "")
        ]


def test_description_special_characters(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option "$f\"\\";
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option"'
        assert get_sorted_fish_completions(completions_file_path, input) == [
            ("--option", '$f"\\')
        ]


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("foo", "")]
        )


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd (foo || --bar);"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --"'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--bar", "")]
        )


def test_subword_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--option=primary", "")]
        )


def test_subword_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r"""cmd --option=(primary || secondary);"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd --option=sec"'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--option=secondary", "")]
        )


def test_handles_quotes(complgen_binary_path: Path):
    GRAMMAR = r"""cmd <ANYTHING> baz;"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'cmd "foo bar" ' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("baz", "")]
        )


def test_bug1(complgen_binary_path: Path):
    GRAMMAR = r"""
mygrep <OPTION>...;

<OPTION> ::= --color=[<WHEN>] || --colour=[<WHEN>];

<WHEN> ::= always | never | auto;
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'mygrep --color=always --col' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--color=", "")]
        )


def test_bug2(complgen_binary_path: Path):
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, """cmd <PATH>..."""
    ) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path("foo").touch()
                Path("bar").touch()
                input = 'complete --do-complete "cmd foo "'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted(
                    [
                        ("foo", ""),
                        ("bar", ""),
                    ]
                )


def test_bug3(complgen_binary_path: Path):
    GRAMMAR = r"""
mygit (status || (--help status))...;
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'mygit status --' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--help", "")]
        )


def test_bug4(complgen_binary_path: Path):
    GRAMMAR = """cmd --pretty=(full | fuller);"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'cmd --pretty=fulle' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--pretty=fuller", "")]
        )


def test_multiple_matching_subwords(complgen_binary_path: Path):
    GRAMMAR = """cmd (--[no-]ahead-behind | --[no-]renames)"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'cmd --no-' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--no-ahead-behind", ""), ("--no-renames", "")]
        )


def test_issue_70(complgen_binary_path: Path):
    GRAMMAR = """
asdf a [ --file=<T> | --term ]...;
asdf b [ --file <T> | --term ]...;
"""
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = """complete --do-complete 'asdf b --file asdf ' """
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [("--file", ""), ("--term", "")]
        )


LITERALS_ALPHABET = string.ascii_letters + ":="


@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = """cmd {};""".format(literal)
    with gen_fish_aot_completion_script_path(
        complgen_binary_path, GRAMMAR
    ) as completions_file_path:
        input = 'complete --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted(
            [(literal, "")]
        )
