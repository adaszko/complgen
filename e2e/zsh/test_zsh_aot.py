import os
import string
import tempfile
from pathlib import Path

import pytest
from hypothesis import given, settings
from hypothesis.strategies import text

from conftest import set_working_dir, gen_grammar_zsh_capture_script_path, get_zsh_capture_script_sorted_lines
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


def get_sorted_aot_completions(complgen_binary_path: Path, grammar: str, input: str) -> list[str]:
    with gen_grammar_zsh_capture_script_path(complgen_binary_path, grammar) as capture_zsh_path:
        return get_zsh_capture_script_sorted_lines(capture_zsh_path, input)


def test_zsh_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted(['rm rm     -- Remove a project', "remote remote -- Manage a project's remotes"])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'mygrep ') == sorted([
        '--color --color  -- use markers to highlight the matching strings',
        '--colour --colour -- use markers to highlight the matching strings',
    ])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd {{{ echo -e "completion\tdescription" }}};
'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ')]
    assert actual == sorted([['completion', 'completion', '--', 'description']])


def test_completes_paths(complgen_binary_path: Path):
    GRAMMAR = r'''cmd <PATH> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path('?[^a]*{foo,*bar}').write_text('dummy')
            os.mkdir('dir with spaces')
            assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted([
                r'\?\[\^a\]\*\{foo,\*bar\}',
                r'filename\ with\ spaces',
                r'dir\ with\ spaces',
            ])


def test_completes_directories(complgen_binary_path: Path):
    GRAMMAR = '''cmd <DIRECTORY> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir('?[^a]*{foo,*bar}')
            Path('filename with spaces').write_text('dummy')
            assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted([
                r'\?\[\^a\]\*\{foo,\*bar\}',
                r'dir\ with\ spaces',
            ])


def test_completes_file_with_spaces(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH>;'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('file with spaces').write_text('dummy')
            assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted(['file\\ with\\ spaces'])


def test_specializes_for_zsh(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@zsh> ::= {{{ compadd zsh }}};'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted(['zsh'])


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r'''
cargo [+<toolchain>] [<COMMAND>];
<toolchain> ::= {{{ echo toolchain }}};
<COMMAND> ::= t "Run the tests" <TESTNAME>;
<TESTNAME> ::= {{{ echo testname }}};
'''

    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cargo t ') == sorted(['testname'])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cargo +stable-aarch64-apple-darwin ') == sorted(['foo'])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cargo +')]
    assert actual == sorted([
        ['+stable-aarch64-apple-darwin', 'stable-aarch64-apple-darwin'],
        ['+stable-x86_64-apple-darwin', 'stable-x86_64-apple-darwin'],
    ])


def test_completes_strace_expr(complgen_binary_path: Path):
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR, 'strace -e ')]
    assert actual == sorted([
        ['%file', '%file'],
        ['!', '!'],
        ['all', 'all'],
        ['fault', 'fault'],
        ['file', 'file'],
        ['read', 'read'],
        ['trace', 'trace'],
        ['write', 'write'],
    ])


def test_completes_lsof_filter(complgen_binary_path: Path):
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR, 'lsf -sTCP:')]
    assert actual == sorted([
        ['-sTCP:LISTEN', 'LISTEN'],
        ['-sTCP:CLOSED', 'CLOSED'],
        ['-sTCP:^', '^'],
    ])


def test_subword_completes_only_not_entered_yet(complgen_binary_path: Path):
    GRAMMAR = r'''mygrep --color=(always | never | auto);'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'mygrep --color=') == sorted(['--color=always always', '--color=never never ', '--color=auto auto  '])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --option=')]
    assert actual == sorted([
        ['--option=arg1', '--option=arg1', '--', 'descr1'],
        ['--option=arg2', '--option=arg2', '--', 'descr2'],
    ])

def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={{{ echo -e "argument\tdescription" }}};'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --option=')]
    assert actual == sorted([
        ['--option=argument', 'argument', '--', 'description'],
    ])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@zsh> ::= {{{ echo zsh }}};
'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --option=')]
    assert actual == sorted([['--option=zsh']])

def test_description_special_characters(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option "$f\"\\";
'''
    actual = [s.split() for s in get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --')]
    assert actual == sorted([['--option', '--option', '--', '$f\"\\']])


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r'''cmd (foo || --bar);'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == sorted(['foo'])


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r'''cmd (foo || --bar);'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --') == sorted(['--bar'])


def test_subword_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(primary || secondary);'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --option=') == sorted(['--option=primary primary'])


def test_subword_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(primary || secondary);'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --option=sec') == sorted(['--option=secondary secondary'])


def test_subword_fallback_bug(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd (--color=<WHEN> || --color <WHEN> | --colour=<WHEN> | --colour <WHEN>);
<WHEN> ::= always | never | auto;
'''
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd --colour') == sorted(['--colour', '--colour= --colour='])


LITERALS_ALPHABET = string.ascii_letters + ':='
@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10, deadline=None)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = '''cmd {};'''.format(literal)
    assert get_sorted_aot_completions(complgen_binary_path, GRAMMAR, 'cmd ') == [literal]
