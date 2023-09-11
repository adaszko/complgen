import os
import re
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

import pytest

from conftest import set_working_dir, capture_script_path
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


def get_sorted_completions(generated_script_path: Path, input: str) -> list[str]:
    zsh_process = subprocess.run(['zsh', generated_script_path, input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    stdout = zsh_process.stdout.decode()
    completions = stdout.splitlines()
    completions.sort()
    return completions


@contextlib.contextmanager
def capture_grammar_completions(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    completion_script = subprocess.run([complgen_binary_path, 'compile', '--zsh-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout.decode()
    with capture_script_path(completion_script) as path:
        yield path


def test_zsh_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['rm rm     -- Remove a project', "remote remote -- Manage a project's remotes"])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep ') == sorted([
            '--color --color  -- use markers to highlight the matching strings',
            '--colour --colour -- use markers to highlight the matching strings',
        ])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['completion completion -- description'])


def test_completes_paths(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path('?[^a]*{foo,*bar}').write_text('dummy')
                os.mkdir('dir with spaces')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([
                    r'\?\[\^a\]\*\{foo,\*bar\}',
                    r'filename\ with\ spaces',
                    r'dir\ with\ spaces',
                ])


def test_completes_directories(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir('?[^a]*{foo,*bar}')
                Path('filename with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([
                    r'\?\[\^a\]\*\{foo,\*bar\}',
                    r'dir\ with\ spaces',
                ])


def test_completes_file_with_spaces(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH>;''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('file with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['file\\ with\\ spaces'])


def test_specializes_for_zsh(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { compadd zsh };''') as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['zsh'])


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r'''
cargo [<toolchain>] [<COMMAND>];
<toolchain> ::= { echo toolchain };
<COMMAND> ::= t "Run the tests" <TESTNAME>;
<TESTNAME> ::= { echo testname };
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo t ') == sorted(['testname testname'])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo +stable-aarch64-apple-darwin ') == sorted(['foo foo'])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo +') == sorted(['+stable-aarch64-apple-darwin +stable-aarch64-apple-darwin', '+stable-x86_64-apple-darwin +stable-x86_64-apple-darwin'])


def test_completes_strace_expr(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'strace -e ') == sorted(['%file %file', '! !', 'all all', 'fault fault', 'file file', 'read read', 'trace trace', 'write write'])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'lsf -sTCP:') == sorted(['-sTCP:LISTEN -sTCP:LISTEN', '-sTCP:CLOSED -sTCP:CLOSED', '-sTCP:^ -sTCP:^'])


@pytest.mark.skip(reason="TODO")
def test_subword_completes_only_not_entered_yet(complgen_binary_path: Path):
    GRAMMAR = r'''mygrep --color=(always | never | auto);'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep --color=') == sorted(['--color=always always', '--color=never never', '--color=auto auto'])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted(['--option=arg1 --option=arg1 -- descr1', '--option=arg2 --option=arg2 -- descr2'])

def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted(['--option=argument --option=argument -- description'])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@zsh> ::= { echo zsh };
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted(['--option=zsh --option=zsh'])

def test_description_special_characters(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option "$f\"\\";
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --') == sorted(['--option --option -- $f\"\\'])
