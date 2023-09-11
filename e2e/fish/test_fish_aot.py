import os
import sys
import tempfile
import subprocess
import contextlib
from pathlib import Path
from typing import Generator

from conftest import get_sorted_fish_completions, set_working_dir, fish_completions_from_stdout
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    fish_script = subprocess.run([complgen_binary_path, 'compile', '--fish-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(fish_script)
        f.flush()
        yield Path(f.name)


def test_fish_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted([('rm', "Remove a project"), ('remote', "Manage a project's remotes")], key=lambda pair: pair[0])


def test_fish_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == sorted([('--color', "use markers to highlight the matching strings"), ('--colour', "use markers to highlight the matching strings")], key=lambda pair: pair[0])


def test_fish_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [('completion', 'description')]


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def test_completes_paths(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path(SPECIAL_CHARACTERS).write_text('dummy')
                os.mkdir('dir with spaces')
                input = 'complete --command cmd --do-complete "cmd "'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted([(SPECIAL_CHARACTERS, ''), ('dir with spaces/', ''), ('filename with spaces', '')])


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir(SPECIAL_CHARACTERS)
                Path('baz').write_text('dummy')
                input = 'complete --command cmd --do-complete "cmd "'
                completions = get_sorted_fish_completions(completions_file_path, input)
                assert completions == sorted([(SPECIAL_CHARACTERS + '/', 'Directory'), ('dir with spaces/', 'Directory')])


def test_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@fish> ::= { echo fish };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_fish_completions(completions_file_path, input) == [('fish', '')]


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "cargo +stable-aarch64-apple-darwin "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([('foo', '')])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "cargo +"'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([('+stable-aarch64-apple-darwin', ''), ('+stable-x86_64-apple-darwin', '')])


def test_completes_strace_expr(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "strace -e "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([('!', ''), ('%file', ''), ('file', ''), ('all', ''), ('read', ''), ('trace', ''), ('write', ''), ('fault', '')])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "lsf "'
        completions = get_sorted_fish_completions(completions_file_path, input)
        assert completions == sorted([('-s', '')])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [('--option=arg1', 'descr1'), ('--option=arg2', 'descr2')]

def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [('--option=argument', 'description')]


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@fish> ::= { echo fish };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_fish_completions(completions_file_path, input) == [('--option=fish', '')]


def test_description_special_characters(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option "$f\"\\";
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option"'
        assert get_sorted_fish_completions(completions_file_path, input) == [('--option', '$f\"\\')]
