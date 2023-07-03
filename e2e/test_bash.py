import os
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

import pytest

from conftest import set_working_dir


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    bash_script = subprocess.run([complgen_binary_path, 'compile', '--bash-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(bash_script)
        f.flush()
        yield Path(f.name)


def get_sorted_completions(completions_file_path: Path, input: str) -> list[str]:
    bash_process = subprocess.run(['bash', '--noprofile', '--rcfile', completions_file_path, '-i'], input=input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    lines = bash_process.stdout.decode().splitlines()
    return lines


def test_completes_files(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <FILE> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'baz', 'foo']

    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'baz', 'foo']


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <DIR> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('foo')
                os.mkdir('bar')
                Path('baz').write_text('dummy')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'foo']

    with completion_script_path(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('foo')
                os.mkdir('bar')
                Path('baz').write_text('dummy')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'foo']


def test_bash_uses_correct_transition_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        assert get_sorted_completions(completions_file_path, r'''COMP_WORDS=(cmd remote); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi''') == sorted(['--help', 'rm'])
        assert get_sorted_completions(completions_file_path, r'''COMP_WORDS=(cmd rm); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi''') == sorted([])


def test_bash_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['completion'])
