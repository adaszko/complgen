import os
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path

import pytest

from conftest import set_working_dir


def get_completion_script(complgen_binary_path: Path, grammar: str) -> bytes:
    completed_process = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--bash-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    return completed_process.stdout


def bash_completions_from_stdout(stdout: str) -> list[str]:
    completions = stdout.splitlines()
    completions.sort()
    return completions


@contextlib.contextmanager
def temp_completions_file(bash_script: bytes) -> Path:
    with tempfile.NamedTemporaryFile() as completions_file:
        completions_file.write(bash_script)
        completions_file.flush()
        yield completions_file.name


def get_sorted_completions(completions_file_path: Path, bash_input: str) -> list[str]:
    bash_process = subprocess.run(['bash', '--noprofile', '--rcfile', completions_file_path, '-i'], input=bash_input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    completions = bash_process.stdout.decode()
    parsed = bash_completions_from_stdout(completions)
    parsed.sort(key=lambda pair: pair[0])
    return parsed


def test_completes_files(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FILE> [--help];'''
    bash_script = get_completion_script(complgen_binary_path, GRAMMAR)
    with temp_completions_file(bash_script) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'baz', 'foo']


@pytest.mark.skip(reason="not implemented yet")
def test_completes_paths(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH> [--help];'''
    bash_script = get_completion_script(complgen_binary_path, GRAMMAR)
    with temp_completions_file(bash_script) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == ['bar', 'baz', 'foo']


@pytest.mark.skip(reason="not implemented yet")
def test_completes_directories(complgen_binary_path: Path):
    GRAMMAR = '''cmd <DIRECTORY> [--help];'''
    bash_script = get_completion_script(complgen_binary_path, GRAMMAR)
    with temp_completions_file(bash_script) as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('foo')
                os.mkdir('bar')
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

    completion_script = get_completion_script(complgen_binary_path, GRAMMAR)
    with temp_completions_file(completion_script) as completions_file_path:
        assert get_sorted_completions(completions_file_path, r'''COMP_WORDS=(cmd remote); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi''') == sorted(['--help', 'rm'])
        assert get_sorted_completions(completions_file_path, r'''COMP_WORDS=(cmd rm); COMP_CWORD=2; _cmd; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi''') == sorted([])
