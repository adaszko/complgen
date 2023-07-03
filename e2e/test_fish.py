import os
import sys
import tempfile
import subprocess
import contextlib
from pathlib import Path
from typing import Generator

from conftest import set_working_dir


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t', maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    fish_script = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--fish-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(fish_script)
        f.flush()
        yield Path(f.name)


def get_sorted_completions(input: str) -> list[tuple[str, str]]:
    completed_process = subprocess.run(['fish', '--private', '--no-config', '--command', input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    completions = completed_process.stdout.decode()
    parsed = fish_completions_from_stdout(completions)
    parsed.sort(key=lambda pair: pair[0])
    return parsed


def test_fish_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('rm', "Remove a project"), ('remote', "Manage a project's remotes")], key=lambda pair: pair[0])


def test_fish_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('--color', "use markers to highlight the matching strings"), ('--colour', "use markers to highlight the matching strings")], key=lambda pair: pair[0])


def test_fish_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == [('completion', 'description')]


def test_completes_files(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <FILE> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
                completions = get_sorted_completions(input)
                assert completions == sorted([('bar', ''), ('baz/', ''), ('foo', '')])

    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
                completions = get_sorted_completions(input)
                assert completions == sorted([('bar', ''), ('baz/', ''), ('foo', '')])
