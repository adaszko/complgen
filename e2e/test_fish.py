import sys
import tempfile
import subprocess
from pathlib import Path

import pytest


@pytest.fixture
def complgen_binary_path():
    subprocess.run(['cargo', 'build', '--release'], cwd='..', stdout=sys.stdout, stderr=sys.stderr)
    binary_path = Path('../target/release/complgen')
    assert binary_path.exists()
    return binary_path


def get_fish_completion_script(complgen_binary_path: Path, grammar: str) -> bytes:
    completed_process = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--fish-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr)
    return completed_process.stdout


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t')
        assert len(fields) in (1, 2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


def get_fish_completions_for_grammar(complgen_binary_path: Path, grammar: str) -> list[tuple[str, str]]:
    with tempfile.NamedTemporaryFile() as f:
        fish_script = get_fish_completion_script(complgen_binary_path, grammar)
        f.write(fish_script)
        f.flush()
        completed_process = subprocess.run(['fish', '--private', '--no-config', '--init-command', 'function fish_prompt; end', '--command', 'source {}; _cmd "cmd "'.format(f.name)], stdout=subprocess.PIPE, stderr=sys.stderr)
        return fish_completions_from_stdout(completed_process.stdout.decode())


def test_duplicated_literals_with_varying_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= foo           "Foo description" <FOO-OPTION>
            | bar           "Bar description" [<BAR-SUBCOMMAND>]
            ;

<BAR-SUBCOMMAND> ::= foo <foo-arg>;
'''

    completions = get_fish_completions_for_grammar(complgen_binary_path, GRAMMAR)
    completions.sort(key=lambda pair: pair[0])
    assert completions == [('foo', 'Foo description'), ('bar', "Bar description")]
