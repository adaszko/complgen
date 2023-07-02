import sys
import tempfile
import subprocess
from pathlib import Path


def get_fish_completion_script(complgen_binary_path: Path, grammar: str) -> bytes:
    completed_process = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--fish-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    return completed_process.stdout


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t', maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


def get_sorted_completions(complgen_binary_path: Path, grammar: str) -> list[tuple[str, str]]:
    with tempfile.NamedTemporaryFile() as f:
        fish_script = get_fish_completion_script(complgen_binary_path, grammar)
        f.write(fish_script)
        f.flush()
        completed_process = subprocess.run(['fish', '--private', '--no-config', '--init-command', 'function fish_prompt; end', '--command', 'source {}; _cmd "cmd "'.format(f.name)], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
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

    completions = get_sorted_completions(complgen_binary_path, GRAMMAR)
    assert completions == sorted([('rm', "Remove a project"), ('remote', "Manage a project's remotes")], key=lambda pair: pair[0])


def test_fish_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    completions = get_sorted_completions(complgen_binary_path, GRAMMAR)
    assert completions == sorted([('--color', "use markers to highlight the matching strings"), ('--colour', "use markers to highlight the matching strings")], key=lambda pair: pair[0])


def test_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = '''
cmd { echo -e "completion\tdescription" };
'''
    completions = get_sorted_completions(complgen_binary_path, GRAMMAR)
    assert completions == [('completion', 'description')]
