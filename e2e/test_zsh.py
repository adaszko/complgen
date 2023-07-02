import os
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

from conftest import set_working_dir


def zsh_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t', maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


def get_sorted_completions(input: str) -> list[tuple[str, str]]:
    zsh_process = subprocess.run(['zsh'], input=input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    completions = zsh_process.stdout.decode()
    parsed = zsh_completions_from_stdout(completions)
    parsed.sort(key=lambda pair: pair[0])
    return parsed


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    completion_script = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--zsh-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    COMPADD_INTERCEPT_WRAPPER = '''
declare -a wrapper_completions=()
declare -a wrapper_descriptions=()

compadd () {
    while (( $# )); do
        case $1 in
            --)
                shift
                while (( $# )); do
                    wrapper_completions+=($1)
                    shift
                done
                break
            ;;

            -a)
                shift
                wrapper_completions+=(${(P)1})
                shift
            ;;

            -d)
                shift
                wrapper_descriptions+=(${(P)1})
                shift
            ;;

            *)
                break
            ;;
        esac
    done
}

'''

    with tempfile.NamedTemporaryFile() as f:
        f.write(COMPADD_INTERCEPT_WRAPPER.encode())
        f.write(completion_script)
        f.flush()
        yield Path(f.name)


def test_zsh_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; words=(cmd); CURRENT=2; _cmd; for i in {{1..$#wrapper_completions}}; do printf "%s\t%s\n" ${{wrapper_completions[$i]}} ${{wrapper_descriptions[$i]}}; done'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('rm', "rm (Remove a project)"), ('remote', "remote (Manage a project's remotes)")], key=lambda pair: pair[0])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; words=(mygrep); CURRENT=2; _mygrep; for i in {{1..$#wrapper_completions}}; do printf "%s\t%s\n" ${{wrapper_completions[$i]}} ${{wrapper_descriptions[$i]}}; done'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('--color', "--color (use markers to highlight the matching strings)"), ('--colour', "--colour (use markers to highlight the matching strings)")], key=lambda pair: pair[0])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; words=(cmd); CURRENT=2; _cmd; for i in {{1..$#wrapper_completions}}; do printf "%s\t%s\n" ${{wrapper_completions[$i]}} ${{wrapper_descriptions[$i]}}; done'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('completion', 'description')])
