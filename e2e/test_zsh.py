import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path

import pytest


def get_completion_script(complgen_binary_path: Path, grammar: str) -> str:
    completed_process = subprocess.run([complgen_binary_path, 'compile', '--test-mode', '--zsh-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr)
    return completed_process.stdout.decode()


def zsh_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t')
        assert len(fields) in (1, 2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


@contextlib.contextmanager
def wrapper_script_path(completions_script: str) -> Path:
    COMPADD_INTERCEPT_SCRIPT = '''
typeset -a completions=()
typeset -a descriptions=()

compadd () {
    while (( $# )); do
        case $1 in
            --)
                shift
                while (( $# )); do
                    completions+=($1)
                    shift
                done
                break
            ;;

            -a)
                shift
                completions+=(${(P)1})
                shift
            ;;

            -d)
                shift
                descriptions+=(${(P)1})
                shift
            ;;

            *)
                break
            ;;
        esac
    done
}

'''

    with tempfile.NamedTemporaryFile(mode='w') as f:
        f.write(COMPADD_INTERCEPT_SCRIPT)
        f.write(completions_script)
        f.flush()
        yield f.name



def test_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = '''
cmd { echo -e "completion\tdescription" };
'''
    completion_script = get_completion_script(complgen_binary_path, GRAMMAR)
    with wrapper_script_path(completion_script) as wrapper_path:
        input = 'source {}; words=(cmd); CURRENT=2; _cmd; print -l -- $completions'.format(wrapper_path)
        zsh_process = subprocess.run(['zsh'], input=input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr)
        completions = zsh_process.stdout.decode()
        parsed = zsh_completions_from_stdout(completions)
        assert parsed == [('completion', 'description')]
