import os
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

import pytest

from conftest import set_working_dir
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    bash_script = subprocess.run([complgen_binary_path, 'compile', '--bash-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        if os.path.exists('/opt/homebrew/etc/profile.d/bash_completion.sh'):
            f.write('source /opt/homebrew/etc/profile.d/bash_completion.sh\n'.encode())
        elif os.path.exists('/etc/bash_completion'):
            f.write('source /etc/bash_completion\n'.encode())
        else:
            assert False, "Don't know how to initialize bash completions"
        f.write(bash_script)
        f.flush()
        yield Path(f.name)


def get_sorted_completions(completions_file_path: Path, input: str) -> list[str]:
    bash_process = subprocess.run(['bash', '--noprofile', '--rcfile', completions_file_path, '-i'], input=input.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    lines = bash_process.stdout.decode().splitlines()
    lines.sort()
    return lines


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def test_completes_paths(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path(SPECIAL_CHARACTERS).write_text('dummy')
                os.mkdir('baz')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == sorted(['filename with spaces', SPECIAL_CHARACTERS, 'baz'])


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir(SPECIAL_CHARACTERS)
                Path('baz').write_text('dummy')
                completions = get_sorted_completions(completions_file_path, '''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"''')
                assert completions == sorted(['dir with spaces', SPECIAL_CHARACTERS])


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


def test_specializes_for_bash(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@bash> ::= { echo bash };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['bash'])


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r'''
mycargo test <TESTNAME>;
<TESTNAME> ::= { echo foo; echo bar };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        assert get_sorted_completions(completions_file_path, r'''COMP_WORDS=(mycargo test); COMP_CWORD=2; _mycargo; if [[ ${#COMPREPLY[@]} -gt 0 ]]; then printf '%s\n' "${COMPREPLY[@]}"; fi''') == sorted(['foo', 'bar'])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cargo +stable-aarch64-apple-darwin); COMP_CWORD=2; _cargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['foo'])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cargo +); COMP_CWORD=1; _cargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['+stable-aarch64-apple-darwin', '+stable-x86_64-apple-darwin'])


def test_completes_strace_expr(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR) as path:
        input = r'''COMP_WORDS=(strace -e trace=); COMP_CWORD=2; _strace; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['!', '%file', 'file', 'all'])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR) as path:
        input = r'''COMP_WORDS=(lsf -sTCP:); COMP_CWORD=1; _lsf; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['LISTEN', 'CLOSED', '^'])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['arg1', 'arg2'])


def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['argument'])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@bash> ::= { echo bash };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(cmd --option=); COMP_CWORD=1; _cmd; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['bash'])


def make_integration_script(complgen_binary_path: Path, usage_files_dir: Path):
    return r'''
for path in {usage_files_dir}/*.usage; do
    stem=$(basename "$path" .usage)
    eval "
_complgen_jit_$stem () {{
    local words cword
    _get_comp_words_by_ref -n = words cword
    local -a completions=(\$({complgen_binary_path} complete \"{usage_files_dir}/$stem.usage\" bash \$((COMP_CWORD - 1)) -- \${{COMP_WORDS[@]:1}}))
    local prefix="\${{COMP_WORDS[\$COMP_CWORD]}}"
    for item in "\${{completions[@]}}"; do
        if [[ \$item = "\$prefix"* ]]; then
            COMPREPLY+=("\$item")
        fi
    done
    __ltrim_colon_completions "\$prefix"
    return 0
}}
"
    complete -o nospace -F _complgen_jit_$stem "$stem"
    unset stem
done
'''.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)


@contextlib.contextmanager
def temp_usage_file_path(complgen_binary_path: Path, grammar: str, command: str) -> Generator[Path, None, None]:
    with tempfile.TemporaryDirectory() as usage_files_dir:
        (Path(usage_files_dir) / '{}.usage'.format(command)).write_text(grammar)
        with tempfile.NamedTemporaryFile() as f:
            integration_script = make_integration_script(complgen_binary_path, Path(usage_files_dir))
            f.write(integration_script.encode())
            f.flush()
            yield Path(f.name)


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= { echo foo; echo bar };
'''
    with temp_usage_file_path(complgen_binary_path, GRAMMAR, 'mycargo') as usage_file_path:
        input = r'''COMP_WORDS=(mycargo +); COMP_CWORD=1; _complgen_jit_mycargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(usage_file_path, input) == sorted(['+foo', '+bar'])


def test_shell_integration_wordbreaks_chars(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep --color "use markers to highlight the matching strings"=<WHEN>;
<WHEN> ::= always | never | auto;
'''
    with temp_usage_file_path(complgen_binary_path, GRAMMAR, 'mygrep') as usage_file_path:
        input = r'''COMP_WORDS=(mygrep --color=); COMP_CWORD=1; _complgen_jit_mygrep; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(usage_file_path, input) == sorted(['--color=always', '--color=never', '--color=auto'])


def test_mygrep_example(complgen_binary_path: Path, usage_directory_path: Path):
    GRAMMAR = (usage_directory_path / "mygrep.usage").read_text()
    with completion_script_path(complgen_binary_path, GRAMMAR) as path:
        input = r'''COMP_WORDS=(mygrep); COMP_CWORD=1; _mygrep; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_completions(path, input) == sorted(['-E', '--extended-regexp', '-F', '--fixed-strings', '-G', '--basic-regexp',
'-P', '--perl-regexp', '-e', '--regexp', '-f', '--file', '-i',
'--ignore-case', '--no-ignore-case', '-w', '--word-regexp', '-x',
'--line-regexp', '-z', '--null-data', '-s', '--no-messages', '-v',
'--invert-match', '-V', '--version', '--help', '-m', '--max-count=',
'--max-count', '-b', '--byte-offset', '-n', '--line-number',
'--line-buffered', '-H', '--with-filename', '-h', '--no-filename', '--label',
'-o', '--only-matching', '-q', '--quiet', '--silent', '--binary-files', '-a',
'--text', '-d', '--directories', '-D', '--devices', '-r', '--recursive',
'-R', '--dereference-recursive', '--include', '--exclude', '--exclude-from', '--exclude-dir', '-L', '--files-without-match', '-l',
'--files-with-matches', '-c', '--count', '-T', '--initial-tab', '-Z',
'--null', '-B', '--before-context', '-A', '--after-context', '-C',
'--context', '-', '--group-separator=', '--no-group-separator', '--color',
'--colour', '--color=', '--colour=', '-U', '--binary'])
