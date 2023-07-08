import os
import sys
import stat
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

from conftest import set_working_dir


def zsh_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    completions = []
    for line in stdout.splitlines():
        fields = line.split(' -- ')
        if len(fields) == 2:
            completions.append((fields[0], fields[1]))
        elif len(fields) == 1:
            completions.append((fields[0], ''))
        else:
            assert False
    return completions


def get_sorted_completions(generated_script_path: Path, input: str) -> list[tuple[str, str]]:
    zsh_process = subprocess.run([generated_script_path, input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    stdout = zsh_process.stdout.decode()
    completions = zsh_completions_from_stdout(stdout)
    completions.sort(key=lambda pair: pair[0])
    return completions


@contextlib.contextmanager
def capture_script_path(completion_script: str) -> Generator[Path, None, None]:
    this_file = Path(__file__)
    capture_preamble_path = this_file.parent.parent / 'capture_preamble.zsh'
    capture_postamble_path = this_file.parent.parent / 'capture_postamble.zsh'
    with tempfile.NamedTemporaryFile(mode='w') as f:
        f.write(capture_preamble_path.read_text())
        f.write("\n")
        f.write(completion_script.replace("'", "''"))
        f.write("\n")
        f.write(capture_postamble_path.read_text())
        f.flush()
        st = os.stat(f.name)
        os.chmod(f.name, st.st_mode | stat.S_IEXEC)
        yield Path(f.name)


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
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('rm', "(Remove a project)"), ('remote', "(Manage a project's remotes)")], key=lambda pair: pair[0])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep ') == sorted([('--color', "(use markers to highlight the matching strings)"), ('--colour', "(use markers to highlight the matching strings)")], key=lambda pair: pair[0])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('completion', 'description')])


def test_completes_paths(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('bar', ''), ('foo', ''), ('baz/', '')])


def test_completes_directories(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('foo')
                os.mkdir('bar')
                Path('baz').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('bar/', ''), ('foo/', '')])


def get_jit_zsh_completions_expr(complgen_binary_path: Path, grammar: str, completed_word_index: int, words_before_cursor: list[str]) -> str:
    process = subprocess.run([complgen_binary_path, 'complete', '-', 'zsh', '--', str(completed_word_index)] + words_before_cursor, input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    return process.stdout.decode()


def test_jit_completes_paths_zsh(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('foo').write_text('dummy')
            Path('bar').write_text('dummy')
            os.mkdir('baz')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <PATH> [--help];''', 0, [])
            assert expr == 'local -a completions=("bar" "baz/" "foo")\nlocal -a descriptions=("bar" "baz/" "foo")\ncompadd -d descriptions -a completions\n'


def test_jit_completes_directories_zsh(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('foo')
            os.mkdir('bar')
            Path('baz').write_text('dummy')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <DIRECTORY> [--help];''', 0, [])
            assert expr == 'local -a completions=("bar" "foo")\nlocal -a descriptions=("bar" "foo")\ncompadd -d descriptions -a completions\n'


def test_specializes_for_zsh(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { echo zsh };''') as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('zsh', '')])


def test_jit_specializes_for_zsh(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { echo zsh };''', 0, [])
    assert expr == 'local -a completions=("zsh")\nlocal -a descriptions=("zsh")\ncompadd -d descriptions -a completions\n'
