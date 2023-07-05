import os
import sys
import stat
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

from conftest import set_working_dir


def get_sorted_completions(capture_zsh_path: Path, input: str) -> list[tuple[str, str]]:
    zsh_process = subprocess.run([capture_zsh_path, input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    stdout = zsh_process.stdout.decode()
    completions = []
    for line in stdout.splitlines():
        fields = line.split(' -- ')
        if len(fields) == 2:
            completions.append((fields[0], fields[1]))
        elif len(fields) == 1:
            completions.append((fields[0], ''))
        else:
            assert False
    completions.sort(key=lambda pair: pair[0])
    return completions


@contextlib.contextmanager
def custom_capture_zsh(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    completion_script = subprocess.run([complgen_binary_path, 'compile', '--zsh-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(Path('capture_zsh_beginning.zsh').read_bytes())
        f.write("\n".encode())
        f.write(completion_script.decode().replace("'", "''").encode())
        f.write("\n".encode())
        f.write(Path('capture_zsh_end.zsh').read_bytes())
        f.flush()
        st = os.stat(f.name)
        os.chmod(f.name, st.st_mode | stat.S_IEXEC)
        yield Path(f.name)


def test_zsh_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with custom_capture_zsh(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('rm', "(Remove a project)"), ('remote', "(Manage a project's remotes)")], key=lambda pair: pair[0])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with custom_capture_zsh(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep ') == sorted([('--color', "(use markers to highlight the matching strings)"), ('--colour', "(use markers to highlight the matching strings)")], key=lambda pair: pair[0])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''
    with custom_capture_zsh(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('completion', 'description')])


def test_completes_files(complgen_binary_path: Path):
    with custom_capture_zsh(complgen_binary_path, '''cmd <PATH> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('foo').write_text('dummy')
                Path('bar').write_text('dummy')
                os.mkdir('baz')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('bar', ''), ('foo', ''), ('baz/', '')])


def test_completes_directories(complgen_binary_path: Path):
    with custom_capture_zsh(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('foo')
                os.mkdir('bar')
                Path('baz').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('bar/', ''), ('foo/', '')])
