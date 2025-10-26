import os
import re
import sys
import pty
import termios
import tempfile
import platform
import subprocess
from pathlib import Path

import pytest

from conftest import set_working_dir


def get_autoloaded_completion_output(complgen_binary_path: Path, grammar: str, cmd: str, input: bytes, working_dir: Path | None = None) -> list[str]:
    completion_script = subprocess.run([complgen_binary_path, '--zsh', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout.decode()
    (pid, fd) = pty.fork()
    if pid == 0:
        # We're in the child
        def body(dir):
            (Path(dir) / 'autoload').mkdir()
            (Path(dir) / 'autoload' / '_{}'.format(cmd)).write_text(completion_script)
            Path('.zshrc').write_text('PS1=""; setopt complete_in_word; fpath=($ZDOTDIR/autoload $fpath)')
            os.execvpe('zsh', ['--noglobalrcs', '--interactive'], {'ZDOTDIR': dir, 'COLUMNS': '1000'})

        if working_dir is None:
            with tempfile.TemporaryDirectory() as dir:
                with set_working_dir(Path(dir)):
                    body(dir)
        else:
            body(working_dir)
        return [] # dummy to silence type checker
    else:
        try:
            # We're in the parent
            attrs = termios.tcgetattr(fd)
            attrs[3] &= ~termios.ECHO
            termios.tcsetattr(fd, termios.TCSANOW, attrs)
            pty_write = os.fdopen(fd, mode='ab')
            pty_read = os.fdopen(os.dup(fd), mode='rb')
            pty_write.write('autoload compinit; compinit\n'.encode())
            pty_write.flush()
            pty_write.write(input)
            pty_write.flush()
            pty_write.write('\n'.encode())
            pty_write.write(b'')
            pty_write.flush()
            output = pty_read.read()
            bracketed_pastes = re.findall(rb'\x1b\[\?2004h(.*?)\x1b\[\?2004l', output, re.DOTALL)
            bracketed_pastes = [bp for bp in bracketed_pastes if bp != b'']
            bracketed_pastes = [bp.decode() for bp in bracketed_pastes]
            bracketed_pastes = [bp for bp in bracketed_pastes if bp != 'autoload compinit; compinit']
        finally:
            os.waitpid(pid, 0)
        return bracketed_pastes



@pytest.mark.skipif(platform.system() != 'Darwin', reason='Not running on macOS')
def test_completes_in_word(complgen_binary_path: Path):
    GRAMMAR = '''
cmd prefix-infix-good;
cmd prefix-infix-bad;
'''
    LEFT_ARROW = '\x1b[D'
    TAB = '	'
    bracketed_pastes = get_autoloaded_completion_output(complgen_binary_path, GRAMMAR, 'cmd', 'cmd prefix--good{}{}'.format(LEFT_ARROW * 5, TAB).encode())
    assert bracketed_pastes == ['cmd prefix--goodinfix-good  ']


@pytest.mark.skipif(platform.system() != 'Darwin', reason='Not running on macOS')
def test_tcsh_directory_completion(complgen_binary_path: Path):
    GRAMMAR = '''cmd <DIRECTORY>;'''
    with tempfile.TemporaryDirectory() as dir:
        working_dir = Path(dir)
        with set_working_dir(working_dir):
            Path('foo/bar/baz').mkdir(parents=True)
            bracketed_pastes = get_autoloaded_completion_output(complgen_binary_path, GRAMMAR, 'cmd', b'cmd f/b/b	', working_dir=working_dir)
            assert bracketed_pastes == ['cmd f/b/boo/bar/baz/ ']


@pytest.mark.skipif(platform.system() != 'Darwin', reason='Not running on macOS')
def test_autoload(complgen_binary_path: Path):
    GRAMMAR = '''hello world;'''
    with tempfile.TemporaryDirectory() as dir:
        working_dir = Path(dir)
        with set_working_dir(working_dir):
            bracketed_pastes = get_autoloaded_completion_output(complgen_binary_path, GRAMMAR, 'hello', b'hello 	', working_dir=working_dir)
            assert bracketed_pastes == ['hello world  ']


@pytest.mark.skipif(platform.system() != 'Darwin', reason='Not running on macOS')
def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r"""
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@zsh> ::= {{{ compadd -U -- ${1}zsh }}};
"""
    bracketed_pastes = get_autoloaded_completion_output(complgen_binary_path, GRAMMAR, 'cmd', b'cmd --option=	')
    assert bracketed_pastes == ['cmd --option=zsh  ']
