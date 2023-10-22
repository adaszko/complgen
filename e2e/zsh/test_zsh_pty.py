import os
import re
import sys
import pty
import termios
import tempfile
import subprocess
from pathlib import Path

from conftest import set_working_dir


def get_ansi_bracketed_pastes(complgen_binary_path: Path, grammar: str, input: bytes) -> list[str]:
    completion_script = subprocess.run([complgen_binary_path, 'compile', '--zsh-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout.decode()
    (pid, fd) = pty.fork()
    if pid == 0:
        # We're in the child
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('.zshrc').write_text('PS1=""\nsetopt complete_in_word\n')
                os.execvpe('zsh', ['--noglobalrcs', '--interactive'], {'ZDOTDIR': dir, 'COLUMNS': '1000'})
    else:
        try:
            # We're in the parent
            attrs = termios.tcgetattr(fd)
            attrs[3] &= ~termios.ECHO
            termios.tcsetattr(fd, termios.TCSANOW, attrs)
            with tempfile.NamedTemporaryFile(mode='w') as completion_script_file:
                completion_script_file.write(completion_script)
                completion_script_file.flush()
                pty_write = os.fdopen(fd, mode='ab')
                pty_read = os.fdopen(os.dup(fd), mode='rb')
                pty_write.write('autoload compinit; compinit; source {}\n'.format(completion_script_file.name).encode())
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
                bracketed_pastes = [bp for bp in bracketed_pastes if bp != 'autoload compinit; compinit; source {}'.format(completion_script_file.name)]
        finally:
            os.waitpid(pid, 0)
        return bracketed_pastes



def test_completes_in_word(complgen_binary_path: Path):
    GRAMMAR = '''
cmd prefix-infix-good;
cmd prefix-infix-bad;
'''
    LEFT_ARROW = '\x1b[D'
    TAB = '	'
    bracketed_pastes = get_ansi_bracketed_pastes(complgen_binary_path, GRAMMAR, 'cmd prefix--good{}{}'.format(LEFT_ARROW * 5, TAB).encode())
    assert bracketed_pastes == [
        'cmd prefix--goodinfix-good',
    ]
