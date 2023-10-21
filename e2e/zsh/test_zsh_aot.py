import re
import os
import sys
import pty
import termios
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

from conftest import set_working_dir, capture_script_path
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


def get_sorted_completions(generated_script_path: Path, input: str) -> list[str]:
    zsh_process = subprocess.run(['zsh', generated_script_path, input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    stdout = zsh_process.stdout.decode()
    completions = stdout.splitlines()
    completions.sort()
    return completions


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
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['rm rm       Remove a project', "remote remote   Manage a project's remotes"])


def test_zsh_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep ') == sorted([
            '--color --color    use markers to highlight the matching strings',
            '--colour --colour   use markers to highlight the matching strings',
        ])


def test_zsh_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd {{{ echo -e "completion\tdescription" }}};
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cmd ')]
        assert actual == sorted([['completion', 'completion', 'description']])


def test_completes_paths(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path('?[^a]*{foo,*bar}').write_text('dummy')
                os.mkdir('dir with spaces')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([
                    r'\?\[\^a\]\*\{foo,\*bar\}',
                    r'filename\ with\ spaces',
                    r'dir\ with\ spaces',
                ])


def test_completes_directories(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir('?[^a]*{foo,*bar}')
                Path('filename with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([
                    r'\?\[\^a\]\*\{foo,\*bar\}',
                    r'dir\ with\ spaces',
                ])


def test_completes_file_with_spaces(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH>;''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('file with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['file\\ with\\ spaces'])


def test_specializes_for_zsh(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@zsh> ::= {{{ compadd zsh }}};''') as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted(['zsh'])


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r'''
cargo [<toolchain>] [<COMMAND>];
<toolchain> ::= {{{ echo toolchain }}};
<COMMAND> ::= t "Run the tests" <TESTNAME>;
<TESTNAME> ::= {{{ echo testname }}};
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo t ') == sorted(['testname'])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo +stable-aarch64-apple-darwin ') == sorted(['foo'])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cargo +')]
        assert actual == sorted([
            ['+stable-aarch64-apple-darwin', 'stable-aarch64-apple-darwin'],
            ['+stable-x86_64-apple-darwin', 'stable-x86_64-apple-darwin'],
        ])


def test_completes_in_word(complgen_binary_path: Path):
    GRAMMAR = '''
cmd prefix-infix-good;
cmd prefix-infix-bad;
'''
    completion_script = subprocess.run([complgen_binary_path, 'compile', '--zsh-script', '-', '-'], input=GRAMMAR.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout.decode()
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
                LEFT_ARROW = '\x1b[D'
                TAB = '	'
                pty_write.write('cmd prefix--good{}{}'.format(LEFT_ARROW * 5, TAB).encode())
                pty_write.flush()
                pty_write.write('\n'.encode())
                pty_write.write(b'')
                pty_write.flush()
                output = pty_read.read()
                bracketed_pastes = re.findall(rb'\x1b\[\?2004h(.*?)\x1b\[\?2004l', output, re.DOTALL)
                bracketed_pastes = [bp for bp in bracketed_pastes if bp != b'']
                bracketed_pastes = [bp.decode() for bp in bracketed_pastes]
                print(bracketed_pastes)
                assert bracketed_pastes == [
                    'autoload compinit; compinit; source {}'.format(completion_script_file.name),
                    'cmd prefix--goodinfix-good',
                ]
        finally:
            os.waitpid(pid, 0)


def test_completes_strace_expr(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'strace -e ')]
        assert actual == sorted([
            ['%file', '%file'],
            ['!', '!'],
            ['all', 'all'],
            ['fault', 'fault'],
            ['file', 'file'],
            ['read', 'read'],
            ['trace', 'trace'],
            ['write', 'write'],
        ])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'lsf -sTCP:')]
        assert actual == sorted([
            ['-sTCP:LISTEN', 'LISTEN'],
            ['-sTCP:CLOSED', 'CLOSED'],
            ['-sTCP:^', '^'],
        ])


def test_subword_completes_only_not_entered_yet(complgen_binary_path: Path):
    GRAMMAR = r'''mygrep --color=(always | never | auto);'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'mygrep --color=') == sorted(['--color=always always', '--color=never never', '--color=auto auto'])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cmd --option=')]
        assert actual == sorted([
            ['--option=arg1', '--option=arg1', 'descr1'],
            ['--option=arg2', '--option=arg2', 'descr2'],
        ])

def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={{{ echo -e "argument\tdescription" }}};'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cmd --option=')]
        assert actual == sorted([
            ['--option=argument', 'argument', 'description'],
        ])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@zsh> ::= {{{ echo zsh }}};
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cmd --option=')]
        assert actual == sorted([['--option=zsh']])

def test_description_special_characters(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option "$f\"\\";
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        actual = [s.split() for s in get_sorted_completions(capture_zsh_path, 'cmd --')]
        assert actual == sorted([['--option', '--option', '$f\"']])
