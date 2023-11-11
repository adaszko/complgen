import os
import sys
import tempfile
import subprocess
from pathlib import Path
from typing import Optional

from conftest import set_working_dir
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def get_sorted_jit_bash_completions(complgen_binary_path: Path, grammar: str, words_before_cursor: list[str] = [], prefix: Optional[str] = None) -> list[str]:
    args = [complgen_binary_path, 'complete', '-', 'bash']
    if prefix is not None:
        args += ['--prefix={}'.format(prefix)]
    args += ['--']
    args += words_before_cursor
    process = subprocess.run(args, input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    lines = process.stdout.decode().splitlines()
    return sorted(lines)


def test_jit_completes(complgen_binary_path: Path):
    GRAMMAR = '''cmd (--help | --version); '''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR) == sorted(['--help', '--version'])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, ['+stable-aarch64-apple-darwin']) == sorted(['foo'])


def test_jit_completes_literal_prefix(complgen_binary_path: Path):
    GRAMMAR = '''cmd (--help | --version);'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='--h') == sorted(['--help'])

def test_jit_completes_paths_bash(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path(SPECIAL_CHARACTERS).write_text('dummy')
            os.mkdir('dir with spaces')
            assert get_sorted_jit_bash_completions(complgen_binary_path, '''cmd <PATH> [--help];''') == sorted(['filename with spaces', SPECIAL_CHARACTERS, 'dir with spaces'])


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('subdir')
            (Path('subdir') / 'file.txt').write_text('dummy')
            assert get_sorted_jit_bash_completions(complgen_binary_path, '''cmd <PATH>;''', prefix='subdir/') == sorted(['subdir/file.txt'])


def test_jit_completes_directories_bash(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir(SPECIAL_CHARACTERS)
            Path('filename with spaces').write_text('dummy')
            assert get_sorted_jit_bash_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') == sorted(['dir with spaces', SPECIAL_CHARACTERS])


def test_jit_specializes_for_bash(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@bash> ::= {{{ echo bash }}};'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR) == sorted(['bash'])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='+') == sorted(['+stable-aarch64-apple-darwin', '+stable-x86_64-apple-darwin'])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    assert get_sorted_jit_bash_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR, ['-e'], prefix='trace=') == sorted(['!', '%file', 'file', 'all'])


def test_jit_completes_lsf_filter(complgen_binary_path: Path):
    assert get_sorted_jit_bash_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR, prefix='-sTCP:') == sorted(['^', 'LISTEN', 'CLOSED'])


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted(['arg1', 'arg2'])


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={{{ echo -e "argument\tdescription" }}};'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted(['argument'])


def test_jit_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@bash> ::= {{{ echo bash }}};
'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted(['bash'])


def test_github_issue_34(complgen_binary_path: Path):
    GRAMMAR = r'''
mygrep --color "use markers to highlight the matching strings"=<WHEN>;
<WHEN> ::= always | never | auto;
'''
    assert get_sorted_jit_bash_completions(complgen_binary_path, GRAMMAR, prefix='--color') == sorted(['--color='])
