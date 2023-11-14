import os
import sys
import tempfile
import subprocess
from pathlib import Path
from typing import Optional

from conftest import set_working_dir, fish_completions_from_stdout
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def get_sorted_jit_fish_completions(complgen_binary_path: Path, grammar: str, words_before_cursor: list[str] = [], prefix: Optional[str] = None) -> list[tuple[str, str]]:
    args = [complgen_binary_path, 'jit', '-', 'fish']
    if prefix is not None:
        args += ['--prefix={}'.format(prefix)]
    args += ['--']
    args += words_before_cursor
    process = subprocess.run(args, input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    parsed = fish_completions_from_stdout(process.stdout.decode())
    return sorted(parsed, key=lambda pair: pair[0])


def test_jit_completes_paths_fish(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path(SPECIAL_CHARACTERS).write_text('dummy')
            os.mkdir('dir with spaces')
            assert get_sorted_jit_fish_completions(complgen_binary_path, '''cmd <PATH> [--help];''') == sorted([(SPECIAL_CHARACTERS, ''), ('filename with spaces', ''), ('dir with spaces/', '')])


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('subdir')
            (Path('subdir') / 'file.txt').write_text('dummy')
            assert get_sorted_jit_fish_completions(complgen_binary_path, '''cmd <PATH>;''', prefix='subdir/') == sorted([('subdir/file.txt', '')])


def test_jit_completes_directories_fish(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir(SPECIAL_CHARACTERS)
            Path('filename with spaces').write_text('dummy')
            assert get_sorted_jit_fish_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') == sorted([(SPECIAL_CHARACTERS + '/', 'Directory'), ('dir with spaces/', 'Directory')])


def test_jit_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@fish> ::= {{{ echo fish }}};'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR) == sorted([('fish', '')])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, ['+stable-aarch64-apple-darwin']) == sorted([('foo', '')])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='+') == sorted([('+stable-aarch64-apple-darwin', ''), ('+stable-x86_64-apple-darwin', '')])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR, ['-e'], prefix='trace=') == sorted([('trace=!', ''), ('trace=%file', ''), ('trace=file', ''), ('trace=all', '')])


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR, prefix='-sTCP:') == sorted([('-sTCP:^', ''), ('-sTCP:LISTEN', ''), ('-sTCP:CLOSED', '')])


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted([('--option=arg1', 'descr1'), ('--option=arg2', 'descr2')])


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={{{ echo -e "argument\tdescription" }}};'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted([('--option=argument', 'description')])


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@fish> ::= {{{ echo fish }}};
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted([('fish', '')])
