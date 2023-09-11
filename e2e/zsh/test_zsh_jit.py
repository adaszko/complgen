import os
import sys
import tempfile
import subprocess
from pathlib import Path


from conftest import set_working_dir
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


def get_jit_zsh_completions_expr(complgen_binary_path: Path, grammar: str, completed_word_index: int, words_before_cursor: list[str]) -> str:
    process = subprocess.run([complgen_binary_path, 'complete', '-', 'zsh', '--', str(completed_word_index)] + words_before_cursor, input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    return process.stdout.decode()


def test_jit_completes_paths_zsh(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path('?[^a]*{foo,*bar}').write_text('dummy')
            os.mkdir('dir with spaces')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <PATH> [--help];''', 0, [])
            assert expr.splitlines() == [
                r'local -a completions=("\\?\\[\\^a\\]\\*\\{foo,\\*bar\\}" "dir\\ with\\ spaces" "filename\\ with\\ spaces")',
                '''compadd -Q -S '' -a completions'''
            ]


def test_jit_completes_directories_zsh(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir('?[^a]*{foo,*bar}')
            Path('filename with spaces').write_text('dummy')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <DIRECTORY> [--help];''', 0, [])
            assert expr.splitlines() == [
                r'local -a completions=("\\?\\[\\^a\\]\\*\\{foo,\\*bar\\}" "dir\\ with\\ spaces")',
                '''compadd -Q -S '' -a completions'''
            ]


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('subdir')
            (Path('subdir') / 'file.txt').write_text('dummy')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <PATH>;''', 0, ['subdir/'])
            assert expr.splitlines() == [
                r'local -a completions=("subdir/file.txt")',
                '''compadd -Q -S '' -a completions'''
            ]



def test_jit_specializes_for_zsh(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { compadd zsh };''', 0, [])
    assert expr == '''local -a completions=("zsh")\ncompadd -Q -S '' -a completions\n'''


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 1, ['+stable-aarch64-apple-darwin'])
    assert expr == '''local -a completions=("foo")\ncompadd -Q -S '' -a completions\n'''


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['+'])
    assert expr == '''local -a completions=("+stable-aarch64-apple-darwin" "+stable-x86_64-apple-darwin")\nlocal -a descriptions=("stable-aarch64-apple-darwin" "stable-x86_64-apple-darwin")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, STRACE_EXPR_GRAMMAR, 1, ['-e', 'trace='])
    assert expr == '''local -a completions=("trace=!" "trace=%file" "trace=all" "trace=file")\nlocal -a descriptions=("!" "%file" "all" "file")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, LSOF_FILTER_GRAMMAR, 0, ['-sTCP:'])
    assert expr == '''local -a completions=("-sTCP:CLOSED" "-sTCP:LISTEN" "-sTCP:^")\nlocal -a descriptions=("CLOSED" "LISTEN" "^")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("--option=arg1" "--option=arg2")\nlocal -a descriptions=("arg1 -- descr1" "arg2 -- descr2")\ncompadd -l -Q -S '' -d descriptions -a completions\n'''


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("--option=argument")\nlocal -a descriptions=("argument -- description")\ncompadd -l -Q -d descriptions -a completions\n'''


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@zsh> ::= { echo zsh };
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("zsh")\ncompadd -Q -S '' -a completions\n'''
