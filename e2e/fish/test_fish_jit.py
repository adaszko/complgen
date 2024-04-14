import os
import tempfile
from pathlib import Path
from typing import Optional

from conftest import set_working_dir, gen_fish_jit_completion_script_path, get_sorted_fish_completions
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def get_sorted_jit_fish_completions(complgen_binary_path: Path, grammar: str, words_before_cursor: list[str] = [], prefix: Optional[str] = None) -> list[tuple[str, str]]:
    with gen_fish_jit_completion_script_path(complgen_binary_path, grammar) as completions_file_path:
        if words_before_cursor and prefix:
            args = '{} {}'.format(' '.join(words_before_cursor), prefix)
        elif words_before_cursor:
            args = '{} '.format(' '.join(words_before_cursor))
        elif prefix:
            args = f'{prefix}'
        else:
            args = ''
        input = f'__complgen_jit {args}'
        return get_sorted_fish_completions(completions_file_path, input)


def test_jit_completes_paths_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path(SPECIAL_CHARACTERS).write_text('dummy')
            os.mkdir('dir with spaces')
            expected = sorted([(SPECIAL_CHARACTERS, ''), ('filename with spaces', ''), ('dir with spaces/', '')])
            actual = get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR)
            assert actual == expected


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH>;'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('subdir')
            (Path('subdir') / 'file.txt').write_text('dummy')
            assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='subdir/') == sorted([('subdir/file.txt', '')])


def test_jit_completes_directories_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <DIRECTORY> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir(SPECIAL_CHARACTERS)
            Path('filename with spaces').write_text('dummy')
            assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR) == sorted([(SPECIAL_CHARACTERS + '/', 'Directory'), ('dir with spaces/', 'Directory')])


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
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='--option=') == sorted([('--option=fish', '')])

def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r'''
mygrep (--color=<WHEN> || --colour=<WHEN>);
<WHEN> ::= always | never | auto;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='') == sorted([('--color=', '')])


def test_falls_back_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r'''
mygrep (--color=<WHEN> || --colour=<WHEN>);
<WHEN> ::= always | never | auto;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, prefix='--colou') == sorted([('--colour=', '')])
