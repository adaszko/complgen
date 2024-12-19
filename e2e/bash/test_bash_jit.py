import os
import string
import tempfile
from pathlib import Path

import pytest
from hypothesis import given, settings
from hypothesis.strategies import text

from conftest import complgen_binary_path, set_working_dir, gen_bash_jit_completions_script_path, get_sorted_bash_completions
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def test_jit_completes(complgen_binary_path: Path):
    GRAMMAR = '''cmd (--help | --version); '''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--help', '--version'])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, ['+stable-aarch64-apple-darwin']) as completion_script:
        input = r'''COMP_WORDS=(cargo +stable-aarch64-apple-darwin); COMP_CWORD=2; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['foo'])


def test_jit_completes_literal_prefix(complgen_binary_path: Path):
    GRAMMAR = '''cmd (--help | --version);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--h') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--help'])


def test_jit_completes_paths_bash(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path(SPECIAL_CHARACTERS).write_text('dummy')
            os.mkdir('dir with spaces')
            with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
                input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
                assert get_sorted_bash_completions(completion_script, input) == sorted(['filename with spaces', SPECIAL_CHARACTERS, 'dir with spaces'])


def test_jit_completes_subdirectory_files(complgen_binary_path: Path):
    GRAMMAR = '''cmd <PATH>;'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('subdir')
            (Path('subdir') / 'file.txt').write_text('dummy')
            with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='subdir/') as completion_script:
                input = r'''COMP_WORDS=(cmd subdir/); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
                assert get_sorted_bash_completions(completion_script, input) == sorted(['subdir/file.txt'])


def test_jit_completes_directories_bash(complgen_binary_path: Path):
    GRAMMAR = '''cmd <DIRECTORY> [--help];'''
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir(SPECIAL_CHARACTERS)
            Path('filename with spaces').write_text('dummy')
            with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
                input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
                assert get_sorted_bash_completions(completion_script, input) == sorted(['dir with spaces', SPECIAL_CHARACTERS])


def test_jit_specializes_for_bash(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= {{{ echo foo }}}; <FOO@bash> ::= {{{ echo bash }}};'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['bash'])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='+') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['+stable-aarch64-apple-darwin', '+stable-x86_64-apple-darwin'])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    with gen_bash_jit_completions_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR, ['-e'], prefix='trace=') as completion_script:
        input = r'''COMP_WORDS=(strace); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['!', '%file', 'file', 'all'])


def test_jit_completes_lsf_filter(complgen_binary_path: Path):
    with gen_bash_jit_completions_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR, prefix='-sTCP:') as completion_script:
        input = r'''COMP_WORDS=(lsf); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['^', 'LISTEN', 'CLOSED'])


def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['arg1', 'arg2'])


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={{{ echo -e "argument\tdescription" }}};'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['argument'])


def test_jit_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= {{{ echo generic }}};
<FOO@bash> ::= {{{ echo bash }}};
'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['bash'])


def test_github_issue_34(complgen_binary_path: Path):
    GRAMMAR = r'''
mygrep --color "use markers to highlight the matching strings"=<WHEN>;
<WHEN> ::= always | never | auto;
'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--color') as completion_script:
        input = r'''COMP_WORDS=(mygrep); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--color='])


def test_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r'''cmd (foo || --bar);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['foo'])


def test_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r'''cmd (foo || --bar);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--bar'])


def test_subword_fallback_completes_default(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(primary || secondary);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['primary'])


def test_subword_fallbacks_on_no_matches(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(primary || secondary);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=sec') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['secondary'])


def test_respects_ignore_case_option(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --case-lower | --CASE-UPPER;'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--case-') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; bind "set completion-ignore-case on"; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--case-lower', '--CASE-UPPER'])
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--case-') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; bind "set completion-ignore-case off"; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['--case-lower'])


def test_respects_ignore_case_option_subwords(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(lower | UPPER);'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=LO') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; bind "set completion-ignore-case on"; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['lower'])
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR, prefix='--option=LO') as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; bind "set completion-ignore-case off"; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted([''])


def test_funky_spec_command_name(complgen_binary_path: Path):
    GRAMMAR = r'''// <NONTERM>; <NONTERM@bash> ::= {{{ echo dummy }}};'''
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
        input = r'''COMP_WORDS=(//); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == sorted(['dummy'])


LITERALS_ALPHABET = string.ascii_letters + ':='
@given(text(LITERALS_ALPHABET, min_size=1))
@settings(max_examples=10)
def test_handles_special_characters(complgen_binary_path: Path, literal: str):
    GRAMMAR = '''cmd {};'''.format(literal)
    with gen_bash_jit_completions_script_path(complgen_binary_path, GRAMMAR) as completion_script:
        input = r'''COMP_WORDS=(cmd); COMP_CWORD=1; __complgen_jit; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(completion_script, input) == [literal]
