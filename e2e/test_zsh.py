import os
import sys
import tempfile
import contextlib
import subprocess
from pathlib import Path
from typing import Generator

from conftest import set_working_dir
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


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
    zsh_process = subprocess.run(['zsh', generated_script_path, input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
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
                Path('filename with spaces').write_text('dummy')
                Path('?[^a]*{foo,*bar}').write_text('dummy')
                os.mkdir('dir with spaces')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([(r'\?\[\^a\]\*\{foo,\*bar\}', ''), (r'filename\ with\ spaces', ''), (r'dir\ with\ spaces', '')])


def test_completes_directories(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir('?[^a]*{foo,*bar}')
                Path('filename with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([(r'\?\[\^a\]\*\{foo,\*bar\}', ''), (r'dir\ with\ spaces', '')])


def test_completes_file_with_spaces(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <PATH>;''') as capture_zsh_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('file with spaces').write_text('dummy')
                assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('file\\ with\\ spaces', '')])


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
                r'local -a completions=("\?\[\^a\]\*\{foo,\*bar\}" "dir\ with\ spaces" "filename\ with\ spaces")',
                r'local -a descriptions=("\?\[\^a\]\*\{foo,\*bar\}" "dir\ with\ spaces" "filename\ with\ spaces")',
                '''compadd -Q -S '' -d descriptions -a completions'''
            ]


def test_jit_completes_directories_zsh(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir('?[^a]*{foo,*bar}')
            Path('filename with spaces').write_text('dummy')
            expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <DIRECTORY> [--help];''', 0, [])
            assert expr.splitlines() == [
                r'local -a completions=("\?\[\^a\]\*\{foo,\*bar\}" "dir\ with\ spaces")',
                r'local -a descriptions=("\?\[\^a\]\*\{foo,\*bar\}" "dir\ with\ spaces")',
                '''compadd -Q -S '' -d descriptions -a completions'''
            ]


def test_specializes_for_zsh(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { compadd zsh };''') as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd ') == sorted([('zsh', '')])


def test_jit_specializes_for_zsh(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@zsh> ::= { compadd zsh };''', 0, [])
    assert expr == '''local -a completions=("zsh")\nlocal -a descriptions=("zsh")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_mycargo(complgen_binary_path: Path):
    GRAMMAR = r'''
cargo [<toolchain>] [<COMMAND>];
<toolchain> ::= { echo toolchain };
<COMMAND> ::= t "Run the tests" <TESTNAME>;
<TESTNAME> ::= { echo testname };
'''

    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo t ') == sorted([('testname', '')])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo +stable-aarch64-apple-darwin ') == sorted([('foo', '')])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 1, ['+stable-aarch64-apple-darwin'])
    assert expr == '''local -a completions=("foo")\nlocal -a descriptions=("foo")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cargo +') == sorted([('+stable-aarch64-apple-darwin', ''), ('+stable-x86_64-apple-darwin', '')])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['+'])
    assert expr == '''local -a completions=("stable-aarch64-apple-darwin" "stable-x86_64-apple-darwin")\nlocal -a descriptions=("stable-aarch64-apple-darwin" "stable-x86_64-apple-darwin")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_completes_strace_expr(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'strace -e ') == sorted([('%file', ''), ('!', ''), ('all', ''), ('fault', ''), ('file', ''), ('read', ''), ('trace', ''), ('write', '')])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, STRACE_EXPR_GRAMMAR, 1, ['-e', 'trace='])
    assert expr == '''local -a completions=("!" "%file" "all" "file")\nlocal -a descriptions=("!" "%file" "all" "file")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_completes_lsof_filter(complgen_binary_path: Path):
    with capture_grammar_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'lsf -sTCP:') == sorted([('-sTCP:LISTEN', ''), ('-sTCP:CLOSED', ''), ('-sTCP:^', '')])


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    expr = get_jit_zsh_completions_expr(complgen_binary_path, LSOF_FILTER_GRAMMAR, 0, ['-sTCP:'])
    assert expr == '''local -a completions=("CLOSED" "LISTEN" "^")\nlocal -a descriptions=("CLOSED" "LISTEN" "^")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted([('--option=arg1', 'arg1 (descr1)'), ('--option=arg2', 'arg2 (descr2)')])

def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("arg1" "arg2")\nlocal -a descriptions=("arg1 (descr1)" "arg2 (descr2)")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted([('--option=argument', 'description')])


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("argument")\nlocal -a descriptions=("argument (description)")\ncompadd -Q -S '' -d descriptions -a completions\n'''


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@zsh> ::= { echo zsh };
'''
    with capture_grammar_completions(complgen_binary_path, GRAMMAR) as capture_zsh_path:
        assert get_sorted_completions(capture_zsh_path, 'cmd --option=') == sorted([('--option=zsh', '')])


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@zsh> ::= { echo zsh };
'''
    expr = get_jit_zsh_completions_expr(complgen_binary_path, GRAMMAR, 0, ['--option='])
    assert expr == '''local -a completions=("zsh")\nlocal -a descriptions=("zsh")\ncompadd -Q -S '' -d descriptions -a completions\n'''
