import os
import sys
import tempfile
import subprocess
import contextlib
from pathlib import Path
from typing import Generator

from conftest import set_working_dir
from common import LSOF_FILTER_GRAMMAR, STRACE_EXPR_GRAMMAR


def fish_completions_from_stdout(stdout: str) -> list[tuple[str, str]]:
    result = []
    for line in stdout.splitlines():
        fields = line.split('\t', maxsplit=2)
        if len(fields) == 1:
            result.append((fields[0], ''))
        else:
            result.append((fields[0], fields[1]))
    return result


@contextlib.contextmanager
def completion_script_path(complgen_binary_path: Path, grammar: str) -> Generator[Path, None, None]:
    fish_script = subprocess.run([complgen_binary_path, 'compile', '--fish-script', '-', '-'], input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True).stdout
    with tempfile.NamedTemporaryFile() as f:
        f.write(fish_script)
        f.flush()
        yield Path(f.name)


def get_sorted_completions(input: str) -> list[tuple[str, str]]:
    completed_process = subprocess.run(['fish', '--private', '--no-config', '--command', input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    completions = completed_process.stdout.decode()
    parsed = fish_completions_from_stdout(completions)
    parsed.sort(key=lambda pair: pair[0])
    return parsed


def test_fish_uses_correct_description_with_duplicated_literals(complgen_binary_path: Path):
    GRAMMAR = '''
cmd <COMMAND> [--help];

<COMMAND> ::= rm           "Remove a project" <RM-OPTION>
            | remote       "Manage a project's remotes" [<REMOTE-SUBCOMMAND>]
            ;

<REMOTE-SUBCOMMAND> ::= rm <name>;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('rm', "Remove a project"), ('remote', "Manage a project's remotes")], key=lambda pair: pair[0])


def test_fish_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == sorted([('--color', "use markers to highlight the matching strings"), ('--colour', "use markers to highlight the matching strings")], key=lambda pair: pair[0])


def test_fish_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == [('completion', 'description')]


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def test_completes_paths(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path(SPECIAL_CHARACTERS).write_text('dummy')
                os.mkdir('dir with spaces')
                input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
                completions = get_sorted_completions(input)
                assert completions == sorted([(SPECIAL_CHARACTERS, ''), ('dir with spaces/', ''), ('filename with spaces', '')])


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir(SPECIAL_CHARACTERS)
                Path('baz').write_text('dummy')
                input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
                completions = get_sorted_completions(input)
                assert completions == sorted([(SPECIAL_CHARACTERS + '/', 'Directory'), ('dir with spaces/', 'Directory')])


def get_sorted_jit_fish_completions(complgen_binary_path: Path, grammar: str, completed_word_index: int, words_before_cursor: list[str]) -> list[tuple[str, str]]:
    process = subprocess.run([complgen_binary_path, 'complete', '-', 'fish', '--', str(completed_word_index)] + words_before_cursor, input=grammar.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    parsed = fish_completions_from_stdout(process.stdout.decode())
    return sorted(parsed, key=lambda pair: pair[0])


def test_jit_completes_paths_fish(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            Path('filename with spaces').write_text('dummy')
            Path(SPECIAL_CHARACTERS).write_text('dummy')
            os.mkdir('dir with spaces')
            assert get_sorted_jit_fish_completions(complgen_binary_path, '''cmd <PATH> [--help];''', 0, []) == sorted([(SPECIAL_CHARACTERS, ''), ('filename with spaces', ''), ('dir with spaces/', '')])


def test_jit_completes_directories_fish(complgen_binary_path: Path):
    with tempfile.TemporaryDirectory() as dir:
        with set_working_dir(Path(dir)):
            os.mkdir('dir with spaces')
            os.mkdir(SPECIAL_CHARACTERS)
            Path('filename with spaces').write_text('dummy')
            assert get_sorted_jit_fish_completions(complgen_binary_path, '''cmd <DIRECTORY> [--help];''', 0, []) == sorted([(SPECIAL_CHARACTERS + '/', 'Directory'), ('dir with spaces/', 'Directory')])


def test_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@fish> ::= { echo fish };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cmd --do-complete "cmd "'.format(completions_file_path)
        assert get_sorted_completions(input) == [('fish', '')]


def test_jit_specializes_for_fish(complgen_binary_path: Path):
    GRAMMAR = '''cmd <FOO>; <FOO> ::= { echo foo }; <FOO@fish> ::= { echo fish };'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, []) == sorted([('fish', '')])


def test_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
cargo test --test testname;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cargo --do-complete "cargo +stable-aarch64-apple-darwin "'.format(completions_file_path)
        completions = get_sorted_completions(input)
        assert completions == sorted([('foo', '')])


def test_jit_matches_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain> foo;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 1, ['+stable-aarch64-apple-darwin']) == sorted([('foo', '')])


def test_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cargo --do-complete "cargo +"'.format(completions_file_path)
        completions = get_sorted_completions(input)
        assert completions == sorted([('+stable-aarch64-apple-darwin', ''), ('+stable-x86_64-apple-darwin', '')])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, ['+']) == sorted([('stable-aarch64-apple-darwin', ''), ('stable-x86_64-apple-darwin', '')])


def test_completes_strace_expr(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cargo --do-complete "strace -e "'.format(completions_file_path)
        completions = get_sorted_completions(input)
        assert completions == sorted([('!', ''), ('%file', ''), ('file', ''), ('all', ''), ('read', ''), ('trace', ''), ('write', ''), ('fault', '')])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR, 1, ['-e', 'trace=']) == sorted([('!', ''), ('%file', ''), ('file', ''), ('all', '')])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR) as completions_file_path:
        input = 'source {}; complete --command cargo --do-complete "lsf "'.format(completions_file_path)
        completions = get_sorted_completions(input)
        assert completions == sorted([('-s', '')])


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR, 0, ['-sTCP:']) == sorted([('^', ''), ('LISTEN', ''), ('CLOSED', '')])
