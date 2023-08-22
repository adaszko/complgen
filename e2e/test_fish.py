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


def get_sorted_completions(completions_script_path: Path, input: str) -> list[tuple[str, str]]:
    completed_process = subprocess.run(['fish', '--private', '--no-config', '--init-command', 'source {}'.format(completions_script_path), '--command', input], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
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
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_completions(completions_file_path, input) == sorted([('rm', "Remove a project"), ('remote', "Manage a project's remotes")], key=lambda pair: pair[0])


def test_fish_uses_correct_description_with_duplicated_descriptions(complgen_binary_path: Path):
    GRAMMAR = '''
cmd [<OPTION>]...;

<OPTION> ::= --color    "use markers to highlight the matching strings" [<WHEN>]
           | --colour   "use markers to highlight the matching strings" [<WHEN>]
           ;
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_completions(completions_file_path, input) == sorted([('--color', "use markers to highlight the matching strings"), ('--colour', "use markers to highlight the matching strings")], key=lambda pair: pair[0])


def test_fish_external_command_produces_description(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd { echo -e "completion\tdescription" };
'''

    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_completions(completions_file_path, input) == [('completion', 'description')]


SPECIAL_CHARACTERS = '?[^a]*{foo,*bar}'


def test_completes_paths(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <PATH> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                Path('filename with spaces').write_text('dummy')
                Path(SPECIAL_CHARACTERS).write_text('dummy')
                os.mkdir('dir with spaces')
                input = 'complete --command cmd --do-complete "cmd "'
                completions = get_sorted_completions(completions_file_path, input)
                assert completions == sorted([(SPECIAL_CHARACTERS, ''), ('dir with spaces/', ''), ('filename with spaces', '')])


def test_completes_directories(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, '''cmd <DIRECTORY> [--help];''') as completions_file_path:
        with tempfile.TemporaryDirectory() as dir:
            with set_working_dir(Path(dir)):
                os.mkdir('dir with spaces')
                os.mkdir(SPECIAL_CHARACTERS)
                Path('baz').write_text('dummy')
                input = 'complete --command cmd --do-complete "cmd "'
                completions = get_sorted_completions(completions_file_path, input)
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
        input = 'complete --command cmd --do-complete "cmd "'
        assert get_sorted_completions(completions_file_path, input) == [('fish', '')]


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
        input = 'complete --command cargo --do-complete "cargo +stable-aarch64-apple-darwin "'
        completions = get_sorted_completions(completions_file_path, input)
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
        input = 'complete --command cargo --do-complete "cargo +"'
        completions = get_sorted_completions(completions_file_path, input)
        assert completions == sorted([('+stable-aarch64-apple-darwin', ''), ('+stable-x86_64-apple-darwin', '')])


def test_jit_completes_prefix(complgen_binary_path: Path):
    GRAMMAR = '''
cargo +<toolchain>;
<toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, ['+']) == sorted([('stable-aarch64-apple-darwin', ''), ('stable-x86_64-apple-darwin', '')])


def test_completes_strace_expr(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, STRACE_EXPR_GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "strace -e "'
        completions = get_sorted_completions(completions_file_path, input)
        assert completions == sorted([('!', ''), ('%file', ''), ('file', ''), ('all', ''), ('read', ''), ('trace', ''), ('write', ''), ('fault', '')])


def test_jit_completes_strace_expr(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, STRACE_EXPR_GRAMMAR, 1, ['-e', 'trace=']) == sorted([('!', ''), ('%file', ''), ('file', ''), ('all', '')])


def test_completes_lsof_filter(complgen_binary_path: Path):
    with completion_script_path(complgen_binary_path, LSOF_FILTER_GRAMMAR) as completions_file_path:
        input = 'complete --command cargo --do-complete "lsf "'
        completions = get_sorted_completions(completions_file_path, input)
        assert completions == sorted([('-s', '')])


def test_jit_completes_lsof_filter(complgen_binary_path: Path):
    assert get_sorted_jit_fish_completions(complgen_binary_path, LSOF_FILTER_GRAMMAR, 0, ['-sTCP:']) == sorted([('^', ''), ('LISTEN', ''), ('CLOSED', '')])


def test_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_completions(completions_file_path, input) == [('--option=arg1', 'descr1'), ('--option=arg2', 'descr2')]

def test_jit_subword_descriptions(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option=(arg1 "descr1" | arg2 "descr2");'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, ['--option=']) == sorted([('arg1', 'descr1'), ('arg2', 'descr2')])


def test_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_completions(completions_file_path, input) == [('--option=argument', 'description')]


def test_jit_completes_subword_external_command(complgen_binary_path: Path):
    GRAMMAR = r'''cmd --option={ echo -e "argument\tdescription" };'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, ['--option=']) == sorted([('argument', 'description')])


def test_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@fish> ::= { echo fish };
'''
    with completion_script_path(complgen_binary_path, GRAMMAR) as completions_file_path:
        input = 'complete --command cmd --do-complete "cmd --option="'
        assert get_sorted_completions(completions_file_path, input) == [('--option=fish', '')]


def test_jit_subword_specialization(complgen_binary_path: Path):
    GRAMMAR = r'''
cmd --option=<FOO>;
<FOO> ::= { echo generic };
<FOO@fish> ::= { echo fish };
'''
    assert get_sorted_jit_fish_completions(complgen_binary_path, GRAMMAR, 0, ['--option=']) == sorted([('fish', '')])


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= { echo foo; echo bar };
'''
    with tempfile.TemporaryDirectory() as usage_files_dir:
        (Path(usage_files_dir) / 'mycargo.usage').write_text(GRAMMAR)
        INTEGRATION_SCRIPT = r'''
function _complgen_jit
    set --local COMP_LINE (commandline --cut-at-cursor)
    set --local COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end
    set --local usage_file_path $argv[1]
    set COMP_CWORD (math $COMP_CWORD - 1)
    {complgen_binary_path} complete $usage_file_path fish (math $COMP_CWORD - 1) -- $COMP_WORDS[2..]
end

for path in {usage_files_dir}/*.usage
    set --local stem (basename $path .usage)
    complete --command $stem --no-files --arguments "(_complgen_jit {usage_files_dir}/$stem.usage)"
end
'''.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)
        with tempfile.NamedTemporaryFile() as f:
            f.write(INTEGRATION_SCRIPT.encode())
            f.flush()
            input = 'complete --command mycargo --do-complete "mycargo +"'
            assert get_sorted_completions(f.name, input) == [('+bar', ''), ('+foo', '')]
