import tempfile
from pathlib import Path

from conftest import (
    get_sorted_fish_completions,
    temp_file_with_contents,
)

INTEGRATION_SCRIPT_TEMPLATE = r"""
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
    set --local prefix $COMP_WORDS[$COMP_CWORD]
    set --local last (math $COMP_CWORD - 1)
    if test $last -lt 2
        set words
    else
        set words $COMP_WORDS[2..$last]
    end
    {complgen_binary_path} jit --test dummy $usage_file_path fish --prefix="$prefix" -- $words | source -
    __complgen_jit "$prefix"
end

for path in {usage_files_dir}/*.usage
    set --local stem (basename $path .usage)
    complete --command $stem --no-files --arguments "(_complgen_jit {usage_files_dir}/$stem.usage)"
end
"""


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = """
mycargo +<toolchain>;
<toolchain> ::= {{{ echo foo; echo bar }}};
"""
    with tempfile.TemporaryDirectory() as usage_files_dir:
        INTEGRATION_SCRIPT = INTEGRATION_SCRIPT_TEMPLATE.format(
            complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir
        )
        (Path(usage_files_dir) / "mycargo.usage").write_text(GRAMMAR)
        with temp_file_with_contents(INTEGRATION_SCRIPT) as p:
            command = 'complete --do-complete "mycargo +"'
            assert get_sorted_fish_completions(p, command) == [
                ("+bar", ""),
                ("+foo", ""),
            ]


def test_shell_integration_descriptions(complgen_binary_path: Path):
    GRAMMAR = """
mycargo foo "descr1";
"""
    with tempfile.TemporaryDirectory() as usage_files_dir:
        INTEGRATION_SCRIPT = INTEGRATION_SCRIPT_TEMPLATE.format(
            complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir
        )
        (Path(usage_files_dir) / "mycargo.usage").write_text(GRAMMAR)
        with temp_file_with_contents(INTEGRATION_SCRIPT) as p:
            command = 'complete --do-complete "mycargo "'
            assert get_sorted_fish_completions(p, command) == [("foo", "descr1")]
