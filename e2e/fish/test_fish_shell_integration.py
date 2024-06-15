import tempfile
from pathlib import Path

from conftest import get_sorted_fish_completions, get_sorted_jit_fish_completions, temp_file_with_contents


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= {{{ echo foo; echo bar }}};
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
    set --local prefix $COMP_WORDS[$COMP_CWORD]
    set --local last (math $COMP_CWORD - 1)
    if test $last -lt 2
        set words
    else
        set words $COMP_WORDS[2..$last]
    end
    set -l fn (mktemp -q '/tmp/complgen.fish.XXXXXX')
    {complgen_binary_path} jit --test dummy $usage_file_path fish --prefix="$prefix" -- $words >$fn
    source $fn
    __complgen_jit "$prefix"
    rm -- "$fn"
end

for path in {usage_files_dir}/*.usage
    set --local stem (basename $path .usage)
    complete --command $stem --no-files --arguments "(_complgen_jit {usage_files_dir}/$stem.usage)"
end
'''.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)
        with temp_file_with_contents(INTEGRATION_SCRIPT) as p:
            command = 'complete --command mycargo --do-complete "mycargo +"'
            assert get_sorted_fish_completions(p, command) == [('+bar', ''), ('+foo', '')]
