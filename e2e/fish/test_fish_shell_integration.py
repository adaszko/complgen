import tempfile
from pathlib import Path

from conftest import get_sorted_fish_completions


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
            assert get_sorted_fish_completions(f.name, input) == [('+bar', ''), ('+foo', '')]
