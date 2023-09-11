import contextlib
import tempfile
from pathlib import Path
from typing import Generator

from conftest import get_sorted_bash_completions


def make_integration_script(complgen_binary_path: Path, usage_files_dir: Path):
    return r'''
for path in {usage_files_dir}/*.usage; do
    stem=$(basename "$path" .usage)
    eval "
_complgen_jit_$stem () {{
    local words cword
    _get_comp_words_by_ref -n = words cword
    local -a completions=(\$({complgen_binary_path} complete \"{usage_files_dir}/$stem.usage\" bash \$((COMP_CWORD - 1)) -- \${{COMP_WORDS[@]:1}}))
    local prefix="\${{COMP_WORDS[\$COMP_CWORD]}}"
    for item in "\${{completions[@]}}"; do
        if [[ \$item = "\$prefix"* ]]; then
            COMPREPLY+=("\$item")
        fi
    done
    __ltrim_colon_completions "\$prefix"
    return 0
}}
"
    complete -o nospace -F _complgen_jit_$stem "$stem"
    unset stem
done
'''.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)



@contextlib.contextmanager
def temp_usage_file_path(complgen_binary_path: Path, grammar: str, command: str) -> Generator[Path, None, None]:
    with tempfile.TemporaryDirectory() as usage_files_dir:
        (Path(usage_files_dir) / '{}.usage'.format(command)).write_text(grammar)
        with tempfile.NamedTemporaryFile() as f:
            integration_script = make_integration_script(complgen_binary_path, Path(usage_files_dir))
            f.write(integration_script.encode())
            f.flush()
            yield Path(f.name)


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= {{{ echo foo; echo bar }}};
'''
    with temp_usage_file_path(complgen_binary_path, GRAMMAR, 'mycargo') as usage_file_path:
        input = r'''COMP_WORDS=(mycargo +); COMP_CWORD=1; _complgen_jit_mycargo; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(usage_file_path, input) == sorted(['+foo', '+bar'])


def test_shell_integration_wordbreaks_chars(complgen_binary_path: Path):
    GRAMMAR = '''
mygrep --color "use markers to highlight the matching strings"=<WHEN>;
<WHEN> ::= always | never | auto;
'''
    with temp_usage_file_path(complgen_binary_path, GRAMMAR, 'mygrep') as usage_file_path:
        input = r'''COMP_WORDS=(mygrep --color=); COMP_CWORD=1; _complgen_jit_mygrep; printf '%s\n' "${COMPREPLY[@]}"'''
        assert get_sorted_bash_completions(usage_file_path, input) == sorted(['--color=always', '--color=never', '--color=auto'])
