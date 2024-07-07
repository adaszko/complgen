import sys
import tempfile
import subprocess
from pathlib import Path

from conftest import gen_zsh_capture_script_path


INTEGRATION_SCRIPT_TEMPLATE = '''
_complgen_jit () {{
    local stem=$1
    local -a w=("${{(@)words[2,$CURRENT-1]}}")
    local zsh_code=$({complgen_binary_path} jit {usage_files_dir}/$stem.usage zsh --prefix="$PREFIX" -- "${{w[@]}}")
    eval $zsh_code
    return 0
}}

for f in {usage_files_dir}/*.usage(N); do
    local stem=$f:t:r
    compdef "_complgen_jit $stem" $stem
done
'''


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= {{{ echo foo; echo bar }}};
'''
    with tempfile.TemporaryDirectory() as usage_files_dir:
        (Path(usage_files_dir) / 'mycargo.usage').write_text(GRAMMAR)
        INTEGRATION_SCRIPT = INTEGRATION_SCRIPT_TEMPLATE.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)
        with gen_zsh_capture_script_path(INTEGRATION_SCRIPT) as script_path:
            zsh_process = subprocess.run(['zsh', script_path, 'mycargo +'], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
            stdout = zsh_process.stdout.decode()
            completions = stdout.splitlines()
            completions.sort()
            assert completions == ['+bar', '+foo']
