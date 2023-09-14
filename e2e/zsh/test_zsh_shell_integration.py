import sys
import tempfile
import subprocess
from pathlib import Path

from conftest import capture_script_path


def test_shell_integration(complgen_binary_path: Path):
    GRAMMAR = '''
mycargo +<toolchain>;
<toolchain> ::= {{{ echo foo; echo bar }}};
'''
    with tempfile.TemporaryDirectory() as usage_files_dir:
        (Path(usage_files_dir) / 'mycargo.usage').write_text(GRAMMAR)
        INTEGRATION_SCRIPT = '''
_complgen_jit () {{
    local stem=$1
    local -a w=("${{(@)words[2,$#words]}}")
    local zsh_code=$({complgen_binary_path} complete {usage_files_dir}/$stem.usage zsh $((CURRENT - 2)) -- "${{w[@]}}")
    eval $zsh_code
    return 0
}}

for f in {usage_files_dir}/*.usage; do
    local stem=$f:t:r
    compdef "_complgen_jit $stem" $stem
done
'''.format(complgen_binary_path=complgen_binary_path, usage_files_dir=usage_files_dir)
        with capture_script_path(INTEGRATION_SCRIPT) as capture_path:
            zsh_process = subprocess.run(['zsh', capture_path, 'mycargo +'], stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
            stdout = zsh_process.stdout.decode()
            completions = stdout.splitlines()
            completions.sort()
            assert completions == ['+bar bar   ', '+foo foo   ']
