import sys
import subprocess


def test_jit_completes(complgen_binary_path: str):
    GRAMMAR = '''cmd (--help | --version); '''
    process = subprocess.run([complgen_binary_path, 'complete', '-', '--'], input=GRAMMAR.encode(), stdout=subprocess.PIPE, stderr=sys.stderr, check=True)
    assert sorted(process.stdout.decode().splitlines()) == sorted(['--help', '--version'])
