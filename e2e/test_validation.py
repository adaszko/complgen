import subprocess
from pathlib import Path

from inline_snapshot import snapshot


def complgen_check(complgen_binary_path: Path, grammar: str) -> subprocess.CompletedProcess:
    args = [complgen_binary_path, "check", "-"]
    result = subprocess.run(
        args,
        input=grammar,
        capture_output=True,
        text=True,
    )
    return result


def test_detects_ambiguous_transitions(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} {{{ echo bar }}};""").returncode == 0

    r = complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} | {{{ echo bar }}};""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:4:error: External commands are only allowed at tail position
  |
0 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
  = help: Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)
  = help: or output the suffix from the external command
  = help: or specify regex the output needs to match via @SHELL"..." construct
""")

    assert complgen_check(complgen_binary_path, """cmd (foo | {{{ echo bar }}});""").returncode == 0

    r = complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} || {{{ echo bar }}};""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:4:error: External commands are only allowed at tail position
  |
0 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
  = help: Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)
  = help: or output the suffix from the external command
  = help: or specify regex the output needs to match via @SHELL"..." construct
""")

    assert complgen_check(complgen_binary_path, """cmd foo || {{{ echo bar }}};""").returncode == 0

    assert complgen_check(complgen_binary_path, """cmd [{{{ echo foo }}}] foo;""").returncode == 0

    assert complgen_check(complgen_binary_path, """cmd {{{ echo foo }}}... foo baz;""").returncode == 0


def test_issue_45(complgen_binary_path: Path):
    GRAMMAR = r"""
hyprctl [<OPTION>]... <COMMAND>;

<COMMAND> ::= animations          "list animations and beziers (not in --help)"
            | switchxkblayout     "switch keyboard layout" <DEVICE> (next | prev)
            ;

<DEVICE> ::= {{{ hyprctl devices -j | awk '/^"keyboards"/,/^\],$/' | sed -n 's/.*"name": "\(.*\)".*/\1/p' }}};
"""
    assert complgen_check(complgen_binary_path, GRAMMAR).returncode == 0
