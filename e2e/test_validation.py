import subprocess
import sys
from pathlib import Path
from subprocess import CalledProcessError

from pytest import raises


def complgen_check(complgen_binary_path: Path, grammar: str):
    args = [complgen_binary_path, "check", "-"]
    subprocess.run(
        args,
        input=grammar.encode(),
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        check=True,
    )


def test_detects_ambiguous_transitions(complgen_binary_path: Path):
    complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} {{{ echo bar }}};""")

    with raises(CalledProcessError):
        complgen_check(
            complgen_binary_path, """cmd {{{ echo foo }}} | {{{ echo bar }}};"""
        )

    complgen_check(complgen_binary_path, """cmd (foo | {{{ echo bar }}});""")

    with raises(CalledProcessError):
        complgen_check(
            complgen_binary_path, """cmd {{{ echo foo }}} || {{{ echo bar }}};"""
        )

    complgen_check(complgen_binary_path, """cmd foo || {{{ echo bar }}};""")

    complgen_check(complgen_binary_path, """cmd [{{{ echo foo }}}] foo;""")

    complgen_check(complgen_binary_path, """cmd {{{ echo foo }}}... foo baz;""")


def test_issue_45(complgen_binary_path: Path):
    GRAMMAR = r"""
hyprctl [<OPTION>]... <COMMAND>;

<COMMAND> ::= animations          "list animations and beziers (not in --help)"
            | switchxkblayout     "switch keyboard layout" <DEVICE> (next | prev)
            ;

<DEVICE> ::= {{{ hyprctl devices -j | awk '/^"keyboards"/,/^\],$/' | sed -n 's/.*"name": "\(.*\)".*/\1/p' }}};
"""
    complgen_check(complgen_binary_path, GRAMMAR)
