import glob
import subprocess
from pathlib import Path

import pytest
from inline_snapshot import snapshot


def complgen_check_path(complgen_binary_path: Path, path: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [complgen_binary_path, "check", path],
        capture_output=True,
        text=True,
    )


@pytest.mark.skip("pending precise implementaion of ambiguity detection")
def test_examples(complgen_binary_path: Path, examples_directory_path: Path):
    for usage_file_path in glob.glob(str(examples_directory_path / "*.usage")):
        r = complgen_check_path(complgen_binary_path, usage_file_path)
        assert r.returncode == 0, usage_file_path


def complgen_check(complgen_binary_path: Path, grammar: str) -> subprocess.CompletedProcess:
    args = [complgen_binary_path, "check", "-"]
    result = subprocess.run(
        args,
        input=grammar,
        capture_output=True,
        text=True,
    )
    return result


def test_ambiguous_transition1(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} {{{ echo bar }}};""").returncode == 0

def test_ambiguous_transition2(complgen_binary_path: Path):
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

def test_ambiguous_transition3(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd (foo | {{{ echo bar }}});""").returncode == 0

def test_ambiguous_transition4(complgen_binary_path: Path):
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

def test_ambiguous_transition5(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd foo || {{{ echo bar }}};""").returncode == 0

def test_ambiguous_transition6(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd [{{{ echo foo }}}] foo;""").returncode == 0

def test_ambiguous_transition6(complgen_binary_path: Path):
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


def test_subword_spaces_detection1(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """aerc :(quit -f);""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:7:error: Adjacent literals in expression used in a subword context.  First one:
  |
0 | aerc :(quit -f);
  |        ^^^^
  |
0:12:error: Second one:
  |
0 | aerc :(quit -f);
  |             ^^
  |
  = help: Join the adjacent literals into one as spaces are invalid in a subword context
""")


def test_subword_spaces_detection2(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """aerc :<COMMAND>; <COMMAND> ::= quit -f;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:31:error: Adjacent literals in expression used in a subword context.  First one:
  |
0 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
  |                                ^^^^
  |
0:36:error: Second one:
  |
0 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
  |                                     ^^
  |
  = help: Join the adjacent literals into one as spaces are invalid in a subword context
0:6:error: Referenced in a subword context at
  |
0 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
  |       ^^^^^^^^^
  |
""")

def test_subword_spaces_detection3(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """aerc [<OPTION>]...;
aerc [<OPTION>]... mbox:<PATH>;
aerc [<OPTION>]... mailto:<MAILTOLINK>;
aerc [<OPTION>]... :<COMMAND>;

# shortened for brevity

<COMMAND> ::= (help <topic> | man <topic>) "show help page inside aerc"
    # shortened for brevity
    | (quit <QUIT_ARGS> | exit <QUIT_ARGS> | q <QUIT_ARGS>) "exit aerc"
    | (redraw) "force a full redraw of the screen"
    ;

<QUIT_ARGS> ::= ( -f) "force close aerc"
    ;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:6:error: Undefined nonterminals are only allowed at tail position
  |
0 | aerc [<OPTION>]...;
  |       ^^^^^^^^
  |
  = help: Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)
  = help: or supply its definition via ::=
""")


def test_subword_spaces4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """cmd foo[bar];""")
    assert r.returncode == 0

    r = complgen_check(complgen_binary_path, """cmd foo((bar)...);""")
    assert r.returncode == 0


def test_bug1(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """cmd (--bash-script <PATH>) | --help;""")
    assert r.returncode == 0


def test_bug2(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """darcs ( <FILE> | <DIRECTORY> );""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:8:error: Undefined nonterminals are only allowed at tail position
  |
0 | darcs ( <FILE> | <DIRECTORY> );
  |         ^^^^^^
  |
  = help: Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)
  = help: or supply its definition via ::=
""")


def test_bug3(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
aerc [<OPTION>]...;
aerc [<OPTION>]... foo;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
1:6:error: Undefined nonterminals are only allowed at tail position
  |
1 | aerc [<OPTION>]...;
  |       ^^^^^^^^
  |
  = help: Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)
  = help: or supply its definition via ::=
""")


def test_bug4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """darcs [<INITIALIZATION>] <COMMAND>;""")
    assert r.returncode == 1
    assert r.stderr == snapshot('Error: Final DFA contains ambiguous transition(s): [Nonterminal(u!("INITIALIZATION"), None, 0), Nonterminal(u!("COMMAND"), None, 0)]\n')
