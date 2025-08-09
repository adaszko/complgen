import glob
import subprocess
from pathlib import Path

import pytest
from inline_snapshot import snapshot


def complgen_check_path(complgen_binary_path: Path, path: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [complgen_binary_path, '--bash', '/dev/null', path],
        capture_output=True,
        text=True,
    )


@pytest.mark.skip("pending precise implementaion of ambiguity detection")
def test_examples(complgen_binary_path: Path, examples_directory_path: Path):
    for usage_file_path in glob.glob(str(examples_directory_path / "*.usage")):
        r = complgen_check_path(complgen_binary_path, usage_file_path)
        assert r.returncode == 0, usage_file_path


def complgen_check(complgen_binary_path: Path, grammar: str) -> subprocess.CompletedProcess:
    args = [complgen_binary_path, "--bash", "-", "-"]
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
0:4:error: Ambiguous grammar.  Matching can't differentiate:
  |
0 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
0:23:error: and:
  |
0 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |                        ^^^^^^^^^^^^^^^^
  |
""")

def test_ambiguous_transition3(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd (foo | {{{ echo bar }}});""").returncode == 0

def test_ambiguous_transition4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} || {{{ echo bar }}};""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
0:4:error: Ambiguous grammar.  Matching can't differentiate:
  |
0 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
0:24:error: and:
  |
0 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |                         ^^^^^^^^^^^^^^^^
  |
""")

def test_ambiguous_transition5(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd foo || {{{ echo bar }}};""").returncode == 0

def test_ambiguous_transition6(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd [{{{ echo foo }}}] foo;""").returncode == 0

def test_ambiguous_transition7(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd {{{ echo foo }}}... foo baz;""").returncode == 0

def test_ambiguous_transition8(complgen_binary_path: Path):
    GRAMMAR = """
mygit (<command> || [-c <name>=<value>] <command>);
<command> ::= clone;
"""
    r = complgen_check(complgen_binary_path, GRAMMAR)
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
1:24:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | mygit (<command> || [-c <name>=<value>] <command>);
  |                         ^^^^^^
  |
1:30:error: and:
  |
1 | mygit (<command> || [-c <name>=<value>] <command>);
  |                               ^
  |
""")



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
9:7:error: Adjacent literals in expression used in a subword context.  First one:
  |
9 |     | (quit <QUIT_ARGS> | exit <QUIT_ARGS> | q <QUIT_ARGS>) "exit aerc"
  |        ^^^^
  |
13:18:error: Second one:
   |
13 | <QUIT_ARGS> ::= ( -f) "force close aerc"
   |                   ^^
   |
   = help: Join the adjacent literals into one as spaces are invalid in a subword context
3:20:error: Referenced in a subword context at
  |
3 | aerc [<OPTION>]... :<COMMAND>;
  |                     ^^^^^^^^^
  |
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
warning: undefined nonterminal(s): FILE
0:8:error: Ambiguous grammar.  Matching can't differentiate:
  |
0 | darcs ( <FILE> | <DIRECTORY> );
  |         ^^^^^^
  |
0:17:error: and:
  |
0 | darcs ( <FILE> | <DIRECTORY> );
  |                  ^^^^^^^^^^^
  |
""")


def test_bug3(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
aerc [<OPTION>]...;
aerc [<OPTION>]... foo;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
warning: undefined nonterminal(s): OPTION
1:6:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | aerc [<OPTION>]...;
  |       ^^^^^^^^
  |
2:6:error: and:
  |
2 | aerc [<OPTION>]... foo;
  |       ^^^^^^^^
  |
""")


def test_bug4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """darcs [<INITIALIZATION>] <COMMAND>;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
warning: undefined nonterminal(s): INITIALIZATION COMMAND
0:7:error: Ambiguous grammar.  Matching can't differentiate:
  |
0 | darcs [<INITIALIZATION>] <COMMAND>;
  |        ^^^^^^^^^^^^^^^^
  |
0:25:error: and:
  |
0 | darcs [<INITIALIZATION>] <COMMAND>;
  |                          ^^^^^^^^^
  |
""")


def test_clashing_variants(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, r"""
mygit (clone "Clone a repository into a new directory" | clone --bare);
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
1:7:error: Clashing variants.  Completion can't differentiate:
  |
1 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |
1:57:error: and:
  |
1 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |                                                          ^^^^^
  |
""")
