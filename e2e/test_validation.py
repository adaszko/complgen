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
-:1:5:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
-:1:24:error: and:
  |
1 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |                        ^^^^^^^^^^^^^^^^
  |
""")

def test_ambiguous_transition3(complgen_binary_path: Path):
    assert complgen_check(complgen_binary_path, """cmd (foo | {{{ echo bar }}});""").returncode == 0

def test_ambiguous_transition4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """cmd {{{ echo foo }}} || {{{ echo bar }}};""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:1:5:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^
  |
-:1:25:error: and:
  |
1 | cmd {{{ echo foo }}} || {{{ echo bar }}};
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
-:2:25:error: Ambiguous grammar.  Matching can't ascertain where below element ends:
  |
2 | mygit (<command> || [-c <name>=<value>] <command>);
  |                         ^^^^^^
  |
-:2:31:error: ...and where below element begins:
  |
2 | mygit (<command> || [-c <name>=<value>] <command>);
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
-:1:8:error: Adjacent literals in expression used in a subword context
  |
1 | aerc :(quit -f);
  |        ^^^^ First one
  |
-:1:13:error
  |
1 | aerc :(quit -f);
  |             ^^ Second one
  |
  = help: Join the adjacent literals into one as spaces are invalid in a subword context
""")


def test_subword_spaces_detection2(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """aerc :<COMMAND>; <COMMAND> ::= quit -f;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:1:32:error: Adjacent literals in expression used in a subword context
  |
1 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
  |                                ^^^^ First one
  |
-:1:37:error
  |
1 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
  |                                     ^^ Second one
  |
  = help: Join the adjacent literals into one as spaces are invalid in a subword context
-:1:7:error: Referenced in a subword context at
  |
1 | aerc :<COMMAND>; <COMMAND> ::= quit -f;
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
-:10:8:error: Adjacent literals in expression used in a subword context
   |
10 |     | (quit <QUIT_ARGS> | exit <QUIT_ARGS> | q <QUIT_ARGS>) "exit aerc"
   |        ^^^^ First one
   |
-:14:19:error
   |
14 | <QUIT_ARGS> ::= ( -f) "force close aerc"
   |                   ^^ Second one
   |
   = help: Join the adjacent literals into one as spaces are invalid in a subword context
-:4:21:error: Referenced in a subword context at
  |
4 | aerc [<OPTION>]... :<COMMAND>;
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
-:1:9:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | darcs ( <FILE> | <DIRECTORY> );
  |         ^^^^^^
  |
-:1:18:error: and:
  |
1 | darcs ( <FILE> | <DIRECTORY> );
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
-:2:7:error: Ambiguous grammar.  Matching can't differentiate:
  |
2 | aerc [<OPTION>]...;
  |       ^^^^^^^^
  |
-:3:7:error: and:
  |
3 | aerc [<OPTION>]... foo;
  |       ^^^^^^^^
  |
""")


def test_bug4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """darcs [<INITIALIZATION>] <COMMAND>;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
warning: undefined nonterminal(s): INITIALIZATION COMMAND
-:1:8:error: Ambiguous grammar.  Matching can't differentiate:
  |
1 | darcs [<INITIALIZATION>] <COMMAND>;
  |        ^^^^^^^^^^^^^^^^
  |
-:1:26:error: and:
  |
1 | darcs [<INITIALIZATION>] <COMMAND>;
  |                          ^^^^^^^^^
  |
""")


def test_clashing_variants(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, r"""
mygit (clone "Clone a repository into a new directory" | clone --bare);
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:8:error: Clashing variants.  Completion can't differentiate:
  |
2 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |
-:2:58:error: and:
  |
2 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |                                                          ^^^^^
  |
""")


def test_ambiguous_dfa(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
darcs <SOURCE> ... <DESTINATION>;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
warning: undefined nonterminal(s): SOURCE DESTINATION
-:2:7:error: Ambiguous grammar.  Matching can't differentiate:
  |
2 | darcs <SOURCE> ... <DESTINATION>;
  |       ^^^^^^^^
  |
-:2:20:error: and:
  |
2 | darcs <SOURCE> ... <DESTINATION>;
  |                    ^^^^^^^^^^^^^
  |
""")


def test_ambiguous_subword_leader(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
foo <BAR>=bar;
foo baz;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:5:error: Ambiguous grammar.  Matching can't ascertain where below element ends:
  |
2 | foo <BAR>=bar;
  |     ^^^^^
  |
-:2:10:error: ...and where below element begins:
  |
2 | foo <BAR>=bar;
  |          ^^^^
  |
""")


def test_unambiguous_subword_leaders(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
darcs foo={{{ echo first }}};
darcs foo=bar{{{ echo second }}};
""")
    assert r.returncode == 0
    assert r.stderr == snapshot('')


def test_duplicated_subword_leader(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
foo bar=<BAR>;
foo bar=<BAZ>;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
warning: undefined nonterminal(s): BAZ BAR
-:2:5:error: Clashing subword leaders.  Completion can't differentiate:
  |
2 | foo bar=<BAR>;
  |     ^^^^^^^^^
  |
-:3:5:error: and:
  |
3 | foo bar=<BAZ>;
  |     ^^^^^^^^^
  |
""")


def test_varying_command_names(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
foo quux;
bar quux;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:1:error: Varying command names:
  |
2 | foo quux;
  | ^^^
  |
-:3:1:error: Varying command names:
  |
3 | bar quux;
  | ^^^
  |
""")


def test_unknown_shell(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
foo quux;
<BAR@quux> ::= {{{ echo foo }}};
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:3:6:error: Unknown shell
  |
3 | <BAR@quux> ::= {{{ echo foo }}};
  |      ^^^^
  |
""")


def test_invalid_command_name(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """
foo/bar quux;
""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:1:error: Invalid command name
  |
2 | foo/bar quux;
  | ^^^^^^^
  |
""")
