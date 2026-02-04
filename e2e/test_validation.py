import glob
import subprocess
from pathlib import Path

from inline_snapshot import snapshot


def complgen_check_path(
    complgen_binary_path: Path, path: str
) -> subprocess.CompletedProcess:
    return subprocess.run(
        [complgen_binary_path, "--bash", "/dev/null", path],
        capture_output=True,
        text=True,
    )


def test_examples(complgen_binary_path: Path, examples_directory_path: Path):
    for usage_file_path in glob.glob(str(examples_directory_path / "*.usage")):
        r = complgen_check_path(complgen_binary_path, usage_file_path)
        assert r.returncode == 0, usage_file_path


def complgen_check(
    complgen_binary_path: Path,
    grammar: str,
    shell: str = "bash",
) -> subprocess.CompletedProcess:
    opt_from_shell = {
        "bash": "--bash",
        "fish": "--fish",
        "zsh": "--zsh",
        "pwsh": "--pwsh",
    }
    assert shell in opt_from_shell
    args = [complgen_binary_path, opt_from_shell[shell], "-", "-"]
    result = subprocess.run(
        args,
        input=grammar,
        capture_output=True,
        text=True,
    )
    return result


def test_ambiguous_transition1(complgen_binary_path: Path):
    assert (
        complgen_check(
            complgen_binary_path, """cmd {{{ echo foo }}} {{{ echo bar }}};"""
        ).returncode
        == 0
    )


def test_ambiguous_transition2(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path, """cmd {{{ echo foo }}} | {{{ echo bar }}};"""
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:1:5:error: Ambiguous grammar
  |
1 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^ matching can't tell apart this
  |
-:1:24:error
  |
1 | cmd {{{ echo foo }}} | {{{ echo bar }}};
  |                        ^^^^^^^^^^^^^^^^ from this
  |
""")


def test_ambiguous_transition3(complgen_binary_path: Path):
    assert (
        complgen_check(
            complgen_binary_path, """cmd (foo | {{{ echo bar }}});"""
        ).returncode
        == 0
    )


def test_ambiguous_transition4(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path, """cmd {{{ echo foo }}} || {{{ echo bar }}};"""
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:1:5:error: Ambiguous grammar
  |
1 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |     ^^^^^^^^^^^^^^^^ matching can't tell apart this
  |
-:1:25:error
  |
1 | cmd {{{ echo foo }}} || {{{ echo bar }}};
  |                         ^^^^^^^^^^^^^^^^ from this
  |
""")


def test_ambiguous_transition5(complgen_binary_path: Path):
    assert (
        complgen_check(
            complgen_binary_path, """cmd foo || {{{ echo bar }}};"""
        ).returncode
        == 0
    )


def test_ambiguous_transition6(complgen_binary_path: Path):
    assert (
        complgen_check(
            complgen_binary_path, """cmd [{{{ echo foo }}}] foo;"""
        ).returncode
        == 0
    )


def test_ambiguous_transition7(complgen_binary_path: Path):
    assert (
        complgen_check(
            complgen_binary_path, """cmd {{{ echo foo }}}... foo baz;"""
        ).returncode
        == 0
    )


def test_ambiguous_transition8(complgen_binary_path: Path):
    GRAMMAR = """
mygit (<command> || [-c <name>=<value>] <command>);
<command> ::= clone;
"""
    r = complgen_check(complgen_binary_path, GRAMMAR)
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:25:error: Ambiguous grammar
  |
2 | mygit (<command> || [-c <name>=<value>] <command>);
  |                         ^^^^^^ matching can't tell where this ends
  |
-:2:31:error
  |
2 | mygit (<command> || [-c <name>=<value>] <command>);
  |                               ^ and where this begins
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
    r = complgen_check(
        complgen_binary_path, """aerc :<COMMAND>; <COMMAND> ::= quit -f;"""
    )
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
    r = complgen_check(
        complgen_binary_path,
        """aerc [<OPTION>]...;
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
    ;""",
    )
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
-:1:9:error: Ambiguous grammar
  |
1 | darcs ( <FILE> | <DIRECTORY> );
  |         ^^^^^^ matching can't tell apart this
  |
-:1:18:error
  |
1 | darcs ( <FILE> | <DIRECTORY> );
  |                  ^^^^^^^^^^^ from this
  |
""")


def test_bug3(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
aerc [<OPTION>]...;
aerc [<OPTION>]... foo;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:7:error: Ambiguous grammar
  |
2 | aerc [<OPTION>]...;
  |       ^^^^^^^^ matching can't tell apart this
  |
-:3:7:error
  |
3 | aerc [<OPTION>]... foo;
  |       ^^^^^^^^ from this
  |
""")


def test_bug4(complgen_binary_path: Path):
    r = complgen_check(complgen_binary_path, """darcs [<INITIALIZATION>] <COMMAND>;""")
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:1:8:error: Ambiguous grammar
  |
1 | darcs [<INITIALIZATION>] <COMMAND>;
  |        ^^^^^^^^^^^^^^^^ matching can't tell apart this
  |
-:1:26:error
  |
1 | darcs [<INITIALIZATION>] <COMMAND>;
  |                          ^^^^^^^^^ from this
  |
""")


def test_bug5(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
mygit diff <PATH>;
mygit diff {{{}}}@fish"[a-zA-Z_-]*";
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
error: DFA Ambiguity:
  mygit diff {{{ compgen -A file -- "$1" }}}
  mygit diff {{{  }}}
""")


def test_bug6(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd [<commit>] <PATH>;
<commit> ::= {{{}}}@zsh"[a-zA-Z_-]*";
<commit@bash> ::= {{{ ls $(rev-parse --show-toplevel)/.git/refs/heads/ }}}@bash"[a-zA-Z_-]*";
""",
        shell="zsh",
    )
    assert r.returncode == 0
    assert r.stderr == snapshot("""""")


def test_clashing_variants(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        r"""
mygit (clone "Clone a repository into a new directory" | clone --bare);
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:8:error: Clashing variants
  |
2 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ completion can't tell apart
  |
-:2:58:error
  |
2 | mygit (clone "Clone a repository into a new directory" | clone --bare);
  |                                                          ^^^^^ from this
  |
""")


def test_ambiguous_dfa(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
darcs <SOURCE> ... <DESTINATION>;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:7:error: Ambiguous grammar
  |
2 | darcs <SOURCE> ... <DESTINATION>;
  |       ^^^^^^^^ matching can't tell apart this
  |
-:2:20:error
  |
2 | darcs <SOURCE> ... <DESTINATION>;
  |                    ^^^^^^^^^^^^^ from this
  |
""")


def test_ambiguous_subword_leader(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
foo <BAR>=bar;
foo baz;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:5:error: Ambiguous grammar
  |
2 | foo <BAR>=bar;
  |     ^^^^^ matching can't tell where this ends
  |
-:2:10:error
  |
2 | foo <BAR>=bar;
  |          ^^^^ and where this begins
  |
""")


def test_unambiguous_subword_leaders(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
darcs foo={{{ echo first }}};
darcs foo=bar{{{ echo second }}};
""",
    )
    assert r.returncode == 0
    assert r.stderr == snapshot("")


def test_varying_command_names(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
foo quux;
bar quux;
""",
    )
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
    r = complgen_check(
        complgen_binary_path,
        """
foo quux;
<BAR@quux> ::= {{{ echo foo }}};
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:3:6:error: Unknown shell
  |
3 | <BAR@quux> ::= {{{ echo foo }}};
  |      ^^^^
  |
  = help: Can only use one of: bash, fish, zsh, pwsh
""")


def test_invalid_command_name(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
foo/bar quux;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:2:1:error: Invalid command name
  |
2 | foo/bar quux;
  | ^^^^^^^
  |
""")


def test_nonterminal_cycle_outer(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd <FOO>;
<FOO> ::= <BAR>;
<BAR> ::= <FOO>;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:3:1:error: Nonterminal definitions cycle
  |
3 | <FOO> ::= <BAR>;
  | ^^^^^
  |
-:3:11:error: Nonterminal definitions cycle
  |
3 | <FOO> ::= <BAR>;
  |           ^^^^^
  |
-:4:11:error: Nonterminal definitions cycle
  |
4 | <BAR> ::= <FOO>;
  |           ^^^^^
  |
""")


def test_nonterminal_cycle_inner(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd <FOO>;
<FOO> ::= <BAR>;
<BAR> ::= <QUUX>;
<QUUX> ::= <BAR>;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:3:1:error: Nonterminal definitions cycle
  |
3 | <FOO> ::= <BAR>;
  | ^^^^^
  |
-:3:11:error: Nonterminal definitions cycle
  |
3 | <FOO> ::= <BAR>;
  |           ^^^^^
  |
-:4:11:error: Nonterminal definitions cycle
  |
4 | <BAR> ::= <QUUX>;
  |           ^^^^^^
  |
-:5:12:error: Nonterminal definitions cycle
  |
5 | <QUUX> ::= <BAR>;
  |            ^^^^^
  |
""")


def test_non_command_specialization(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd <FOO>;
<FOO@bash> ::= foo;
<FOO@bash> ::= bar;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:3:16:error: Can only specialize external commands
  |
3 | <FOO@bash> ::= foo;
  |                ^^^
  |
  = help: Use a {{{ ... }}} command here instead
""")


def test_duplicated_nonterminal_definition(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd <FOO>;
<FOO> ::= foo;
<FOO> ::= bar;
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:4:1:error: Duplicate nonterminal definition
  |
4 | <FOO> ::= bar;
  | ^^^^^
  |
-:3:1:error: Previous definition
  |
3 | <FOO> ::= foo;
  | ^^^^^
  |
""")


def test_duplicated_nonterminal_definition_specialization(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd <FOO>;
<FOO@bash> ::= {{{ echo foo }}};
<FOO@bash> ::= {{{ echo bar }}};
""",
    )
    assert r.returncode == 1
    assert r.stderr == snapshot("""\
-:4:1:error: Duplicate nonterminal definition
  |
4 | <FOO@bash> ::= {{{ echo bar }}};
  | ^^^^^^^^^^
  |
-:3:1:error: Previous definition
  |
3 | <FOO@bash> ::= {{{ echo foo }}};
  | ^^^^^^^^^^
  |
""")


def test_unused_specialization(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
foo bar;
<foo@bash> ::= {{{ : }}};
""",
    )
    assert r.returncode == 0
    assert r.stderr == snapshot("""\
-:3:1:warning: Unused specialization
  |
3 | <foo@bash> ::= {{{ : }}};
  | ----------
  |
""")


def test_unused_specialization_issue68(complgen_binary_path: Path):
    """https://github.com/adaszko/complgen/issues/68"""
    r = complgen_check(
        complgen_binary_path,
        """
spec_test <COMMAND>;
<COMMAND> ::= command <ID>;
<ID@bash> ::= {{{ : }}};
<ID@zsh> ::= {{{ : }}};
""",
    )
    assert r.returncode == 0
    assert r.stderr == snapshot("""""")


def test_transitive_unused_specialization(complgen_binary_path: Path):
    r = complgen_check(
        complgen_binary_path,
        """
cmd foo;
<BAR> ::= <BAZ>;
<BAZ@bash> ::= {{{ : }}};
<BAZ@zsh> ::= {{{ : }}};
""",
    )
    assert r.returncode == 0
    assert r.stderr == snapshot("""\
-:3:1:warning: Unused
  |
3 | <BAR> ::= <BAZ>;
  | -----
  |
""")
