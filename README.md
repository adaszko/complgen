## Value Proposition

`complgen` generates completion scripts for bash/fish/zsh from a man-page/EBNF-like grammar.  The resulting
scripts require only the target shell to be present and can be distributed on their own.

## Demo

[![asciicast](https://asciinema.org/a/SAH1uGqgwBEhyRV7G6Zasu45y.svg)](https://asciinema.org/a/SAH1uGqgwBEhyRV7G6Zasu45y)

## Usage

### Bash

<details>

```sh
$ cat hello.usage
hello --color=(always | never | auto);
$ complgen aot --bash-script hello.bash hello.usage
$ bash
$$ source hello.bash
$$ hello --color <TAB>
always auto never
```

</details>

### Fish

<details>

```sh
$ cat hello.usage
mygrep --color=(always | never | auto);
$ complgen aot --fish-script hello.fish hello.usage
$ source hello.fish
$ hello --color=
--color=always  --color=auto  --color=never
```

</details>

### Zsh

<details>

```sh
➜ cat hello.usage
hello --color=(always | never | auto);
➜ complgen aot --zsh-script _hello hello.usage
➜ source _hello
➜ hello --color=                                                                                                                                                                                                                                                       [~/repos/complgen]
always
auto
never
```

💡 Note: Under ZSH, `source` isn't strictly necessary — it is enough to put the output file in one of the
directories listed in `$fpath` variable.
</details>

### Just-in-Time Mode (for advanced users)

This mode bypasses writing to a file and thus reduces the number of required manual steps needed to use a
completion script.  Since it's just an optimization, it is intended for heavy complgen users and is covered in
[a separate document](ADVANCED.md).

## Installation

```sh
cargo install --git https://github.com/adaszko/complgen complgen
```

## Syntax

See the [`examples` subdirectory](examples/).

Try piping through the `scrape` subcommand to quickly generate grammar skeleton that can be tweaked
further, e.g.:

```
$ grep --help | complgen scrape
 | (-E | --extended-regexp) "PATTERNS are extended regular expressions"
 | (-F | --fixed-strings) "PATTERNS are strings"
 | (-G | --basic-regexp) "PATTERNS are basic regular expressions"
[...]
```

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

A grammar is a series of lines terminated by a semicolon (`;`).  Each line either represents a single variant
of invoking the completed command or is a nonterminal definition.

 * `a b` matches `a` followed by `b`.
 * `a b | c` matches either `a b` or `c` (IOW: sequence binds stronger than alternative).
 * `[a]` matches zero or one occurrences of `a`.
 * `a...` matches one or more occurrences of `a`
 * `[a]...` matches zero or more occurrences of `a`.
 * `(aaa | bbb || ccc)` shows `aaa` and `bbb` as candidates, and `ccc` only when current input matches neither
   `aaa` nor `bbb`.  `||` behaves exactly like `|` when matching, it differs only when offering completions.

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

### Filename completion

There's a couple of predefined nonterminals that are handled specially by `complgen`:

| Name          | bash | fish | zsh | Description |
|---------------|------|------|-----|-------------|
|`<PATH>`       | ✅   | ✅   | ✅  | file or directory path |
|`<DIRECTORY>`  | ✅   | ✅   | ✅  | directory path |
|`<PID>`        | ❌   | ✅   | ✅  | process id |
|`<USER>`       | ✅   | ✅   | ✅  | user name |
|`<GROUP>`      | ✅   | ✅   | ✅  | group name |
|`<HOST>`       | ✅   | ✅   | ✅  | hostname |
|`<INTERFACE>`  | ❌   | ✅   | ✅  | network interface name |
|`<PACKAGE>`    | ❌   | ✅   | ❌  | OS package name |

The reason there's no predefined `<FILE>` nonterminal is that it would work only for files from the current
directory which is too specific to be generally useful.

These nonterminals can still be defined in the grammar in the usual way (`<PATH> ::= ...`), in which case
their predefined meaning gets overriden.

### Completion descriptions (fish/zsh only)

If a literal is immediately followed with a quoted string, it's going to appear as a hint to the user at
completion time.  E.g. the grammar:

```sh
grep --extended-regexp "PATTERNS are extended regular expressions" | --exclude  (skip files that match GLOB)
```

results in something like this under fish (and zsh):

```fish
fish> grep --ex<TAB>
--exclude  (skip files that match GLOB)  --extended-regexp  (PATTERNS are extended regular expressions)
```

Note that `bash` does not support showing descriptions.

### Sourcing completions from external commands output

It is possible to use entire shell commands as a source of completions:

```
cargo {{{ rustup toolchain list | cut -d' ' -f1 | sed 's/^/+/' }}};
```

The stdout of the pipeline above will be automatically filtered by the shell based on the prefix entered so
far.

##### The prefix entered so far

Sometimes, it's more efficient to take into account the entered prefix in the shell command itself.  For all
three shells (bash, fish, zsh), it's available in the `$1` variable:

```
cargo {{{ rustup toolchain list | cut -d' ' -f1 | grep "^$1" | sed 's/^/+/' }}};
```

Note that in general, it's best to leave the filtering up to the executing shell since it may be configured to
perform some non-standard filtering.  zsh for example is capable of expanding `/u/l/b` to `/usr/local/bin`.

##### Completion descriptions

Externals commands are also assumed to produce descriptions similar to those described in the [section
above](#descriptions-aka-completion-hints).  Their expected stdout format is a sequence of lines of the form

```
COMPLETION\tDESCRIPTION
```

For fish and zsh, the `DESCRIPTION` part will be presented to the user.  Under bash, only the `COMPLETION`
part will be visible.  All external commands nonetheless need to take care as to *not* produce superfluous
`\t` characters that may confuse the resulting shell scripts.

### Target shell-specific behavior

In order to make use of shell-specific completion functions, `complgen` supports a mechanism that allows for
picking a specific nonterminal expansion based on the target shell.  To use an example: all shells are able to
complete a user on the system, although each has a different function for it.  We unify their interface under
the nonterminal `<USER>` using few `nonterminal@shell` definitions:

```
cmd <USER>;
<USER@bash> ::= {{{ compgen -A user "$1" | sort | uniq }}}; # bash produces duplicates for some reason
<USER@fish> ::= {{{ __fish_complete_users "$1" }}};
<USER@zsh> ::= {{{ _users }}};
```

### Completing option arguments

It's possible to match not only entire words, but also *within* words themselves, using the same grammar
syntax as for matching entire words.  In that sense, it all fractally works on subwords too (there are
limitations on `{{{ ... }}}` usage though).  The most common application of that general mechanism is to
handle equal sign arguments (`--option=ARGUMENT`):

```
grep --color=(always | never | auto);
```

Note however that equal sign arguments aren't some special case within complgen — the same mechanism works for
more complicated things, e.g.:

```
strace -e <EXPR>;
<EXPR> ::= [<qualifier>=][!]<value>[,<value>]...;
<qualifier> ::= trace | read | write | fault;
<value> ::= %file | file | all;
```

The above grammar was pulled straight out of [`strace` man page](https://man7.org/linux/man-pages/man1/strace.1.html#OPTIONS).

### Cleaning up the list of completion candidates

If you do `git <TAB>` in most shells you're presented with a list of git subcommands.  Even though git accepts
a bunch of global options (`--help`, `--version`, etc.), they don't show up there (sic!).  That's a special
mechanism intended for reducing clutter.  Under complgen, the same effect is achieved via a construction
called fallbacks, which are represented in the grammar as the double bar operator (`||`):

```
mygit (<SUBCOMMAND> || <OPTION>);
<SUBCOMMAND> ::= fetch | add | commit | push;
<OPTION> ::= --help | --version;
```

With the grammar above, `git <TAB>` will offer to complete *only* subcommands.  For `git --<TAB>` OTOH,
`complgen` will offer to complete options.

`||` has the lowest priority of all operators, so the grammar above might have been written without any use of
`<NONTERMINALS>`.  They're there only for readability sake.

## Trailing spaces handling

There are few general rules governing whether to append a space to a completion:

 * A space is appended if the completion corresponds to an entire literal from the `.usage` file, e.g.
   for the grammar `cmd --help;` and the command line `cmd <TAB>`, it completes to `cmd --help<SPACE>`.

 * A trailing space isn't appended if the literal is a part of a subword and the entire subword hasn't been
   completed yet, e.g. for the grammar `cmd --color=(auto|always);` and the command line `cmd --col<TAB>`, it
 completes to `cmd --color=` (no trailing space).

There are exceptions:

 * Under Fish, if your completion contains [one of the special
   characters](https://github.com/fish-shell/fish-shell/blob/408ab860906fbf6e08f314bea982220fdee3428e/src/complete.cpp#L183),
 fish won't insert the trailing space.  See also [Not adding space after dot at completion time · Issue #6928](https://github.com/fish-shell/fish-shell/issues/6928).

## Caveats

* `{{{ ... }}}` is only allowed at tail positions, where it doesn't lead to matching against an arbitrary
  output of an external command:
    * OK: `cmd {{{ echo foo }}} bar;`
    * ERROR: `cmd ({{{ echo foo }}} | bar);`

        We'd have to match against the output of `{{{ echo foo }}}` at *compilation* time to determine which
        branch to take, which is impossible to do in general as `echo foo` might as well have been an
        arbitrary shell command.

    * OK: `cmd (foo | {{{ echo bar }}});`
    * ERROR: `cmd ({{{ echo foo }}} || bar);`

        Reason: Same as for `|` above.

    * OK: `cmd (foo || {{{ echo bar }}});`
    * OK: `cmd [{{{ echo foo }}}] foo`;

        The entire commands always gets tokenized into shell words as a first step, so it's possible to tell
        where the output of `{{{ echo foo }}}` ends.  Note that two `foo` completions are produced here, then
        they're deduplicated, and finally, only one `foo` is offered to the user.

    * OK: `cmd {{{ echo foo }}}... foo baz`;

* Within subwords, `{{{ ... }}}` is only allowed at tail position, where it doesn't lead to matching against
  an arbitrary output of an external command:
    * ERROR: `{{{ git tag }}}..tag`

        Impossible to guess at compilation time where the output of the first `{{{ git tag }}` ends and *our*
        `..` begins.

    * OK: `--option={{{ echo foo }}}`
    * ERROR: `{{{ echo foo }}}bar`

        Impossible to guess at compilation time where the output of the first `{{{ echo foo }}` ends the
        second `bar` begins in the general case.

* The limitations above also apply to predefined nonterminals (`<PATH>`, `<DIRECTORY>`, etc.) since they're
  internally implemented as external commands.

 * Bash requires `bash-completion` OS package to be installed because completion scripts produced by
   `complgen`, call shell functions from that package at *completion* time.  This is necessary to work around
   Bash's default behavior of [breaking shell words on any character present in the
   `$COMP_WORDBREAKS`](https://stackoverflow.com/a/12495480) environment variable.

* Non-regular grammars aren't completed 100% *precisely*. For instance, in case of `find(1)`, `complgen` will
  still suggest `)` even in cases when all `(` have already been properly closed before the cursor.

## License

`complgen`'s source code is covered by [License](LICENSE).  Completion scripts generated by `complgen` are
subject only to [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
