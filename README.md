## Value Proposition

`complgen` compiles man-page/EBNF-like grammars into standalone completion scripts.  See
[examples](examples/).

## Demo

![demo](https://github.com/adaszko/complgen/blob/demo/complgen.gif)

## Usage

### Bash

<details>

```sh
$ cat hello.usage
hello --color=(always | never | auto);
$ complgen --bash hello.bash hello.usage
$ source hello.bash
$ hello --color=<TAB>
always auto never
```

</details>

### Fish

<details>

```sh
$ cat hello.usage
hello --color=(always | never | auto);
$ complgen --fish hello.fish hello.usage
$ source hello.fish
$ hello --color=<TAB>
--color=always  --color=auto  --color=never
```

</details>

### Zsh

<details>

```sh
% cat hello.usage
hello --color=(always | never | auto);
% complgen --zsh _hello hello.usage
% source _hello
% hello --color=<TAB>
always
auto
never
```

💡 Note: Under ZSH, `source` isn't strictly necessary — it is enough to put the output file in one of the
directories listed in `$fpath` variable.
</details>

### PowerShell

<details>

```powershell
PS> Get-Content hello.usage
hello --color=(always | never | auto);
PS> complgen --pwsh hello.ps1 hello.usage
PS> . ./hello.ps1
PS> hello --color=<TAB>
--color=always  --color=auto  --color=never
```

💡 Note: Requires PowerShell Core 7+ (pwsh). Add the script to your `$PROFILE` for persistent completions.
</details>

## Installation

```sh
cargo install --git https://github.com/adaszko/complgen --tag v0.6.1 complgen
```

## Syntax

See the [`examples` subdirectory](examples/).

Try piping through the `scrape` subcommand to quickly generate grammar skeleton that can be tweaked
further, e.g.:

```
$ grep --help | complgen --scrape
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

There's a small set of predefined nonterminals that are handled specially by `complgen`:

| Name          | bash | fish | zsh | pwsh | Description |
|---------------|------|------|-----|------|-------------|
|`<PATH>`       | ✅   | ✅   | ✅  | ✅   | file or directory path |
|`<DIRECTORY>`  | ✅   | ✅   | ✅  | ✅   | directory path |

The reason there's no predefined `<FILE>` nonterminal is that it would work only for files from the current
directory which is too specific to be generally useful.

These nonterminals can still be defined in the grammar in the usual way (`<PATH> ::= ...`), in which case
their predefined meaning gets overriden.

### Descriptions (fish/zsh/pwsh)

If a literal is immediately followed with a quoted string, it's going to appear as a hint to the user at
completion time.  E.g. the grammar:

```sh
grep --extended-regexp "PATTERNS are extended regular expressions" | --exclude  "skip files that match GLOB";
```

results in something like this under fish, zsh, and PowerShell:

```fish
fish> grep --ex<TAB>
--exclude  (skip files that match GLOB)  --extended-regexp  (PATTERNS are extended regular expressions)
```

Note that `bash` does not support showing descriptions. In PowerShell, descriptions appear as tooltips.

### Sourcing completions from external commands output

It is possible to produce completions based on an external command output:

    cmd {{{ echo foo; echo bar; echo baz; echo quux }}};

The stdout of the pipeline above will be automatically filtered by the shell based on the prefix entered so
far.

In order to make the same command work both in super- and sub-word context, following arguments are passed:

 * `$1`: completed prefix
 * `$2`: subword input matched thus far (always an empty string in superwords completion context)

For example, given the grammar above, and the input `cmd b<TAB>`:

    $1 = "b"
    $2 = ""

However, for the grammar:

    cmd --opt={{{ echo foo; echo bar; echo baz; echo quux }}};

and input `cmd --opt=b<TAB>`:

    $1 = "b"
    $2 = "--opt="

Note that in general, it's best to leave the filtering up to the executing shell since it may be configured to
perform some non-prefix filtering.  zsh for example is capable of expanding `/u/l/b` to `/usr/local/bin`.

##### Descriptions

Externals commands are also assumed to produce descriptions similar to those described in the [section
above](#descriptions).  Their expected stdout format is a sequence of lines of the form

```
COMPLETION\tDESCRIPTION
```

For fish and zsh, the `DESCRIPTION` part will be presented to the user.  Under bash, only the `COMPLETION`
part will be visible.  All external commands nonetheless need to take care as to *not* produce superfluous
`\t` characters that may confuse the resulting shell scripts.

### Bypassing tail restriction

`complgen` will error out if you place `{{{ ... }}}` at a position where it's a subject to matching (as
opposed to completing).  It is possible to overcome that restriction by providing a regular expression
matching the command output:

```
cmd ({{{ echo foo }}}@bash"foo" | bar);
```

### Target shell-specific behavior

In order to make use of shell-specific completion functions, `complgen` supports a mechanism that allows for
picking a specific nonterminal expansion based on the target shell.  To use an example: all shells are able to
complete a user on the system, although each has a different function for it.  We unify their interface under
the nonterminal `<USER>` using few `nonterminal@shell` definitions:

```
cmd <USER>;
<USER@bash> ::= {{{ compgen -A user "$1" | sort | uniq }}}; # produce candidates on stdout under bash
<USER@fish> ::= {{{ __fish_complete_users "$1" }}}; # produce candidates on stdout under fish
<USER@zsh> ::= {{{ IPREFIX="$2" PREFIX="$1" _users }}}; # produce candidates via compadd and friends under zsh
<USER@pwsh> ::= {{{ Get-LocalUser | Where-Object { $_.Name -like "$prefix*" } | ForEach-Object { $_.Name } }}}; # PowerShell
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

## Caveats

* The tail commands limitation applies to predefined nonterminals (`<PATH>`, `<DIRECTORY>`, etc.) since
  they're internally implemented as external commands.

* Bash requires `bash-completion` OS package to be installed because completion scripts produced by
  `complgen`, call shell functions from that package at *completion* time.  This is necessary to work around
  Bash's default behavior of [breaking shell words on any character present in the
  `$COMP_WORDBREAKS`](https://stackoverflow.com/a/12495480) environment variable.

* PowerShell completions require PowerShell Core 7+ (pwsh). Windows PowerShell 5.x is not supported.

* [Fish 4.0 fuzzy subsequence filtering](https://fishshell.com/docs/4.0/relnotes.html#completions) does not
  work in scripts generated by complgen.

* Non-regular grammars aren't completed 100% *precisely*. For instance, in case of `find(1)`, `complgen` will
  still suggest `)` even in cases when all `(` have already been properly closed before the cursor.

## License

`complgen`'s source code is covered by [License](LICENSE).  Completion scripts generated by `complgen` are
subject only to [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
