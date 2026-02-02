## Value Proposition

`complgen` compiles man-page grammars into standalone completion scripts.  One grammar file compiles into
scripts for most popular shells, freeing you from having to reimplement and maintain the same logic for each
shell separately.  See [examples](examples/).

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
cargo install --git https://github.com/adaszko/complgen --tag v0.7.3 complgen
```

## Syntax

See the [`examples` subdirectory](examples/).

A first approximation of a grammar (not guaranteed to work!) can be produced by piping target program's usage
info onto `complgen --scrape` as below.  Note that all scraping is meant to do is to save you from too much
typing.  The produced grammar will still need to be fleshed out by hand.

```
$ grep --help | complgen --scrape
 | (-E | --extended-regexp) "PATTERNS are extended regular expressions"
 | (-F | --fixed-strings) "PATTERNS are strings"
 | (-G | --basic-regexp) "PATTERNS are basic regular expressions"
[...]
```

***

complgen's grammar is based on
[compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

A grammar is a series of lines separated by a semicolon (`;`).  Each line either represents a single variant
of invoking the completed command (e.g. `git status; git log`) or is a nonterminal definition (`<FOO> ::=
bar`).

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

### Filename Completion

There's a small set of predefined nonterminals that are handled specially by `complgen`:

| Name          | bash | fish | zsh | pwsh | Description |
|---------------|------|------|-----|------|-------------|
|`<PATH>`       | ✅   | ✅   | ✅  | ✅   | file or directory path |
|`<DIRECTORY>`  | ✅   | ✅   | ✅  | ✅   | directory path |

These can still be redefined in the grammar (`<PATH> ::= ...`), in which case their predefined meaning gets
overridden.

### Completion Descriptions (fish/zsh/pwsh)

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

### Sourcing Completions from External Commands

It is possible to produce completions based on an external command output:

    cmd {{{ echo foo; echo bar; echo baz; echo quux }}};

```
bash$ cmd <TAB>
bar   baz   foo   quux
```

Command's stdout is automatically filtered by the respective shell, based on the prefix entered so far.

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

### Disambiguate Commands with a Regex

Some grammars like

```
cmd ({{{ echo foo }}} lhs | {{{ echo bar }}} rhs | baz);
```

will get rejected at compilation time:

```
cmd.usage:1:6:error: Ambiguous grammar
  |
1 | cmd ({{{ echo foo }}} lhs | {{{ echo bar }}} rhs | baz);
  |      ^^^^^^^^^^^^^^^^ matching can't tell apart this
  |
cmd.usage:1:29:error
  |
1 | cmd ({{{ echo foo }}} lhs | {{{ echo bar }}} rhs | baz);
  |                             ^^^^^^^^^^^^^^^^ from this
  |
```

It is due to complgen not being able to tell which of the `|` branches to take on inputs like `$ cmd foo
<TAB>`.  It may be apparent to you it should complete to `$ cmd foo lhs` but in fact, external commands like
`{{{ echo foo }}}` or `{{{ echo bar }}}` are completely opaque to complgen.  The way to salvage like grammars
is to declare the command's stdout matches a certain regex:

```
cmd ({{{ echo foo }}}@bash"foo" lhs | {{{ echo bar }}}@bash"bar" rhs | baz);
```

Now the completion proceeds as intuited: `cmd foo <TAB>` becomes `cmd foo lhs`.


### Target Shell-specific Behavior

External commands quickly degrade into necessitating shell-specific syntax.  complgen provides support to
conditionally choose the command based on the target shell.

To use an example: all shells are able to complete users present on the system although each has a different
function for it:

```
cmd <USER>;
<USER@bash> ::= {{{ compgen -A user "$1" | sort | uniq }}}; # produce candidates on stdout under bash
<USER@fish> ::= {{{ __fish_complete_users "$1" }}}; # produce candidates on stdout under fish
<USER@zsh> ::= {{{ IPREFIX="$2" PREFIX="$1" _users }}}; # produce candidates via compadd and friends under zsh
<USER@pwsh> ::= {{{ Get-LocalUser | Where-Object { $_.Name -like "$prefix*" } | ForEach-Object { $_.Name } }}}; # PowerShell
```

complgen will pick the right definition of `<USER>` depending on what you're compiling the grammar to.

### Completing Within Shell Words

It's possible to match not only entire shell words, but also *within* words, using largely the same grammar
syntax as for matching entire words, barring few spaces here and there.  The most common application is to
handle option arguments (e.g. `--option=ARGUMENT`):

```
grep --color=(always | never | auto);
```

The same mechanism works for more complicated things:

```
strace -e <EXPR>;
<EXPR> ::= [<qualifier>=][!]<value>[,<value>]...;
<qualifier> ::= trace | read | write | fault;
<value> ::= %file | file | all;
```

The above grammar was pulled straight out of [`strace` man
page](https://man7.org/linux/man-pages/man1/strace.1.html#OPTIONS), illustrating how complgen follows the de
facto standard man pages notation.


### Controlling What Completions Get Shown First

If you do `git <TAB>` under the default completion that comes with git, you get something like:

```
bash$ git <TAB>
absorb              bisect              ci                  credential-gcloud   fork                help                mv                  reflog              revert              show                submodule           whatchanged
ad                  blame               citool              describe            format-patch        init                notes               refs                reword              show-branch         sw                  worktree
add                 br                  clang-format        diff                fsck                instaweb            prune               remote              rewrite             sparse-checkout     switch
am                  branch              clean               difftool            gc                  lfs                 pull                repack              rm                  stage               tag
amend               bundle              clone               dlog                gitk                log                 push                replace             root                stash               touch
apply               checkout            co                  fetch               grep                maintenance         range-diff          request-pull        scalar              stash-all           tree
archive             cherry              commit              filter-repo         gui                 merge               rebase              reset               send-email          stash-unstaged      uncommit
backfill            cherry-pick         config              forgit              head                mergetool           recent              restore             shortlog            status              wdiff
```

So even though git accepts many global options, they don't show up here!  If OTOH you do `git --<TAB>`
instead:

```
bash$ git --<TAB>
--bare                 --exec-path=           --help                 --info-path            --namespace=           --no-replace-objects   --version
--exec-path            --git-dir=             --html-path            --man-path             --no-pager             --paginate             --work-tree=
```

You get presented with options in place of subcommands.  It's a useful mechanism that allows to assigning
levels of priority to possible choices, surfacing the most frequently used ones.

Under complgen, the same effect is achieved via
_fallbacks_ (`||`).  An abridged version version of the above would look like below:

```
mygit (<SUBCOMMAND> || <OPTION>);
<SUBCOMMAND> ::= fetch | add | commit | push;
<OPTION> ::= --help | --version;
```

The effect:

```
bash$ mygit <TAB>
add      commit   fetch    push

bash$ mygit --<TAB>
--help      --version
```

In the 2nd case (`mygit --<TAB>`), the completion script first tries to match the input (`--`) against
`<SUBCOMMAND>` and failing that, proceeds to try matching the part on the right hand side of the fallback
operator `||`, i.e. `<OPTION>`.

Note that `||` behaves exactly like the regular `|` in when it comes to matching.  Where its behavior diverges
is with respect to completion.  Where `|` variants would offer all alternative branches at once, `||` tries to
match variants from left to right, stopping at the first branch that matches current user input.

`||` has the lowest priority of all operators, so the grammar above might have been written without any use of
`<NONTERMINALS>`.  They're there only for readability sake.

## Caveats

* Bash requires `bash-completion` OS package to be installed because completion scripts produced by
  `complgen`, call shell functions from that package at *completion* time.  This is necessary to work around
  Bash's default behavior of [breaking shell words on any character present in the
  `$COMP_WORDBREAKS`](https://stackoverflow.com/a/12495480) environment variable.

* [Fish 4.0 fuzzy subsequence filtering](https://fishshell.com/docs/4.0/relnotes.html#completions) does not
  work in scripts generated by complgen.

* PowerShell completions require PowerShell Core 7+ (pwsh). Windows PowerShell 5.x is not supported.

 * You (as the `.usage` author) are responsible for making sure the nontail regexes don't overlap (i.e. they
   match uniquely).  In case of an overlap, it isn't specified which regex matches first, which may lead to
   unexpected completion behavior.

* The tail commands limitation applies to predefined nonterminals (`<PATH>`, `<DIRECTORY>`, etc.) since
  they're internally implemented as external commands.

* Non-regular grammars aren't completed 100% *precisely*. For instance, in case of `find(1)`, `complgen` will
  still suggest `)` even in cases when all `(` have already been properly closed before the cursor.

## License

`complgen`'s source code is covered by [License](LICENSE).  Completion scripts generated by `complgen` are
subject only to [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
