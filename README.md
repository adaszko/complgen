## Value Proposition

`complgen` allows you to generate completion scripts for all major shells from a *single*, concise EBNF-like
grammar.  It compiles the grammar down to a standalone bash/fish/zsh shell script that can be distributed on
its own.  As a separate use case, it can also produce completions directly on stdout, which is meant to be
used in interactive shells (see below).

`complgen` takes flags to complete from grammar files.  Ideally, the grammar files are meant to be developed
and versioned along with the completed command line tool to avoid version mismatches.  There's nothing
stopping you however from writing the grammar file yourself, optionally tailoring it for your most-frequent
use cases, sort of like shell aliases on steroids.

## Demo

[![asciicast](https://asciinema.org/a/SAH1uGqgwBEhyRV7G6Zasu45y.svg)](https://asciinema.org/a/SAH1uGqgwBEhyRV7G6Zasu45y)

## Usage

There are two ways to use complgen:

### 1. To generate standalone completion scripts for bash/fish/zsh:

This mode is most useful if you're a CLI tool author and want to ship shell completions in your installation
package.

```
$ complgen aot --bash-script grep.bash usage/small.usage
$ bash
$$ source grep.bash
$$ grep --color <TAB>
always auto never
```

Note: ZSH also supports automatic loading of completion scripts.  It is enough to place the generated script
at one of directories listed in the `$fpath` variable.


### 2. To generate completions on stdout by compiling the grammar "just-in-time":


This mode is useful if you're command line user and want to improve the CLI experience on *your* machine by
either implementing a missing autocompletion for a specific CLI tool, or override the default one with a one
better tailored for your needs and usage patterns.  Or you simply want to iterate quickly on a `.usage` file
before you compile it to a shell script.

```
$ complgen jit usage/small.usage bash -- --color
always
auto
never
```

The just-in-time mode is intended to be further integrated with shells so that it provides completions
directly from grammars, bypassing compilation and `source`ing completion shell script files.

Note that it is assummed the `.usage` file stem is the same as the completed command name, so to complete
`grep` command, its grammar needs to land in `grep.usage`.

### Bash Integration

Note: This assumes you have `bash-completion` OS-level package installed and it's been sourced!  It often
boils down to `apt install bash-completion; source /etc/bash_completion` or `brew install bash-completion;
source /opt/homebrew/etc/profile.d/bash_completion.sh`, depending on your OS.  Without this package, scripts
generated by `complgen` are not able to correctly process command lines containing containing characters like
`=`, `:`, `@`, or any other from `$COMP_WORDBREAKS`.

See also https://github.com/git/git/commit/da48616f1df51ff43acc64cdf8966f7b72142a11

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.bashrc`:

<details>

```bash
for path in ~/.config/complgen/*.usage; do
    stem=$(basename "$path" .usage)
    eval "
_complgen_jit_$stem () {
    local words cword
    _get_comp_words_by_ref -n \"\$COMP_WORDBREAKS\" words cword
    local prefix="\${words[\$cword]}"
    mapfile -t COMPREPLY < <(complgen jit \"$HOME/.config/complgen/${stem}.usage\" bash --comp-wordbreaks=\"\$COMP_WORDBREAKS\" --prefix=\"\$prefix\" -- \"\${words[@]:1:\$cword-1}\")
    return 0
}
"
    complete -o nospace -F _complgen_jit_$stem "$stem"
    unset stem
done
```
</details>

### Fish Integration

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.config/fish/config.fish`:

<details>

```fish
function _complgen_jit
    set --local COMP_LINE (commandline --cut-at-cursor)
    set --local COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end
    set --local usage_file_path $argv[1]
    set --local prefix $COMP_WORDS[$COMP_CWORD]
    set --local last (math $COMP_CWORD - 1)
    if test $last -lt 2
        set words
    else
        set words $COMP_WORDS[2..$last]
    end
    set -l fn (mktemp -q '/tmp/complgen.fish.XXXXXX')
    complgen jit $usage_file_path fish --prefix="$prefix" -- $words >$fn
    source $fn
    __complgen_jit "$prefix"
    rm -- "$fn"
end

for path in ~/.config/complgen/*.usage
    set --local stem (basename $path .usage)
    complete --command $stem --no-files --arguments "(_complgen_jit ~/.config/complgen/$basename.usage)"
end
```
</details>

### Zsh Integration

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.zshrc`:

<details>

```zsh
_complgen_jit () {
    local stem=$1
    local -a w=("${(@)words[2,$CURRENT-1]}")
    local zsh_code=$(complgen jit ~/.config/complgen/${stem}.usage zsh --prefix="$PREFIX" -- "${w[@]}")
    eval $zsh_code
    return 0
}

for f in $HOME/.config/complgen/*.usage(N); do
    local stem=$f:t:r
    compdef "_complgen_jit $stem" $stem
done
```

</details>

## Installation

### From source (all OSes)

```sh
cargo install --git https://github.com/adaszko/complgen complgen
```

### From Homebrew (macOS)

```
brew tap adaszko/complgen https://github.com/adaszko/complgen-homebrew-tap.git
brew install adaszko/complgen/complgen
```

### Downloading binaries (Linux, macOS)

Just `wget` a binary for your architecture from [the releases
page](https://github.com/adaszko/complgen/releases), `chmod a+x` the downloaded file and you're good to go.
The Linux binaries are linked against [musl libc](http://musl.libc.org/), so they should work on any Linux
distribution.

## Syntax

See the [`examples` subdirectory](examples/) for simple examples and [`usage` subdirectory](usage/) for more
involved ones.

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

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

### Filenames completion

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

Limitations apply.  See the [Limitations section](#Limitations).

### Descriptions

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

### External commands

It is possible to use entire shell commands as a source of completions:

```
cargo {{{ rustup toolchain list | cut -d' ' -f1 | sed 's/^/+/' }}};
```

The stdout of the pipeline above will be automatically filtered by the shell based on the prefix entered so
far.

Limitations apply.  See the [Limitations section](#Limitations).

##### The `$1` parameter

Sometimes, it's more efficient to take into account the entered prefix in the shell command itself.  For all
three shells (bash, fish, zsh), it's available in the `$1` variable:

```
cargo {{{ rustup toolchain list | cut -d' ' -f1 | grep "^$1" | sed 's/^/+/' }}};
```

Note that in general, it's best to leave the filtering up to the executing shell since it may be configured to
perform some non-standard filtering.  zsh for example is capable of expanding `/u/l/b` to `/usr/local/bin`.

##### Descriptions

Externals commands are also assumed to produce descriptions similar to those described in the [section
above](#descriptions-aka-completion-hints).  Their expected stdout format is a sequence of lines of the form

```
COMPLETION\tDESCRIPTION
```

For fish and zsh, the `DESCRIPTION` part will be presented to the user.  Under bash, only the `COMPLETION`
part will be visible.  All external commands nonetheless need to take care as to *not* produce superfluous
`\t` characters that may confuse the resulting shell scripts.

### Specialization

In order to make use of shell-specific completion functions, `complgen` supports a mechanism that allows for
picking a specific nonterminal expansion based on the target shell.  To use an example, all shells are able to
complete a user on the system, although each has a different function for it.  We unify their interface under
the nonterminal `<USER>` using few `nonterminal@shell` definitions:

```
cmd <USER>;
<USER@bash> ::= {{{ compgen -A user "$1" | sort | uniq }}}; # bash produces duplicates for some reason
<USER@fish> ::= {{{ __fish_complete_users "$1" }}};
<USER@zsh> ::= {{{ _users }}};
```

### `--option=ARGUMENT` and subwords

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

### Fallback Completions

This feature is currently work-in-progress, here's where it's supported already:

| Shell | jit | aot |
|-------|-----|-----|
| bash  | ✅  | ❌  |
| fish  | ✅  | ❌  |
| zsh   | ✅  | ❌  |

If you do `git <TAB>` in most shells you're presented with a list of git subcommands.  Even though git accepts
a bunch of global options (`--help`, `--version`, etc.), they don't show up there (sic!).  That's a special
mechanism intended for reducing clutter.  Under complgen, the same effect is achieved via a construction
called fallbacks, which are represented in a grammar with the double bar operator (`||`):

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

* `{{{ ... }}}` is only allowed at tail positions, where it doesn't lead to matching against an arbitrary
  output of an external command:
    * OK: `cmd {{{ echo foo }}} {{{ echo bar }}};`
    * ERROR: `cmd ({{{ echo foo }}} | {{{ echo bar }}});`

        We'd have to match against the output of `{{{ echo foo }}}` at *compilation* time to determine which
        branch to take, which is impossible to do in general as `echo foo` might as well have been an
        arbitrary shell command.

    * OK: `cmd (foo | {{{ echo bar }}});`
    * ERROR: `cmd ({{{ echo foo }}} || {{{ echo bar }}});`

        Reason: Same as for `|` above.

    * OK: `cmd (foo || {{{ echo bar }}});`
    * OK: `cmd [{{{ echo foo }}}] foo`;

        The entire commands always gets tokenized into shell words as a first step, so it's possible to tell
        where the output of `{{{ echo foo }}}` ends.  Note that two `foo` completions are produced here, then
        they're deduplicated, and finally, only one `foo` is offered to the user.

    * OK: `cmd {{{ echo foo }}}... foo baz`;

* Within subwords, `{{{ ... }}}` is only allowed at tail position, where it doesn't lead to matching against
  an arbitrary output of an external command:
    * ERROR: `{{{ git tag }}}..{{{ git tag }}}`

        Impossible to guess at compilation time where the output of the first `{{{ git tag }}` ends and *our*
        `..` begins.

    * OK: `--option={{{ echo foo }}}`
    * ERROR: `{{{ echo foo }}}{{{ echo bar }}}`

        Impossible to guess at compilation time where the output of the first `{{{ echo foo }}` ends the
        second `{{{ echo bar }}}` begins.

* The limitations above also apply to predefined nonterminals (`<PATH>`, `<DIRECTORY>`, etc.) since they're
  internally implemented as external commands.

 * Bash requires `bash-completion` OS package to be installed because completion scripts produced by
   `complgen`, call shell functions from that package at *completion* time.  This is necessary to work around
   Bash's default behavior of [breaking shell words on any character present in the
   `$COMP_WORDBREAKS`](https://stackoverflow.com/a/12495480) environment variable.

 * Under Fish, if your grammar tokens contain [one of the special
   characters](https://github.com/fish-shell/fish-shell/blob/408ab860906fbf6e08f314bea982220fdee3428e/src/complete.cpp#L183),
   the inserted completion won't end in a space indicating full completion.  See also [Not adding space after
   dot at completion time · Issue #6928](https://github.com/fish-shell/fish-shell/issues/6928)

* Non-regular grammars aren't completed 100% *precisely*. For instance, in case of `find(1)`, `complgen` will
  still suggest `)` even in cases when all `(` have already been properly closed before the cursor.

## Keeping abreast

Best way is to watch GitHub [releases](https://github.com/adaszko/complgen/releases).

