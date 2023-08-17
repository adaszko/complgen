## Value Proposition

`complgen` allows you to generate completion scripts for all major shells from a *single*, concise EBNF-like
grammar.  It compiles the grammar down to a standalone bash/fish/zsh shell script that can be distributed on
its own.  As a separate use case, it can also produce completions from a grammar directly on stdout, which is
meant to be used in interactive shells (see below).

## Demo

[![asciicast](https://asciinema.org/a/rRfe9MmZpzBRQIb21LPADWb6n.svg)](https://asciinema.org/a/rRfe9MmZpzBRQIb21LPADWb6n)

## Usage

There are two ways to use complgen:

### 1. To generate standalone completion scripts for bash/fish/zsh:

```
$ complgen compile --bash-script grep.bash usage/small.usage
$ bash
$$ source grep.bash
$$ grep --color <TAB>
always auto never
```

### 2. To generate completions on stdout by compiling the grammar "just-in-time":

```
$ complgen complete usage/small.usage bash 1 -- --color
always
auto
never
```

The just-in-time mode is intended to be further integrated with shells so that it provides completions
directly from grammars, bypassing compilation and `source`ing completion shell script files.

Note that it is assummed the `.usage` file stem is the same as the completed command name, so to complete
`grep` command, its grammar needs to land in `grep.usage` file.

### Bash Integration

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.bashrc`:

```bash
for path in ~/.config/complgen/*.usage; do
    stem=$(basename "$path" .usage)
    eval "
_complgen_jit_$stem () {
    local -a completions=(\$(complgen complete \"$HOME/.config/complgen/${stem}.usage\" bash \$((COMP_CWORD - 1)) -- \${COMP_WORDS[@]:1}))
    local prefix="\${COMP_WORDS[\$COMP_CWORD]}"
    for item in "\${completions[@]}"; do
        if [[ \$item = "\${prefix}"* ]]; then
            COMPREPLY+=("\$item")
        fi
    done
    return 0
}
"
    complete -o nospace -F _complgen_jit_$stem "$stem"
    unset stem
done
```

### Fish Integration

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.config/fish/config.fish`:

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
    complgen complete $usage_file_path fish -- (math $COMP_CWORD - 2) $COMP_WORDS[2..-1]
end

for path in ~/.config/complgen/*.usage
    set --local stem (basename $path .usage)
    complete --command $stem --no-files --arguments "(_complgen_jit ~/.config/complgen/$basename.usage)"
end
```


### Zsh Integration

Assumming your `.usage` files are stored in the `~/.config/complgen` directory, add this to your `~/.zshrc`:

```zsh
_complgen_jit () {
    local stem=$1
    local -a w=("${(@)words[2,$#words]}")
    local zsh_code=$(complgen complete ~/.config/complgen/${stem}.usage zsh $((CURRENT - 2)) -- "${w[@]}")
    eval $zsh_code
    return 0
}

for f in $HOME/.config/complgen/*.usage; do
    local stem=$f:t:r
    compdef "_complgen_jit $stem" $stem
done
```

## Installation

```sh
cargo install --git https://github.com/adaszko/complgen complgen
```

## Syntax

See the [`examples` subdirectory](examples/) for simple examples and [`usage` subdirectory](usage/) for more
involved ones.

Try piping through the `scrape` subcommand to quickly generate grammar skeleton that can be tweaked
further, e.g.:

```
$ grep --help | complgen scrape
[...]
ggrep [<OPTION>] ... <PATTERNS> [<FILE>] ...
-E "are extended regular expressions" | --extended-regexp "are extended regular expressions" <PATTERNS>
-F "are strings" | --fixed-strings "are strings" <PATTERNS>
-G "are basic regular expressions" | --basic-regexp "are basic regular expressions" <PATTERNS>
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

 * `<PATH>` is completed as a file or directory path (bash, fish, zsh)
 * `<DIRECTORY>` is completed as a directory path (bash, fish, zsh)
 * `<PID>` is completed as a process id (fish, zsh)
 * `<USER>` is completed as a user name (bash, fish, zsh)
 * `<GROUP>` is completed as a group name (bash, fish, zsh)
 * `<HOST>` is completed as a hostname (bash, fish, zsh)
 * `<INTERFACE>` is completed as a network interface name (fish, zsh)
 * `<PACKAGE>` is completed as a package name (fish)

These nonterminals can still be defined in the grammar in the usual way (`<PATH> ::= ...`), in which case
their predefined meaning gets overriden.

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
cargo { rustup toolchain list | cut -d' ' -f1 | sed 's/^/+/' };
```

The stdout of the pipeline above will be automatically filtered by the shell based on the prefix entered so
far.

##### The `$1` parameter

Sometimes, it's more efficient to take into account the entered prefix in the shell command itself.  For all
three shells (bash, fish, zsh), it's available in the `$1` variable:

```
cargo { rustup toolchain list | cut -d' ' -f1 | grep "^$1" | sed 's/^/+/' };
```

Note that in general, it's best to leave the filtering up to the executing shell since it may be configured to
perform some non-standard filtering.  zsh for example is capable of expanding `/u/l/b` to `/usr/local/bin`.

##### Triple brackets

To avoid cumbersome escaping, additional triple brackets syntax is also supported:

```
cargo {{{ rustup toolchain list | awk '{ print $1 }' | grep "^$1" | sed 's/^/+/' }}};
```

Its semantics are exactly like the ones of single brackets.

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
<USER@bash> ::= { compgen -A user "$1" | sort | uniq }; # bash produces duplicates for some reason
<USER@fish> ::= { __fish_complete_users "$1" };
<USER@zsh> ::= { _users };
```

### `--option=ARGUMENT` and subwords

It's possible to match not only entire words, but also *within* words themselves, using the same grammar
syntax as for matching entire words.  In that sense, it all fractally works on subwords too.  The most common
application of that general mechanism is to handle equal sign arguments (`--option=ARGUMENT`):

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

Caveats:
 * Fish only allows [a limited set of
   characters](https://github.com/fish-shell/fish-shell/blob/408ab860906fbf6e08f314bea982220fdee3428e/src/complete.cpp#L183)
   within subwords.  Otherwise, it automatically inserts a space character that ends completion of the
   current word thereby completer out of the subword state.
    * [Not adding space after dot at completion time · Issue #6928 · fish-shell/fish-shell · GitHub](https://github.com/fish-shell/fish-shell/issues/6928)

## Limitations

* Non-regular grammars aren't completed 100% *precisely*. For instance, in case of `find(1)`, `complgen` will
  still suggest `)` even in cases when all `(` have already been properly closed before the cursor.

## Related Projects

 * https://docs.rs/clap_complete/
 * https://github.com/kislyuk/argcomplete
 * https://github.com/oilshell/oil/wiki/Shell-Autocompletion
