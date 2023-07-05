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
$ complgen complete usage/small.usage -- --color
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
    local stem=$(basename "$path" .usage)
    eval "
_complgen_jit_$stem () {
    local -a completions=(\$(complgen complete \"$HOME/.config/complgen/${stem}.usage\" bash \$((COMP_CWORD - 1)) -- \${COMP_WORDS[@]:1}))
    completions=\${completions[@]}
    COMPREPLY=(\$(compgen -W \"\$completions\" -- "\${COMP_WORDS[\$COMP_CWORD]}"))
    return 0
}
"
    complete -F _complgen_jit_$stem "$stem"
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

## Rationale

Although various libraries exist for parsing command line arguments that can generate shell completions for
you (e.g. clap), `complgen` has the advantage of being just a command line tool and thus not being tied to any
particular implementation language.  `complgen` is also capable of completing based on external shell commands
output, e.g. `{ cat /etc/passwd | grep -v '^#' | cut -d: -f1 }` to complete users on the system.

## Syntax

See the [subdirectory](usage/) for examples.

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

A grammar is a series of lines terminated by a semicolon (`;`).  Each line either represents a single variant
of invoking the completed command or is a nonterminal definition.

 * `a b` matches `a` followed by `b`.
 * `a b | c` matches either `a b` or `c`.
 * `[a]` matches zero or one occurrences of `a`.
 * `a ...` matches one or more occurrences of `a` (**WARNING**: `a...` will match the literal `a...`, not one or more `a`!).
 * `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

### Filenames completion

There's a couple of predefined nonterminals that are handled specially by `complgen`:

 * `<PATH>` is completed as a file or directory path
 * `<DIRECTORY>` is completed as a directory path

These nonterminals can be defined in the grammar in the usual way (`<PATH> ::= ...`) in which case they lose
their predefined meaning.

### Descriptions (a.k.a. completion hints)

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

Sometimes, it's more efficient to take into account the entered prefix in the shell commands itself.  For all
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

## Limitations

 * Passing option arguments using `=` is not currently supported.  E.g. `--foo=bar` doesn't work, but `--foo
   bar` does.

 * Grouping single character options into a single shell parameter isn't supported, e.g. `tar -xvf` (unless
   you manually enumerate all the combinations in the grammar which isn't very practical).  You need to pass
   each option in a separate shell argument instead: `tar -x -v -f`

 * Non-regular grammars are not supported, e.g. `find(1)`'s arguments can't be completed 100% *precisely* by
   complgen—`complgen` won't accurately suggest a matching `\)` for example.  That doesn't mean it isn't
   useful though.  It's still perfectly capable of suggesting every argument `find(1)` accepts.
