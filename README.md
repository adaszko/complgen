## Value Proposition

`complgen` allows you to generate completion scripts for all major shells (bash, zsh, fish) from a *single*,
concise BNF-like grammar.  It's inspired by [compleat](https://github.com/mbrubeck/compleat/) but instead of
requiring for the `compleat` executable to be available at completion time, it compiles the grammar down to a
standalone shell script that can be distributed on its own.  If you're an author of a CLI tool, you can
generate shell scripts for your command line tool on CI, and package them along with your command line tool.
No additional software needs to be installed in order to use those custom completions.

## Demo

[![asciicast](https://asciinema.org/a/rRfe9MmZpzBRQIb21LPADWb6n.svg)](https://asciinema.org/a/rRfe9MmZpzBRQIb21LPADWb6n)

## Usage

There are two ways to use complgen:

1. To generate standalone completion scripts for bash and/or fish:

```
$ compile --bash-script grep.bash usage/small.usage
$ bash
$$ source grep.bash
$$ grep --color <TAB>
always auto never
```

2. To generate completions on stdout by interpreting the grammar "just-in-time" (just like
[compleat](https://github.com/mbrubeck/compleat/) works):

```
$ complgen complete usage/small.usage -- --color
always
auto
never
```

The just-in-time mode is intended to be further integrated with shells so that it provides completions
directly from grammars, bypassing completion scripts.  This improves readability and reduces maintenance
effort.

## Installation

```
cargo install --git https://github.com/adaszko/complgen complgen
```

## Rationale

Although various libraries exist for parsing command line arguments that can generate shell completions for
you (e.g. clap), `complgen` has the advantage of being just a command line tool and thus not being tied to any
particular implementation language.  `complgen` is also capable of completing based on external shell commands
output, e.g. `{ cat /etc/passwd | grep -v '^#' | cut -d: -f1 }` to complete users on the system.

## Project Status

Generates working completion scripts for `bash` and `fish` as well as JIT-completes although it still probably
is a bit buggy (please report!).  Zsh shell can use Bash completion script via its builtin compatibility mode:

```Zsh
% autoload bashcompinit
% bashcompinit
% source /path/to/your/bash_completion_file
```

## Syntax

See the [subdirectory](usage/) for examples.

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

A grammar is a series of lines terminated by a semicolon (`;`).  Each line (roughly) represents a single
variant of invoking the completed command.

 * `a b` matches `a` followed by `b`.
 * `a b | c` matches either `a b` or `c`.
 * `[a]` matches zero or one occurrences of `a`.
 * `a ...` matches one or more occurrences of `a`.
 * `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

## Limitations

 * Passing option arguments using `=` is not currently supported.  E.g. `--foo=bar` doesn't work, but `--foo
   bar` does.

 * Grouping single character options into a single shell parameter isn't supported, e.g. `tar -xvf` (unless
   you manually enumerate all the combinations in the grammar which isn't very practical).  You need to pass
   each option in a separate shell argument instead: `tar -x -v -f`

 * Non-regular grammars are not supported, e.g. `find(1)`'s arguments can't be completed precisely by
   complgen.

## Roadmap

 * Grammar validation:
    * Warn about undefined variables
    * Warn about duplicate variables

 * Show completion hints (descriptions) in ZSH and Fish (Bash does not support them)

 * Support copying pieces of shell scripts that can define shell functions callable from inline shell commands
   (`{[...]}`):
    * `@bash {{{ ... }}}`
    * `@fish {{{ ... }}}`
    * `@zsh {{{ ... }}}`

 * End-to-end tests that excercise the generated completion scripts and check that they behave properly.
