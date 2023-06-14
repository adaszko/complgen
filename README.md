# Value Proposition

`complgen` allows you to generate completion scripts for all major shells (bash, zsh, fish) from a *single*,
concise grammar.  It's inspired by [compleat](https://github.com/mbrubeck/compleat/) but instead of requiring
for the `compleat` executable to be available at completion time, it compiles the grammar down to a standalone
shell script that can be distributed on its own.  If you're an author of a CLI tool, you can for instance
generate completion scripts for your command line tool on CI, and package them along with your command line
tool.  No additional software needs to be installed in order to use those custom completions.

## Usage

There are two ways to use complgen:

1. To generate standalone completion scripts for bash and/or fish:

```
cargo run --release -- compile --bash-script-path darcs.bash --fish-script-path darcs.fish usage/darcs.usage
```

2. To generate completions on stdout by interpreting the grammar "just-in-time" (just like [compleat](https://github.com/mbrubeck/compleat/) works):

```
cargo run --release -- complete usage/darcs.usage [ARGS...]
```

## Rationale

Although various libraries exist for parsing command line arguments that can generate shell completions for
you (e.g. clap), `complgen` has the advantage of being just a command line tool and thus not being tied to any
particular implementation language.  The disadvantage, of course, is that now, parsing and completions are
maintained separately from each other risking divergence.  On balance, it is deemed still worth it.

# Status

 * Generates working completion scripts for `bash` and `fish`.  `zsh` can use `bash` script [via bash
   compatibility mode](https://stackoverflow.com/a/8492043).

# Syntax

See the [subdirectory](usage/) for examples.

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

A grammar is a series of line terminated by a semicolon (;).  Each line (roughly) represents a single variant of
invoking the command.

 * `a b` matches `a` followed by `b`.
 * `a b | c` matches either `a b` or `c`.
 * `[a]` matches zero or one occurrences of `a`.
 * `a ...` matches one or more occurrences of `a`.
 * `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

# Limitations

 * Grouping single character options into a single shell parameter isn't supported, e.g. `tar -xvf` (unless
   you manually enumerate all the combinations in the grammar which isn't very practical).  You need to pass
   each option in a separate shell argument instead: `tar -x -v -f`

 * Passing option arguments using `=` is not supported.  E.g. `--foo=bar` doesn't work, but `--foo bar` does.

 * Non-regular grammars are not supported, e.g. `find(1)`'s arguments can't be completed precisely by
   complgen.

# Roadmap

 * Grammar validation:
    * Warn about undefined variables
    * Warn about duplicate variables

 * `<name> ::= { shell-command... };` defines a variable that uses a shell command to generate suggested
   completions.  The shell command should print one suggested completion per line.  The shell script is a sh
   shell function that is going to be copied to the resulting completion script.  The shell function does not
   take any arguments and should produce all possible completions on stdout.  The actual shell the function is
   run under, is responsible for filtering the results according to the shell settings.

 * Produce [railroad diagrams](https://github.com/lukaslueg/railroad) to ease grammar development.

 * Show completion hints in ZSH and Fish (Bash does not support them): `--invert-match "select non-matching lines"`

 * Automatic completion of standard objects, e.g. <FILE>, <PATH>, etc.
    * e.g. <DIR>, <DIRECTORY> => compgen -A directory [<PREFIX>]
    * <FILE>, <PATH> => compgen -A file [<PREFIX>]

 * End-to-end tests that excercise the generated completion scripts and check that they behave properly.
