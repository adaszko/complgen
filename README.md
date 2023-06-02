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

 * The generated script can be huge because the DFA isn't optimal (yet).

# Syntax

See the [subdirectory](usage/) for examples.

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one.

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

 * Grouping single character options into a single shell parameter isn't supported, e.g. `tar -xvf`.  You need
   to pass each option in a separate shell argument instead: `tar -x -v -f`

 * Passing option arguments using `=` is not supported.  E.g. `--foo=bar` doesn't work, but `--foo bar` does.

# Roadmap

 * Implement DFA minimization (The Dragon Book, 3.9.6 Minimizing the Number of States of a DFA)

 * Reuse Fish's parsing of man pages to generate completions (https://github.com/fish-shell/fish-shell/blob/946ecf235c002cff596fbbb2c03f9693c30744da/share/tools/create_manpage_completions.py).

 * Show completion hints in ZSH and Fish

 * `name ::= expression;` defines a new production that can be referred to from other productions via `<name>`
   syntax.  Referring to a production recursively won't be supported as that would take us outside of regular languages.

 * `name = { shell-command... }` defines a variable that uses a shell command to generate suggested
   completions.  The shell command should print one suggested completion per line.  The `$COMP_LINE` and
   `$COMP_CWORD` environment will contain the input line and the current word being completed.

 * If no value is defined for `name`, then the pattern `<name>` will match any word.

 * End-to-end tests that excercise the generation completion scripts and check that they behave properly.
