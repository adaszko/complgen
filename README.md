# Value Proposition

`complgen` allows you to generate completion scripts for all major shells (bash, zsh, fish) from a *single*,
simple grammar.  It's inspired by [compleat](https://github.com/mbrubeck/compleat/) but instead of requiring
for the `compleat` executable to be available at completion time, it compiles the grammar into a standalone
shell script that can be distributed independently.  You can, for instance, generate completion scripts for
your command line tool on CI and package them along with your tool.  No additional software needs to be
installed in order to use your custom completions.

# Syntax

The grammar is based on [compleat](https://github.com/mbrubeck/compleat/blob/master/README.markdown#syntax)'s one:

 * `a b` matches `a` followed by `b`.
 * `a b | c` matches either `a b` or `c`.
 * `[a]` matches zero or one occurrences of `a`.
 * `a ...` matches one or more occurrences of `a`.
 * `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

 * `a (b | c)` matches `a` followed by either `b` or `c`.
 * `(a | b) ...` matches `a` or `b` followed by any number of additional
   `a` or `b`.

Patterns may also include *variables*:

 * `name ::= expression;` defines a new production that can be referred to from other productions via `<name>`
   syntax.  Referring to a production recursively is not supported.

 * `name = { shell-command... }` defines a variable that uses a shell command to generate suggested
   completions.  The shell command should print one suggested completion per line.  The `$COMP_LINE` and
   `$COMP_CWORD` environment will contain the input line and the current word being completed.

 * If no value is defined for `name`, then the pattern `<name>` will match any word.

# Roadmap

 * Implement DFA minimization via [Hopcroft's algorithm](https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm)
 * Add producing the completion script based on the NFA in order to reduce completion script size
 * Add an "interpreter mode" that reads `*.usage` files on-demand but requires complgen to be installed on the user's machine

# Limitations

 * Grouping single character options into a single shell parameter isn't supported, e.g. `tar -xvf`.  You need
   to pass each option in a separate shell argument instead: `tar -x -v -f`

 * Passing option arguments using `=` is not supported.  E.g. `--foo=bar` doesn't work, but `--foo bar` does.
