# Value Proposition

`complgen` allows you to generate completion scripts for all major shells (bash, zsh, fish) from a *single*,
simple grammar.  It's inspired by [compleat](https://github.com/mbrubeck/compleat/) but instead of requiring
for the `compleat` executable to be available at completion time, it compiles the grammar into a standalone
shell script that can be distributed independently.  You can, for instance, generate completion scripts for
your command line tool on CI and package them along with your tool.  No additional software needs to be
installed in order to perform the shell completion.
