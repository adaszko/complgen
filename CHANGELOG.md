# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

- In zsh, for the grammar `--color=(always|never)` and input `--color=<TAB>`, you will now see `always` and
  `never`, not `--color=always` and `--color=never`.   This generalized to all subword completions of course.
- Descriptions can now contain escaped characters, e.g. a quote character or a backslash.
- More readable parsing errors using the chic Rust crate
- Scraper is now based on a bunch of regexes instead of a complicated parser.  This simplifies the code a lot.

### Changed

- If multiple literals have the same decription, they're now grouped into a single line (this can be
  controlled with `zstyle ':completion:*' list-grouped true/false`)

### Removed

- Single brackets external commands as they're superfluous (replaced with triple brackets ones)

## 0.1.3
### Fixed

- More compact zsh completions in JIT mode
- Zsh,JIT: Only consider completions with descriptions when aligning completions for better appearance
- Remove leftover parenthesis in ZSH subword descriptions

## 0.1.2
### Fixed

- A bug in Bash output where order of arguments to `mapfile` was wrong

## 0.1.1
### Added

- Binary releases on GitHub

### Fixed

- Bug in parser where descriptions were being distributed too generously.

## 0.1.0
### Added

- Add a Homebrew tap formula for automatic updates on new versions.

- "Distributive" descriptions: you can now use a shorthand descriptions:
  `mygrep (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings"`
  and the "use marker..." description is going to apply to both `--color=` and `--color ...` so you don't need to repeat it.

- Improve performance when running on `darcs.usage` example by 4x :O
- Only the completed part appears in ZSH description now instead of the entire shell word (less noise, more like native ZSH)
- Zsh descriptions now look like the ones produced by _arguments for consistency
- Only emit subword processing shell script code if it's actually used in the grammar to conserve space
- Write ZSH completion each on separate lines if any contains a description to better mimic what _arguments does
- New CLI subcommand: `complgen check` that checks the passed grammar and doesn't write annything to disk.

### Fixed

- In Bash integration script, `grep --color=<TAB>` didn't complete anything.

- Bug in JIT mode: Completing subdirectory files wasn't working due to the entered prefix not being passed
  properly.

- Output proper completion prefix in subword completion in shell integration (JIT mode).  Even though
  completions were produced by complgen, shells were filtering them out due to lack of a proper prefix.
