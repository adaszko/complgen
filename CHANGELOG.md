# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New CLI subcommand: `complgen check` that checks the passed grammar and doesn't write annything to disk.
- Write ZSH completion each on separate lines if any contains a description to better mimic what _arguments does
- Only emit subword processing shell script code if it's actually used in the grammar to conserve space
- Zsh descriptions now look like the ones produced by _arguments for consistency
- Only the completed part appears in ZSH description now instead of the entire shell word (less noise, more like native ZSH)
- Improve performance when running on `darcs.usage` example by 4x :O

### Fixed

- Bug in JIT mode: Completing subdirectory files wasn't working due to the entered prefix not being passed
  properly.

- Output proper completion prefix in subword completion in shell integration (JIT mode).  Even though
  completions were produced by complgen, shells were filtering them out due to lack of a proper prefix.
