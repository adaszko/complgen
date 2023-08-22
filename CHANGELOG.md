# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Bug in JIT mode: Completing subdirectory files wasn't working due to the entered prefix not being passed
  properly.

- Output proper completion prefix in subword completion in shell integration (JIT mode).  Even though
  completions were produced by complgen, shells were filtering them out due to lack of a proper prefix.
