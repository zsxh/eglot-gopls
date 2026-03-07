# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-03-07

### Added
- Add jsonrpc 1.0.24 as a required dependency
- Add `go.test.package` codelens command to run all tests in the current package
- Add `go.test.file` codelens command to run all tests in the current file
- Add `go.benchmark.package` codelens command to run all benchmarks in the current package
- Add `go.benchmark.file` codelens command to run all benchmarks in the current file
- Add testify suite test method support (e.g., `(*MySuite).TestMethod`)
- Add fuzz function support for running tests
- Add `eglot-gopls-test-cover-path` option for coverage profile directory
- Add `eglot-gopls-test-covermode` option for coverage mode (set, count, atomic)
- Add `eglot-gopls-test-timeout` option for test timeout duration
- Add `eglot-gopls-test-flags` option for additional test flags
- Add automatic code coverage for package test runs
- Add suite-to-test mapping cache with file signature-based invalidation
- Add debugging support for testify suite test methods

### Changed
- Update README with installation instructions for eglot-codelens
- Update Commentary section with complete installation instructions
- Mark testify suite debugging as completed

### Fixed


## [0.1.0] - 2026-02-xx

### Added
- Initial release
- Support for `gopls.run_tests` with compilation buffer and error navigation
- Support for `go.test.cursor` and `go.benchmark.cursor` codelens commands
- Support for `go.debug.cursor` with dape integration
- Support for `gopls.run_govulncheck` and `gopls.vulncheck` commands
- Codelens transformation for enhanced test and benchmark commands
