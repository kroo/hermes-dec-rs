# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- CFG module with modular architecture (block, builder, analysis, visualization, regions)
- OXC AST integration for JavaScript code generation
- Comprehensive CLI tools for decompilation and analysis
- GitHub Issues-based development tracking system

### Changed
- Migrated from SWC to OXC for AST handling
- Refactored CFG module into separate modules for parallel development
- Updated project structure for better maintainability

### Fixed
- Compiler errors and warnings post-refactoring
- Type compatibility issues between modules
- Test suite updates for new architecture

## [0.1.0] - 2024-01-XX

### Added
- Initial project setup
- HBC parsing infrastructure
- Basic CFG construction
- Test framework
- CLI interface

[Unreleased]: https://github.com/kroo/hermes-dec-rs/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/kroo/hermes-dec-rs/releases/tag/v0.1.0 