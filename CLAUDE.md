# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

The Unison programming language compiler and CLI (UCM). A Haskell monorepo implementing a content-addressed functional language where code is stored as ASTs identified by hash rather than by name.

## Build System

Uses [Stack](http://docs.haskellstack.org/) with GHC 9.10.3 (resolver lts-24.21).

```sh
stack build --fast              # Build all packages
stack build --fast --test       # Build and run unit tests
stack test --fast               # Same as above
stack exec unison               # Run the UCM executable
```

### Running Specific Tests

```sh
# Unit tests for a specific package with prefix filter
stack test unison-parser-typechecker --fast --test-arguments my-test-prefix

# Transcript integration tests (in unison-src/transcripts/)
stack exec transcripts
stack exec transcripts -- prefix-of-filename

# CLI integration tests
stack exec cli-integration-tests

# Round-trip pretty-printing tests
stack exec unison -- transcript unison-src/transcripts-round-trip/main.md
```

### Full Pre-Push Validation

```sh
./scripts/test.sh
```

## Code Formatting

Ormolu (version 0.8.0.2). CI auto-commits formatting fixes. Fixity declarations live in `.ormolu`. Disable locally with:

```haskell
{- ORMOLU_DISABLE -}
{- reason for disabling -}
...
{- ORMOLU_ENABLE -}
```

## Architecture

### Package Layers (bottom to top)

**Foundation (`lib/`)**
- `unison-prelude` — common imports and utilities
- `unison-hash` — content-address hash types
- `unison-sqlite` — typed SQLite wrapper for the codebase DB
- `unison-util-*` — data structures (relations, ropes, bytes, diff3, cache)

**Core (`unison-core/`)**
- ABT (Abstract Binding Trees) — the term representation
- Types, Kinds, Names, DataDeclarations
- The foundational AST types everything else builds on

**Codebase Storage (`codebase2/`)**
- `core` — codebase API interfaces
- `codebase-sqlite` — SQLite-backed codebase implementation
- `codebase-sync` — syncing codebases (push/pull)

**Syntax (`unison-syntax/`)**
- Lexer, Parser, surface syntax types

**Typechecker & Compiler (`parser-typechecker/`)**
- `Unison.Typechecker` — bidirectional typechecker with ability inference
- `Unison.KindInference` — kind inference
- `Unison.Builtin` — builtin type/term definitions
- `Unison.FileParsers` — file-level parsing pipeline
- `Unison.PatternMatchCoverage` — exhaustiveness/redundancy checking
- `Unison.Hashing` — computing content hashes for definitions

**Runtime (`unison-runtime/`)**
- `Unison.Runtime` — interpreter/code generation

**Merge (`unison-merge/`)**
- Semantic merge algorithm for content-addressed code

**CLI (`unison-cli/`)**
- UCM commands, codebase operations, LSP server
- `unison-cli-main` — the `unison` executable entrypoint
- `unison-cli-integration` — the `cli-integration-tests` executable

**Share API (`unison-share-api/`, `unison-share-projects-api/`)**
- API clients for Unison Share (the code hosting service)

### Transcript Tests

Markdown files in `unison-src/transcripts/` that interleave UCM commands and expected output. The `transcripts` executable runs them and compares against `*.output.md` files. The `idempotent/` subdirectory contains transcripts that must produce identical output on re-run.

## Haskell Conventions

- Extensions are declared per-package in `package.yaml` (not per-file). Common: `BlockArguments`, `LambdaCase`, `OverloadedStrings`, `GADTs`, `DerivingStrategies`.
- Packages use `hpack` (`package.yaml` → `.cabal`). Don't edit `.cabal` files directly.
- GHC options: `-Wall -fno-warn-name-shadowing`
- The project uses lens extensively (`^?`, `%%~`, `%~`, `<|>`, `&`, `<&>` — fixities in `.ormolu`).

## Nix

A Nix flake is available (`nix develop` enters a dev shell with all tooling). Not required — Stack works standalone.
