The Unison language
===================

[![CI Status](https://github.com/unisonweb/unison/actions/workflows/ci.yaml/badge.svg)](https://github.com/unisonweb/unison/actions/workflows/ci.yaml?query=branch%3Atrunk)
[![Pre-Release Status](https://github.com/unisonweb/unison/actions/workflows/pre-release.yaml/badge.svg)](https://github.com/unisonweb/unison/actions/workflows/pre-release.yaml)

* [Overview](#overview)
* [Building using Stack](#building-using-stack)
* [Language Server Protocol (LSP)](docs/language-server.markdown)
* [Codebase Server](#codebase-server)
* [Configuration](./docs/configuration.md)

![Alt](https://repobeats.axiom.co/api/embed/92b662a65fd842d49cb8d7d813043f5f5b4b550d.svg "Repobeats analytics image")

Overview
--------

[Unison](https://unison-lang.org) is a statically-typed functional language with type inference, an effect system, and advanced tooling. It is based around [a big idea of content-addressed code](https://www.unison-lang.org/learn/the-big-idea/), in which function are identified by a hash of their implementation rather than by name, and code is stored as its AST in a database. This provides a number of benefits:

* No builds. Unison has perfect incremental compilation, with a shared compilation cache that is part of the codebase format. Despite the strong static typing, you are almost never waiting for code to compile.
* Instant, non-breaking renaming of definitions.
* Perfect caching of tests, only rerunning deterministic tests if dependencies changed.
* Semantically-aware version control, avoiding spurious merge conflicts from things like order of imports, whitespace or code formatting differences, and so on.

Unison can be used like any other general-purpose language, or you can use it in conjunction with [Unison Cloud](https://unison.cloud) for building distributed systems.

Here is some sample code:


```Haskell
-- A comment!
-- Function signatures appear before the definition
factorial : Nat -> Nat
factorial n = product (range 1 (n + 1))

-- Signatures can be left off; they will be inferred
List.map f as =
  go acc rem = match rem with
    [] -> acc
    a +: as -> go (acc :+ f a) as
  go [] as

> List.map (x -> x * 10) (range 0 10)
= [0, 10, 20, 30, 40, 50, 60, 70, 80, 90]

> List.map factorial [1,2,3,4]
= [1, 2, 6, 24]
```

Functions arguments are separated by spaces instead of parens and commas. Loops are written using recursion (above the helper function `go` defines a loop). The language supports pattern matching via `match <expr> with <cases>`, which works for lists and also user-defined data types.

Other resources:

* [Learn about the big idea behind Unison](https://www.unison-lang.org/learn/the-big-idea/)
* Check out [the project website](https://unison-lang.org)
* Say hello or lurk [in the Discord chat](https://unison-lang.org/discord)
* Explore [the Unison ecosystem](https://share.unison-lang.org/)
* [Learn Unison](https://www.unison-lang.org/learn/)

Building using Stack
--------------------

If these instructions don't work for you or are incomplete, please file an issue.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform.  (Hint: `brew update && brew install stack`)

```sh
$ git clone https://github.com/unisonweb/unison.git
$ cd unison
$ stack --version # we'll want to know this version if you run into trouble
$ stack build --fast --test && stack exec unison
```

To run the Unison Local UI while building from source, you can use the `/dev-ui-install.sh` script. It will download the latest release of [unison-local-ui](https://github.com/unisonweb/unison-local-ui) and put it in the expected location for the unison executable created by `stack build`. When you start unison, you'll see a url where Unison Local UI is running.

See [`development.markdown`](development.markdown) for a list of build commands you'll likely use during development.

Language Server Protocol (LSP)
------------------------------

View Language Server setup instructions [here](docs/language-server.markdown).

AI Agent Server (MCP)
----------------------

View AI Agent Server setup instructions [here](docs/mcp.md).

Codebase Server
---------------

When `ucm` starts it starts a Codebase web server that is used by the
[Unison Local UI](https://github.com/unisonweb/unison-local-ui). It selects a random
port and a unique token that must be used when starting the UI to correctly
connect to the server.

The port, host and token can all be configured by providing environment
variables when starting `ucm`: `UCM_PORT`, `UCM_HOST`, and `UCM_TOKEN`.

Configuration
-------------

See the documentation for configuration [here](docs/configuration.md)
