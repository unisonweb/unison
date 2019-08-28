The Unison language
======

[![Build Status](https://travis-ci.org/unisonweb/unison.svg?branch=master)](https://travis-ci.org/unisonweb/unison)

[Unison](http://unisonweb.org) is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with a unique ability to describe entire distributed systems with a single program. Here's a simple example:

```Haskell
-- comments start with `--`
-- alice : Node, bob : Node

x = factorial 6
Remote.transfer alice
y = foo x -- happens on `alice` node
Remote.transfer bob
bar x y -- happens on `bob` node
```
The `Remote.transfer` function introduces a "remote effect", where computation may proceed on multiple Unison nodes:

* The `Remote.transfer alice` transfers control of the computation to the `alice` node.
* The `foo x` call happens on the `alice` node.
* At each transfer, any required definitions (such as `foo` and `x`) will be dynamically deployed to the `alice` node and cached for future use.
* The `Remote.transfer bob` transfers control of the rest of the computation to the `bob` node.
* The `bar x y` computation happens on the `bob` node. Again, any needed definitions (`bar`, `x`, and `y`) will be dynamically deployed to the `bob` node.

This dynamic transfer / deployment of arbitrary computations is possible because definitions in Unison are identified by a cryptographic hash of their content, _including the hashes of all dependencies_ (the hash is also "nameless" as it isn't affected by naming of variables). To transfer a computation, we send it to the recipient, and the recipient checks to see if the computation references any unknown hashes. Any unknown hashes are synced to the recipient before the transfer completes and the computation proceeds.

If you'd like to learn more about the project, the talk [How to write a search engine in 15 lines of code](http://unisonweb.org/2016-10-12/search.html) has more of an introduction to the language. For a more complete overview of the syntax look at the [unison language reference](/unison-src/tests/language-reference.u).

Since Unison isn't terribly useful in its current form, the rest of this README will focus on stuff that will be of interest for potential contributors, namely, how to build the code, and a brief tour of the (fairly small but action-packed) codebase. If you're just interested in the project and want to follow along with the progress, [unisonweb.org](http://unisonweb.org) is the place to go, or you can also say hello or lurk [in the Slack chat](http://tiny.cc/unisonslack).

Still here? All right then! Let's get to it.

A brief code tour
-----
First, clone unison with `--recursive`:
`git clone --recursive https://github.com/unisonweb/unison.git`

Next, a bit of orientation. Here's the directory structure:

* `editor-support/` includes some very basic and probably incomplete text edit support (read: syntax highlighting)
* `yaks/` has subprojects for various utilties not specific to Unison (the result of ["yak-shaving"](https://en.wiktionary.org/wiki/yak_shaving)). Once mature, each of these might be moved to independent projects and published on Hackage.
* `parser-typechecker/` has the meat: the Unison syntax tree, parser, typechecker, and runtime. Depends on `yaks/`

Building using Stack
-----

If these instructions don't work for you or are incomplete, please file an issue.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform.  (Hint: `brew update && brew install stack`)

```sh
$ git clone --recursive https://github.com/unisonweb/unison.git
$ cd unison
$ stack --version # we'll want to know this version if you run into trouble
$ stack build && stack exec tests && stack exec unison
```

Note: If you get this error:

```
Stack looks for packages in the directories configured in the 'packages' and 'extra-deps' fields defined in your stack.yaml
The current entry points to <root>/yaks/haskeline/ but no .cabal or package.yaml file could be found there.
```
then your local git repo is older than the haskeline submodule dependency; use this to get it:
```
git submodule init
git submodule update
```

See [`development.markdown`](development.markdown) for a list of build commands you'll likely use during development.

A brief tour of the Haskell code
-----

In the `parser-typechecker/` project:

* `Unison.Term` and `Unison.Type` have the syntax trees for terms and types. In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations.
* `Unison.Parsers` has the main entry point for the parser.
* `Unison.Typechecker.Context` is the implementation of the typechecker, and `Unison.Typechecker` has the "public interface to the typechecker" and some convenience functions. There isn't a lot of code here (about 700 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
