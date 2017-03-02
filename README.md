The Unison platform
======

[![Join the chat at https://gitter.im/unisonweb/unison](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/unison?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/unisonweb/unison.svg?branch=master)](https://travis-ci.org/unisonweb/unison)

[Unison](http://unisonweb.org) is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with a unique ability to describe entire distributed systems with a single program. Here's a simple example:

```Haskell
-- comments start with `--`
-- alice : Node, bob : Node

do Remote
  x = factorial 6
  Remote.transfer alice
  y = foo x -- happens on `alice` node
  Remote.transfer bob
  pure (bar x y) -- happens on `bob` node
```

The `do Remote` introduces a "remote block", where computation may proceed on multiple Unison nodes:

* The `Remote.transfer alice` transfers control of the computation to the `alice` node.
* The `foo x` call happens on the `alice` node.
* At each transfer, any required definitions (such as `foo` and `x`) will be dynamically deployed to the `alice` node and cached for future use.
* The `Remote.transfer bob` transfers control of the rest of the computation to the `bob` node.
* The `bar x y` computation happens on the `bob` node. Again, any needed definitions (`bar`, `x`, and `y`) will be dynamically deployed to the `bob` node.

This dynamic transfer / deployment of arbitrary computations is possible because definitions in Unison are identified by a cryptographic hash of their content, _including the hashes of all dependencies_ (the hash is also "nameless" as it isn't affected by naming of variables). To transfer a computation, we send it to the recipient, and the recipient checks to see if the computation references any unknown hashes. Any unknown hashes are synced to the recipient before the transfer completes and the computation proceeds.

If you'd like to learn more about the project, the talk [How to write a search engine in 15 lines of code](http://unisonweb.org/2016-10-12/search.html) has more of an introduction to the language.

Since Unison isn't terribly useful in its current form, the rest of this README will focus on stuff that will be of interest for potential contributors, namely, how to build the code, and a brief tour of the (fairly small but action-packed) codebase. If you're just interested in the project and want to follow along with the progress, [unisonweb.org](http://unisonweb.org) is the place to go, or you can also say hello or lurk [in the chat room](https://gitter.im/unisonweb/unison).

Still here? All right then! Let's get to it.

A brief code tour
-----

First, a bit of orientation. Here's the directory structure:

* `yaks/` has subprojects for various utilties not specific to Unison (the result of ["yak-shaving"](https://en.wiktionary.org/wiki/yak_shaving)). Once mature, each of these might be moved to independent projects and published on Hackage.
* `shared/` has the Unison syntax tree, parser, typechecker. Depends on `yaks/`
* `node/` has code related to the Unison runtime and codebase editor. Depends on `shared/`
* `editor/` has some prototype work on a semantic editor for Unison. Also depends on `shared/`. It's currently being allowed to bitrot, but may be resurrected at some point soon, or else rewritten.

Building using Stack
-----

If these instructions don't work for you or are incomplete, please file an issue.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, version 1.3 or later, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform.

You'll also need [`xz`](http://tukaani.org/xz/) on your path and also the `libghc-curl-dev` library someplace that stack looks for it. See the [Vagrant box setup](#vagrant) and the [vagrant-provision.sh](vagrant-provision.sh) script for known-good setup steps on linux.

Once that's all done and the `stack` executable is on your path, do:

```sh
$ git clone https://github.com/unisonweb/unison.git
$ cd unison
$ stack --version # make sure this returns 1.3 or later
$ stack setup
$ stack build unison-node
```

See [`development.markdown`](development.markdown) for a list of build commands you'll likely use during development.

A brief tour of the Haskell code
-----

In the `shared/` project:

* `Unison.Term` and `Unison.Type` have the syntax trees for terms and types. In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations.
* `Unison.Parsers` has the main entry point for the parser.
* `Unison.Typechecker.Context` is the implementation of the typechecker, and `Unison.Typechecker` has the "public interface to the typechecker" and some convenience functions. There isn't a lot of code here (about 700 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
* `Unison.Codebase` has and interface and implementation for manipulating a Unison codebase

In the `node/` project, there are currently 2 executables, `container` and `unison`, which you can build via `stack build unison-node`:

* `unison` is a command line tool for editing a Unison codebase. It provides a handful of commands for adding new definitions, and editing or viewing existing definitions.
* `container` has an HTTP interface to the Unison runtime, see `development.markdown` for instructions on how to use. Eventually, the `container` functionality will get folded into the `unison` executable.

That's all for now!

### Appendix

#### <a id="vagrant"></a>VM build instructions

If you'd prefer to develop on a VM, you can still use your local text editor or IDE of choice for Haskell editing, since the filesystem is shared between the VM and your local machine.

Here are instructions for this route:

* Download and install [Vagrant](https://www.vagrantup.com/).
* Download and install [VirtualBox](https://www.virtualbox.org/). This is a free VM provider.

Once those are done, from the root directory of the project (the same directory as the `Vagrantfile` file), do:

```sh
$ vagrant up
... lots of log output as the machine gets set up
```

Once it completes, you can do `vagrant ssh`, then `cd /vagrant`. Notice that the `/vagrant` directory on the VM mirrors the root directory of your project. You can edit the code on your local machine, and use the the usual build instructions on the VM to compile and run the project on the VM!
