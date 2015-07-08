The Unison platform
======

[![Join the chat at https://gitter.im/unisonweb/platform](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Unison](http://unisonweb.org) is a new programming platform, currently under active development. This repo contains the code for the Unison node backend (written in Haskell, lives in the `node` directory, with source in `src`), and the Unison editor (currently written in Elm, found in the folder `editor-elm`).

If you're wondering what the project is about, you can get a glimpse [with this video and post discussing the Unison semantic editor](http://pchiusano.github.io/2015-03-17/unison-update5.html). The editor is just one piece of the overall platform, and  there's updates and more info at [unisonweb.org](http://unisonweb.org) as well as [background posts with additional context](http://pchiusano.io/unison).

Since Unison isn't terribly useful in its current form, the rest of this README will focus on stuff that will be of interest for potential contributors, namely, how to build the code, and a brief tour of the (fairly small but action-packed) codebase. If you're just interested in the project and want to follow along with the progress, [unisonweb.org](http://unisonweb.org) is the place to go, or you can also say hello or lurk [in the chat room](https://gitter.im/unisonweb/platform).

Still here? All right then! Let's get to it.

### A brief code tour

First, a bit of orientation. Here's the directory structure:

```
shared/
node/
editor/
editor-elm/
```

The `editor-elm/` directory is the current Elm implementation of the Unison editor. It's being phased out, in favor of a Unison editor written in Haskell and compiled to Javascript via [GHCJS](https://github.com/ghcjs/ghcjs). Thus, we can share code between the editor and Unison node backend. So what's with the directory structure? . The `shared/` directory has Haskell code that will be shared between the editor and node, the `node/` directory has code specific to the node, and `editor/` is the currently-in-development Haskell version of the editor, which also depends on `shared/`.

The dependencies are what you'd expect---`shared/` has minimal external dependencies, and `node/` and `editor/` depend on `shared`. Thus, it should be very obvious and explicit what code and external dependencies are going to be compiled to JS.

### Build intructions

Building the project is easy. You do not need to have anything installed or set up in advance. Here are complete instructions:

```
$ git clone https://github.com/unisonweb/platform.git unisonweb
$ cd unisonweb
$ # If you don't have Nix 1.8 or later (see note below)
$ ./install-nix.sh
```

After install completes, you'll see instructions about starting a fresh terminal session or sourcing the given command. Do that, then:

```
$ cd node
$ ./shell.sh
```

Each subproject has a `shell.sh` file to download and build any needed dependencies, and drop you into a shell where you can use cabal as normal. So there's `node/shell.nix`, `shared/shell.nix`, and `editor/shell.nix`. You'll see lots of output the first time you run one of these, as they use Nix to download and build dependencies, and you may see some warnings about Haddock documentation that you can ignore. Subsequent launches will be snappy since you'll have all the dependencies in your Nix store.

Once that's done, you'll be in a Nix shell and can use cabal as normal:

```
$ cabal run node # cabal repl, cabal test also work
Running node...
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```

That last line is a message from [Scotty](https://hackage.haskell.org/package/scotty) telling you that the node HTTP server is running.

### A brief code tour of the Haskell code

The Unison Haskell code, which has the language, its typechecker, and the node implementation, is split between `shared/` and `node/`. It's not actually much code right now, only about 3k lines!

Obviously, this number is going to go up over time, but right now, it's pretty bite-sized and (hopefully) easy enough to follow. Certainly not of the scale of something like GHC, which clocks in at [over 135k LOC](http://www.aosabook.org/en/ghc.html)!

One brief note for orientation. Because of the split between `shared/` and `node/`, a module like `Unison.Term` (in `shared/`), which has the basic type and instances for JSON encoding/decoding, has a counterpart in `shared/`, `Unison.Term.Extra` with things like _binary_ serialization or hashing of `Unison.Term` values. Logically, it would be nice to put all functionality in the `Unison.Term` module, but binary serialization and hashing code isn't needed by the editor and we don't want to accidentally compile those libraries and code to JS or rely on tree-shaking to hopefully trim it out. Other `.Extra` modules are analogous.

Now, where to begin? Everyone learns differently. You might prefer to read the code 'inside out' (or perhaps 'bottom up'), starting from the core language syntax tree and typechecker, then expanding out to where these get exposed to the outside world. If this route sounds appealing, here's a reasonable path:

* `Unison.Term` in `shared/` is the module containing the definition for Unison language _terms_ and `Unison.Type` is the module containing the definition for Unison language _types_. Eventually, we'll add `Unison.TypeDeclaration`.
* In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations. Also see `Unison.ABT.Extra`.
* The main interface to the typechecker is in `node/` in `Unison.Typechecker`, and the implementation is in `Unison.Typechecker.Context`. There isn't a lot of code here (about 500 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
* From here, you can move to `Unison.Node`, which defines the interface satisfied by the node, `Unison.Node.Implementation`, containing a simple implementation of that interface, and `Unison.NodeServer`, which just wraps the node API in an HTTP+JSON interface.
* Lastly, `node/src/Node.hs` has the code which creates an instance of a `Unison.NodeServer`. The `src/node/Node.hs` file also has the definition of the current Unison 'standard library'. The node logic is agnostic to the "standard library" chosen, so whatever creates an instance of `Unison.Node` has to supply it with the standard library it should use.

If instead, you'd rather work from the 'outside in' (or perhaps 'top down'), you could start with `Unison.NodeServer` and work your way back the other direction to modules like `Term`, `Type`, and `ABT`. Since the entire point of the node codebase is to expose an API over HTTP, `Unison.NodeServer` will end up referencing directly or indirectly all the code in the node, and all the Unison language and typechecker.

That's all for now!
