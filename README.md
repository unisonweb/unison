The Unison platform
======

[![Join the chat at https://gitter.im/unisonweb/platform](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Unison](http://unisonweb.org) is a new programming platform, currently under active development. This repo contains the code for the Unison node backend (written in Haskell, lives in the root directory, with source in `src`), and the Unison editor (currently written in Elm, found in the folder `editor-elm`).

If you're wondering what the project is about, you can get a glimpse [with this video and post discussing the Unison semantic editor](http://pchiusano.github.io/2015-03-17/unison-update5.html). The editor is just one piece of the overall platform, and  there's updates and more info at [unisonweb.org](http://unisonweb.org) as well as [background posts with additional context](http://pchiusano.io/unison).

Since Unison isn't terribly useful in its current form, the rest of this README will focus on stuff that will be of interest for potential contributors, namely, how to build the code, and a brief tour of the (fairly small but action-packed) codebase. If you're just interested in the project and want to follow along with the progress, [unisonweb.org](http://unisonweb.org) is the place to go, or you can also say hello or lurk [in the chat room](https://gitter.im/unisonweb/platform).

Still here? All right then! Let's get to it.

### Build intructions

This assumes you have already installed:

* GHC 7.10.1 or later (earlier versions may still work, but this project won't be maintained against them) and cabal. [Instructions for installing GHC and cabal for various platforms](https://github.com/bitemyapp/learnhaskell/blob/master/install.md).
* [Elm 0.15](http://elm-lang.org/Install.elm)

Then do:

```
$ git clone https://github.com/unisonweb/platform.git unisonweb
$ cabal sandbox init
$ cabal install
$ // wait 20 minutes while half the Haskell ecosystem
$ // gets downloaded and compiled
$ cabal build node
$ ./dist/build/node/node
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```

That last line is a message from [Scotty](https://hackage.haskell.org/package/scotty) telling you that the node HTTP server is running. Leave that running for now.

Next, the _editor_:

```
$ cd editor-elm
$ elm make src/Unison/Editor.elm
Some new packages are needed. Here is the upgrade plan.

  Install:
    elm-lang/core 2.0.1
    evancz/elm-http 1.0.0

Do you approve of this plan? (y/n) y
Downloading elm-lang/core
Downloading evancz/elm-http
Packages configured successfully!
Compiled 62 files
Successfully generated elm.js
$ elm reactor
Elm Reactor 0.3.1 (Elm Platform 0.15)
Listening on http://0.0.0.0:8000/
```

Now open up a browser and go to `http://0.0.0.0:8000/`. Navigate to `src/Unison/Editor.elm`. You should see the Unison expression editor, initially consisting of a single `_`. You can navigate around with the keyboard or mouse. Use `<Enter>` or a click to open a node for editing. Some other keyboard commands:

* `<Enter>` accepts the current selection in the explorer, and arrow keys or the mouse navigate.
* When the explorer is closed:
    * `'s'` performs linking + 1 beta reduction of the selected expression.
    * `'e'` evaluates the selected expression to weak head normal form.
    * `'a'` wraps the current selection in a function call, initially blank
    * `'v'` switches between the 'raw' and interpreted view

Somewhat annoying---when the explorer pops up, it doesn't get focus. Hit tab once to make the focus active, or click in the text box. You might notice some other minor issues.

If you have the elm reactor running, you can make edits to the code, save, and refresh the page to see them.

### A brief code tour

First, a bit of orientation. Here's the directory structure:

```
editor-elm/
src/
  shared/
  node/
  editor/
```

The `editor-elm/` directory is the current Elm implementation of the Unison editor. So what's with the directory structure? Well, Elm is being phased out, and the editor is likely getting a rewrite in Haskell, with the code compiled via [GHCJS](https://github.com/ghcjs/ghcjs). So the `src` directory, which has all Haskell code, has a `shared/` sub-directory (Haskell code that will be shared between the editor and node), a `node/` sub-directory (code specific to the node), and an `editor/` sub-directory (currently empty but will house the Haskell version of the editor).

Though the rewrite hasn't happened yet, having a stable directory structure means development can easily proceed concurrently on both the new editor and other stuff like the typechecker, the language, the standard library, etc.

The `unison.cabal` file has the dependencies you'd expect--the unison library maps to the `shared/` directory and has minimal external dependencies, and executables `node` (and later `editor`) depend on `shared`. Thus, it should be very obvious and explicit what code and external dependencies are going to be compiled to JS.

### A brief code tour of the Haskell code

The Unison Haskell code, which lives in `src/` is not actually much code right now. Files are spread between `src/shared/` and `src/node/` as mentioned above:

```
$ find src -name '*.hs' | xargs wc -l
      39 src/node/Main.hs
     228 src/node/Node.hs
      76 src/node/Unison/ABT/Extra.hs
      66 src/node/Unison/Digest.hs
       7 src/node/Unison/Distance/Extra.hs
      54 src/node/Unison/Eval/Interpreter.hs
       9 src/node/Unison/Hash/Extra.hs
       8 src/node/Unison/Kind/Extra.hs
     162 src/node/Unison/Node/Implementation.hs
      98 src/node/Unison/Node/Store.hs
      74 src/node/Unison/Node.hs
     140 src/node/Unison/NodeServer.hs
      10 src/node/Unison/Reference/Extra.hs
      20 src/node/Unison/Symbol/Extra.hs
      48 src/node/Unison/Term/Extra.hs
       8 src/node/Unison/TermEdit/Extra.hs
      26 src/node/Unison/Type/Extra.hs
     449 src/node/Unison/Typechecker/Context.hs
     130 src/node/Unison/Typechecker.hs
     245 src/shared/Unison/ABT.hs
      20 src/shared/Unison/Distance.hs
       9 src/shared/Unison/Eval.hs
      36 src/shared/Unison/Hash.hs
      31 src/shared/Unison/JSON.hs
      11 src/shared/Unison/Kind.hs
      52 src/shared/Unison/Metadata.hs
      77 src/shared/Unison/Note.hs
      17 src/shared/Unison/Reference.hs
      35 src/shared/Unison/Symbol.hs
     281 src/shared/Unison/Term.hs
     194 src/shared/Unison/TermEdit.hs
     158 src/shared/Unison/Type.hs
    2838 total
```

Under 3k lines total! Obviously, this number is going to go up over time, but right now, it's pretty bite-sized and (hopefully) easy enough to follow. Certainly not of the scale of something like GHC, which clocks in at [over 100k LOC](http://www.aosabook.org/en/ghc.html)!

One brief note for orientation. Because of the split between `shared/` and `node/`, a module like `Unison.Term` (in `shared/`), which has the basic type and instances for JSON encoding/decoding, has a counterpart in `shared/`, `Unison.Term.Extra` with things like _binary_ serialization or hashing of `Unison.Term` values. Logically, it would be nice to put all functionality in the `Unison.Term` module, but binary serialization and hashing code isn't needed by the editor and we don't want to accidentally compile those libraries and code to JS or rely on tree-shaking to hopefully trim it out. Other `.Extra` modules are analogous.

Now, where to begin? Everyone learns differently. You might prefer to read the code 'inside out' (or perhaps 'bottom up'), starting from the core language syntax tree and typechecker, then expanding out to where these get exposed to the outside world. If this route sounds appealing, here's a reasonable path:

* `Unison.Term` is the module containing the definition for Unison language _terms_ and `Unison.Type` is the module containing the definition for Unison language _types_. Eventually, we'll add `Unison.TypeDeclaration`.
* In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations. Also see `Unison.ABT.Extra`.
* The main interface to the typechecker is in `Unison.Typechecker`, and the implementation is in `Unison.Typechecker.Context`. There isn't a lot of code here (under 500 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
* From here, you can move to `Unison.Node`, which defines the interface satisfied by the node, `Unison.Node.Implementation`, containing a simple implementation of that interface, and `Unison.NodeServer`, which just wraps the node API in an HTTP+JSON interface.
* Lastly, `src/node/Node.hs` has the code which creates an instance of a `Unison.NodeServer`. The `src/node/Node.hs` file also has the definition of the current Unison 'standard library'. The node logic is agnostic to the "standard library" chosen, so whatever creates an instance of `Unison.Node` has to supply it with the standard library it should use.

If instead, you'd rather work from the 'outside in' (or perhaps 'top down'), you could start with `Unison.NodeServer` and work your way back the other direction to modules like `Term`, `Type`, and `ABT`. Since the entire point of the node codebase is to expose an API over HTTP, `Unison.NodeServer` will end up referencing directly or indirectly all the code in the node.

### A brief code tour of the current Unison editor

The Unison editor, living in the `editor-elm/` subdirectory, and written in Elm, is also not much code right now:

```
$ cd editor-elm
$ find src -name '*.elm' | xargs wc -l
      24 src/Elmz/Distance.elm
      80 src/Elmz/Json/Decoder.elm
     109 src/Elmz/Json/Encoder.elm
      50 src/Elmz/Json/Request.elm
     292 src/Elmz/Layout.elm
       8 src/Elmz/List.elm
      54 src/Elmz/Matcher.elm
      48 src/Elmz/Maybe.elm
      83 src/Elmz/Mealy.elm
     107 src/Elmz/Moore.elm
     115 src/Elmz/Movement.elm
     148 src/Elmz/Parser.elm
       8 src/Elmz/Result.elm
      76 src/Elmz/Selection1D.elm
     211 src/Elmz/Signal.elm
      75 src/Elmz/Trie.elm
       6 src/Elmz/Void.elm
      39 src/Unison/Action.elm
     131 src/Unison/EditableTerm.elm
     314 src/Unison/Editor.elm
      17 src/Unison/Hash.elm
      93 src/Unison/Metadata.elm
     149 src/Unison/Node.elm
     119 src/Unison/Path.elm
      43 src/Unison/Reference.elm
      98 src/Unison/Scope.elm
      87 src/Unison/SearchboxParser.elm
     231 src/Unison/Styles.elm
      54 src/Unison/Symbol.elm
     329 src/Unison/Term.elm
     311 src/Unison/TermExplorer.elm
      47 src/Unison/Terms.elm
     130 src/Unison/Type.elm
     380 src/Unison/View.elm
    4066 total
```

Since most of this code will likely be getting a rewrite when moving away Elm, we'll avoid going into too much detail. At a high level:

* The `Elmz` package has various utility modules, not specific to Unison.
* The `Unison.Editor` module is the main entry point for the editor.
* Many of the modules in the `Unison` package mirror their counterparts in Haskell. The representation of terms and types is a bit different from the Haskell side. Unfortunately, Elm's type system cannot represent abstract binding trees, so the the JSON encoders/decoders in Elm convert both terms and types to and from simpler, more Elm-friendly represenations.
* There are a few modules which get used a lot:
  * `Elmz.Layout`, which I'm calling an 'annotated layout tree'. It's a pretty simple idea that lets us use regular pure functions to do hit testing, compute selection highlight regions, and so on. There's a description of the technique and some discussion [in this blog post](http://pchiusano.github.io/2014-12-10/wormhole-antipattern.html). The editor uses this in lots of places.
  * `Elmz.Moore` and `Elmz.Mealy` are pure state machine types, with the minor twist that they may drop events. Most of the components of the editor UI are defined as some `Moore i o`, where `i` will be some type which is the union of all events that component can receive, and where `o` might be a view and some other values to pass along. `Moore` and `Mealy` values can be assembled using various combinators, created with recursion, and fed explicitly. This all works out okay and is pretty simple, but it also requires some manual plumbing.

That's all for now!
