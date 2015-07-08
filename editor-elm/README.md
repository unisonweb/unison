This is the Elm version of the Unison editor. It is no longer being maintained and will be replaced by a Haskell version, currently in development (see the `editor/`) directory.

If you want to run this, you'll need to install [Elm 0.15](http://elm-lang.org/Install.elm) first. Make sure you have a node running (via a `cabal run node` in the `node/` subproject shell) as well, since the editor has to talk to the node.

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

The Elm-based Unison editor is not much code:

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
