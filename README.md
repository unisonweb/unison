The Unison platform
======

[Unison](http://unisonweb.org) is a new programming platform, currently under active development. This repo contains the code for the Unison node backend (written in Haskell, found in the folder `node`), and the Unison editor (currently written in Elm, found in the folder `editor`).

[![Join the chat at https://gitter.im/unisonweb/platform](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### Build intructions

This assumes you have GHC 7.8.2 or later installed (earlier versions might work fine, I have not tested) and [Elm 0.15 installed](http://elm-lang.org/Install.elm). Then do:

```
$ git clone https://github.com/unisonweb/platform.git unisonweb
$ cd unisonweb/node
$ cabal sandbox init
$ cabal install
$ // wait 30-45 minutes
$ cabal build node
$ ./dist/build/node/node
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```

That last line is a message from [Scotty](https://hackage.haskell.org/package/scotty) telling you that the node HTTP server is running.

Next, the editor:

```
$ cd unisonweb/editor
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

Now open up a browser and go to `http://0.0.0.0:8000/`. Navigate to `src/Unison/Editor.elm`. You should see the Unison expression editor. You can navigate around with the keyboard or mouse. Use `<Enter>` or a click to open a node for editing. Some other keyboard commands:

* `<Enter>` accepts the current selection in the explorer, and arrow keys or the mouse navigate.
* When the explorer is closed: 
    * `'s'` performs linking + 1 beta reduction of the selected expression. 
    * `'e'` evaluates the selected expression to weak head normal form.
    * `'r'` eta-reduces the selection
    * `'a'` wraps the current selection in a function call, initially blank
    * `'v'` switches between the 'raw' and interpreted view

If you have the elm reactor running, you can make edits to the code and just refresh the page to see them.

### A brief code tour (node)

TODO

### A brief code tour (editor)

TODO
