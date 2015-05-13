You may have noticed this directory is currently empty. That's because there isn't yet an editor written in Haskell, only the `editor-elm/` directory which has the current Elm implementation. A Haskell implementation of the editor will live here, and will depend only on `shared/`.

Keeping the directories and the dependencies separate makes it easy to reason about what exactly will be compiled to JS, and makes it very difficult to accidentally include some heavyweight dependencies that aren't intended to be used by the editor.
