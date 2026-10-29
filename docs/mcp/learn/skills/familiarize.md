Getting to know a library, or the current project, well enough for the task at hand: read its README, then follow what the README mentions, reading documentation and source as you need.

- First work out what you are learning about. If it is the current project, its README is `README` (sometimes `Readme`). If it is a library, it must be installed, so run `list lib` to see what is (see [libraries](learn:ucm/libraries)), and install it if it isn't. To read about a library on [Unison Share](learn:ucm/share) without installing it, use `share.render` (see [share](learn:ucm/share)).
- Read the README with `display`: `display README` for the current project, and for a library use its full name with its folder, as in `display lib.unison_base_7_13_1.README` (`render` is another name for `display`). A README is a documentation value, so `docs README` doesn't work. A bare `README` or `Readme` can also resolve to some other library's README, so always use the full name for a library.
- Note the main types and ideas the README mentions, and look at them: `docs <name>` for the type signature and the documentation, `view <name>` (or `source <name>`) for the source (see [find](learn:ucm/find)). Use full names for a library's definitions: `view lib.unison_base_7_13_1.data.List`. The documentation of a type usually gives an overview of the functions that work with it.
- Don't list every definition. Follow what the README highlights and what your task needs.
- To understand one definition: read its `docs`, optionally `view` its source, `dependencies <name>` to see what it uses, and `dependents <name>` to see where it is used. Repeat on the definitions that matter.
- To find something the README didn't mention: `find <name>` searches by name (`find.all` includes libraries), and `find : MyType -> Something` searches by type.
- Stop when you know enough. If you are only calling a definition, its documentation and type may be enough. If you are changing it or writing something like it, read its source.

## Detail

**An example,** reading a library's README and then a type it mentions:

````
``` ucm
myproject/main> list lib
myproject/main> display lib.unison_base_7_13_1.README
```

``` ucm
myproject/main> docs lib.unison_base_7_13_1.data.List
myproject/main> view lib.unison_base_7_13_1.data.List.map
```
````

All of these work with `transcript-read-only`.

**A command that fails stops the transcript.** `docs` fails for a definition that has no documentation, and so does a type search with no exact match (UCM then falls back to fuzzy matching and reports "I couldn't find exact type matches"). Put such commands in blocks of their own, tagged `:error` when you aren't sure they will succeed (see [transcripts](learn:ucm/transcript)), or use `view`, which works whether or not there is documentation.

**Searching by type.** `find : Nat -> Nat` lists the definitions in your project whose type is `Nat -> Nat`:

````
``` ucm
myproject/main> find : Nat -> Nat

  1. quad : Nat -> Nat
  2. double : Nat -> Nat
```
````

**Dependencies.** `dependencies quad` lists what `quad` uses, split into types and terms, and `dependents double` lists the definitions that use `double`. Each is numbered, so a following `view 2` shows the second one.
