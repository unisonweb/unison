# Computable documents in Unison

```ucm:hide
.> builtins.mergeio
```

Unison documentation is written in Unison and has some neat features:

* The documentation type provides a rich vocabulary of elements that go beyond markdown, including asides, callouts, tooltips, and more.
* Docs may contain Unison code which is parsed and typechecked to ensure validity. No more out of date examples that don't compile or assume a bunch of implicit context!
* Embeded examples are live and can show the results of evaluation. This uses the same evaluation cache as Unison's scratch files, allowing Unison docs to function like well-commented spreadsheets or notebooks.
* Docs links to other definitions are typechecked to ensure they point to valid defintions. The links are resolved to hashes and won't be broken by name changes or moving definitions around.
* Docs can be included in other docs and you can assemble documentation programmatically, using Unison code.
* There's a powerful textual syntax for all of the above, which we'll introduce next.

## Introduction

Documentation blocks start with `{{` and end with a matching `}}`. You can introduce doc blocks anywhere you'd use an expression, and you can also have anonymous documentation blocks immediately before a top-level term (and soon, types as well).

```unison
name = {{Alice}}
d1 = {{ Hello there {{name}}! }}

{{ An important constant, equal to @eval{ImportantConstant} }}
ImportantConstant = 41 + 1
```

Notice that an anonymous documentation block `{{ ... }}` before a definition `ImportantConstant` is just syntax sugar for `ImportantConstant.doc = {{ blah }}`.

You can preview what docs will look like when rendered to the console using the `display` or `docs` commands:

```ucm
.> display d1
.> docs ImportantConstant
```

The `docs ImportantConstant` command will look for `ImportantConstant.doc` in the file or codebase. You can do this instead of explicitly linking docs to definitions.

## Syntax guide

```ucm
.> load ./unison-src/new-runtime-transcripts/doc.md.files/syntax.u
.> add
.> view basicFormatting
.> display basicFormatting
.> view lists
.> display lists
```
