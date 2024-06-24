```ucm
.> project.create test-html-docs

  🎉 I've created the project test-html-docs.

  I'll now fetch the latest version of the base Unison
  library...

  Downloaded 14053 entities.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

test-html-docs/main> builtins.merge

  Done.

```
```unison
{{A doc directly in the namespace.}}
some.ns.direct = 1

{{A doc pretty deeply nested in the namespace.}}
some.ns.pretty.deeply.nested = 2

{{A doc outside the namespace.}}
some.outside = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      some.ns.direct                   : Nat
      some.ns.direct.doc               : Doc
      some.ns.pretty.deeply.nested     : Nat
        (also named lib.base.data.Map.internal.ratio)
      some.ns.pretty.deeply.nested.doc : Doc
      some.outside                     : Nat
        (also named lib.base.data.Map.internal.delta)
      some.outside.doc                 : Doc

```
```ucm
test-html-docs/main> add

  ⍟ I've added these definitions:
  
    some.ns.direct                   : Nat
    some.ns.direct.doc               : Doc
    some.ns.pretty.deeply.nested     : Nat
      (also named lib.base.data.Map.internal.ratio)
    some.ns.pretty.deeply.nested.doc : Doc
    some.outside                     : Nat
      (also named lib.base.data.Map.internal.delta)
    some.outside.doc                 : Doc

test-html-docs/main> docs.to-html some.ns unison-src/transcripts-manual/docs.to-html

```
