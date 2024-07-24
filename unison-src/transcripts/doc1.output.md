# Documenting Unison code

Unison documentation is written in Unison. Documentation is a value of the following type:

``` ucm
scratch/main> view lib.builtins.Doc

  type lib.builtins.Doc
    = Blob Text
    | Link Link
    | Source Link
    | Signature Link.Term
    | Evaluate Link.Term
    | Join [lib.builtins.Doc]

```
You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of structural type `Doc` can be created via syntax like:

``` unison
doc = {{ This is some documentation.

It can span multiple lines.

Can link to definitions like {List.drop} or {type List}

}}
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      doc : Doc2

```
Syntax:

`{{` starts a documentation block; `}}` finishes it. Within the block:

  - Links to definitions are done with `{type List}`.
  - `@signature{List.take}` expands to the type signature of `List.take`
  - `@source{List.map}` expands to the full source of `List.map`
  - `{{ someOtherDoc }}`, inserts a value `someOtherDoc : Doc` here.
  - `@eval{someDefinition}` expands to the result of evaluating `someDefinition`, which can be an arbitrary expression.

### An example

We are going to document `List.take` using some verbiage and a few examples.

```` unison
List.take.doc = {{
`List.take n xs` returns the first `n` elements of `xs`. (No need to add line breaks manually. The display command will do wrapping of text for you.  Indent any lines where you don't want it to do this.)

## Examples:

```
take 0 [1,2,3,4,5]
```

```
take 2 [1,2,3,4,5]
```
}}
````

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      List.take.doc : Doc2

```
Let's add it to the codebase.

``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    List.take.doc : Doc2

```
We can view it with `docs`, which shows the `Doc` value that is associated with a definition.

``` ucm
scratch/main> docs List.take

  `List.take n xs` returns the first `n` elements of `xs`. (No
  need to add line breaks manually. The display command will do
  wrapping of text for you. Indent any lines where you don't
  want it to do this.)
  
  # Examples:
  
        List.take 0 [1, 2, 3, 4, 5]
        ⧨
        []
  
        List.take 2 [1, 2, 3, 4, 5]
        ⧨
        [1, 2]

```
Note that if we view the source of the documentation, the various references are *not* expanded.

``` ucm
scratch/main> view List.take

  builtin lib.builtins.List.take :
    lib.builtins.Nat -> [a] -> [a]

```
