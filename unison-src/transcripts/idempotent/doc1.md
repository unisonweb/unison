# Documenting Unison code

``` ucm :hide
> builtins.mergeio lib.builtins
```

Unison documentation is written in Unison. Documentation is a value of the following type:

``` ucm
> view lib.builtins.Doc

  type lib.builtins.Doc
    = Blob Text
    | Evaluate Link.Term
    | Join [Doc]
    | Link Link
    | Signature Link.Term
    | Source Link
```

You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of structural type `Doc` can be created via syntax like:

``` unison
doc1 = {{ This is some documentation.

It can span multiple lines.

Can link to definitions like {List.drop} or {type List}

}}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doc1 : Doc2

  Run `update` to apply these changes to your codebase.
```

Syntax:

`{{` starts a documentation block; `}}` finishes it. Within the block:

  - Links to definitions are done with `{List.take}` or `{type List}`.
  - `@signature{List.take}` expands to the type signature of `List.take`
  - `@source{List.map}` expands to the full source of `List.map`
  - `{{someOtherDoc}}`, inserts a value `someOtherDoc : Doc2` here.
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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + List.take.doc : Doc2

  Run `update` to apply these changes to your codebase.
```

Let's add it to the codebase.

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

We can view it with `docs`, which shows the `Doc` value that is associated with a definition.

``` ucm
> docs List.take

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
> view List.take

  builtin lib.builtins.List.take :
    lib.builtins.Nat -> [a] -> [a]
```
