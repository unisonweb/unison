# Documenting Unison code

```ucm:hide
scratch/main> builtins.merge lib.builtins
```

Unison documentation is written in Unison. Documentation is a value of the following type:

```ucm
scratch/main> view lib.builtins.Doc
```

You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of structural type `Doc` can be created via syntax like:

```unison
doc1 = [: This is some documentation.

It can span multiple lines.

Can link to definitions like @List.drop or @List

:]
```

Syntax:

`[:` starts a documentation block; `:]` finishes it. Within the block:

* Links to definitions are done with `@List`. `\@` (and `\:]`) if you want to escape.
* `@[signature] List.take` expands to the type signature of `List.take`
* `@[source] List.map` expands to the full source of `List.map`
* `@[include] someOtherDoc`, inserts a value `someOtherDoc : Doc` here.
* `@[evaluate] someDefinition` expands to the result of evaluating `someDefinition`, which must be a pre-existing definition in the codebase (can't be an arbitrary expression).

### An example

We are going to document `List.take` using some verbiage and a few examples. First we have to add the examples to the codebase:

```unison
List.take.ex1 = take 0 [1,2,3,4,5]
List.take.ex2 = take 2 [1,2,3,4,5]
```

```ucm
scratch/main> add
```

And now let's write our docs and reference these examples:

```unison
List.take.doc = [:
`@List.take n xs` returns the first `n` elements of `xs`. (No need to add line breaks manually. The display command will do wrapping of text for you.  Indent any lines where you don't want it to do this.)

## Examples:

  @[source] List.take.ex1
  🔽
  @List.take.ex1 = @[evaluate] List.take.ex1


  @[source] List.take.ex2
  🔽
  @List.take.ex2 = @[evaluate] List.take.ex2
:]
```

Let's add it to the codebase.

```ucm
scratch/main> add
```

We can view it with `docs`, which shows the `Doc` value that is associated with a definition.

```ucm
scratch/main> docs List.take
```

Note that if we view the source of the documentation, the various references are *not* expanded.

```ucm
scratch/main> view List.take
```
