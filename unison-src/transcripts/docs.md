# Documenting Unison code

Unison documentation is written in Unison. Documentation is a value of the following type:

```ucm
.> view builtin.Doc
```

You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of type `Doc` can be created via syntax like:

```unison
use .builtin

doc1 = [: This is some documentation.

It can span multiple lines.

Can link to definitions like @List.drop or @List

:]
```

Syntax:

`[:` starts a documentation block; `:]` finishes it. Within the block:

* Links to definitions are done with `@List`. `\@` if you want to escape.
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
.> add
```

And now let's write our docs and reference these examples:

```unison
use .builtin

docs.List.take = [:

`@List.take n xs` returns the first `n` elements of `xs`

## Examples:

@[source] List.take.ex1
🔽
@[evaluate] List.take.ex1


@[source] List.take.ex2
🔽
@[evaluate] List.take.ex2
:]
```

Let's add it to the codebase, and link it to the definition:

```ucm
.> add
.> link builtin.List.take docs.List.take
```

Now that documentation is linked to the definition. We can view it if we like:

```ucm
.> links builtin.List.take builtin.Doc
.> display 1
```

Or there's also a convenient function, `docs`, which shows the `Doc` values that are linked to a definition. It's implemented in terms of `links` and `display`:

```ucm
.> docs builtin.List.take
```
