# Documenting Unison code

Unison documentation is written in Unison. Documentation is a value of the following type:

```ucm
.> view builtin.Doc

  unique type builtin.Doc
    = Join [builtin.Doc]
    | Signature Term
    | Evaluate Term
    | Blob Text
    | Link Link
    | Source Link

```
You can create these `Doc` values with ordinary code, or you can use the special syntax. A value of structural type `Doc` can be created via syntax like:

```unison
use .builtin

doc1 = [: This is some documentation.

It can span multiple lines.

Can link to definitions like @List.drop or @List

:]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      doc1 : Doc

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      List.take.ex1 : [Nat]
      List.take.ex2 : [Nat]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    List.take.ex1 : [Nat]
    List.take.ex2 : [Nat]

```
And now let's write our docs and reference these examples:

```unison
use .builtin

docs.List.take = [:
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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      docs.List.take : Doc

```
Let's add it to the codebase, and link it to the definition:

```ucm
.> add

  ⍟ I've added these definitions:
  
    docs.List.take : Doc

.> link docs.List.take builtin.List.take

  Updates:
  
    1. builtin.List.take : Nat -> [a] -> [a]
       + 2. docs.List.take : Doc

```
Now that documentation is linked to the definition. We can view it if we like:

```ucm
.> links builtin.List.take builtin.Doc

  1. docs.List.take : Doc
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> display 1

  `builtin.List.take n xs` returns the first `n` elements of `xs`.
  (No need to add line breaks manually. The display command will
  do wrapping of text for you.  Indent any lines where you don't
  want it to do this.)
  
  ## Examples:
  
    List.take.ex1 : [Nat]
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
    🔽
    ex1 = []
  
  
    List.take.ex2 : [Nat]
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
    🔽
    ex2 = [1, 2]
  

```
Or there's also a convenient function, `docs`, which shows the `Doc` values that are linked to a definition. It's implemented in terms of `links` and `display`:

```ucm
.> docs builtin.List.take

  `builtin.List.take n xs` returns the first `n` elements of `xs`.
  (No need to add line breaks manually. The display command will
  do wrapping of text for you.  Indent any lines where you don't
  want it to do this.)
  
  ## Examples:
  
    List.take.ex1 : [Nat]
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
    🔽
    ex1 = []
  
  
    List.take.ex2 : [Nat]
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
    🔽
    ex2 = [1, 2]
  

```
Note that if we view the source of the documentation, the various references are *not* expanded.

```ucm
.> view docs.List.take

  docs.List.take : Doc
  docs.List.take =
    [: `@builtin.List.take n xs` returns the first `n` elements of
    `xs`. (No need to add line breaks manually. The display command
    will do wrapping of text for you.  Indent any lines where you
    don't want it to do this.)
    
    ## Examples:
    
      @[source] ex1
      🔽
      @ex1 = @[evaluate] ex1
    
    
      @[source] ex2
      🔽
      @ex2 = @[evaluate] ex2
    :]

```
