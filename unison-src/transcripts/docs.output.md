# Documenting Unison code

Unison documentation is written in Unison. Documentation is a value of the following type:

```ucm
.> view builtin.Doc

  unique type builtin.Doc
    = Link builtin.Link
    | Source builtin.Link
    | Blob builtin.Text
    | Join [builtin.Doc]
    | Signature builtin.Link.Term
    | Evaluate builtin.Link.Term

.> find take

  1. builtin.Bytes.take : builtin.Nat
                          -> builtin.Bytes
                          -> builtin.Bytes
  2. builtin.List.take : builtin.Nat -> [a] -> [a]
  3. builtin.Text.take : builtin.Nat
                         -> builtin.Text
                         -> builtin.Text
  

```
You can create these `Doc` values with ordinary code, or you can use the special syntax. We'll show the syntax here. We are going to document `List.take` using some verbiage and a few examples. First we have to add the examples to the codebase:

```unison
List.take.ex1 = take 0 [1,2,3,4,5]
List.take.ex2 = take 2 [1,2,3,4,5]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      List.take.ex1 : [builtin.Nat]
      List.take.ex2 : [builtin.Nat]
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    List.take.ex1 : [builtin.Nat]
    List.take.ex2 : [builtin.Nat]

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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      docs.List.take : builtin.Doc
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Let's add it to the codebase, and link it to the definition:

```ucm
.> add

  ⍟ I've added these definitions:
  
    docs.List.take : builtin.Doc

.> link builtin.List.take docs.List.take

  Done.

```
Now that documentation is linked to the definition. We can view it if we like:

```ucm
.> links builtin.List.take builtin.Doc

  1. docs.List.take : builtin.Doc
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> display 1

  `builtin.List.take n xs` returns the first `n` elements of `xs`
  
  ## Examples:
  
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
  🔽
  List.take.ex1 = []
  
  
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
  🔽
  List.take.ex2 = [1, 2]

```
Or there's also a convenient function, `docs`, which shows the `Doc` values that are linked to a definition. It's implemented in terms of `links` and `display`:

```ucm
.> docs builtin.List.take

  `builtin.List.take n xs` returns the first `n` elements of `xs`
  
  ## Examples:
  
  List.take.ex1 = builtin.List.take 0 [1, 2, 3, 4, 5]
  🔽
  List.take.ex1 = []
  
  
  List.take.ex2 = builtin.List.take 2 [1, 2, 3, 4, 5]
  🔽
  List.take.ex2 = [1, 2]

```
