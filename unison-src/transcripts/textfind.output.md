# The `text.find` command

The `text.find` (or `grep`) command can be used to search for text or numeric literals appearing anywhere in your project. Just supply one or more tokens to search for. Unlike regular grep over the text of your code, this ignores local variables and function names that happen to match your search tokens (use `dependents` or `find` for that purpose). It's only searching for text or numeric literals that match.

``` ucm
scratch/main> help text.find.all

  text.find.all (or grep.all)
  `text.find.all token1 token2` finds terms with literals (text
  or numeric) containing both `token1` and `word2`.
  
  Use `text.find` to exclude `lib` from search.

```
You can use `grep.all` to search in `lib` as well.

Here's an example:

``` unison
foo = 
  _ = "an interesting constant"
  1
bar = match "well hi there" with
  "ooga" -> 99
  "booga" -> 23
  _ -> 0
baz = ["an", "quaffle", "tres"] 
qux = 
  quaffle = 99
  quaffle + 1

lib.foo = [Any 46, Any "hi", Any "zoink"]
lib.bar = 3
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar     : Nat
      baz     : [Text]
      foo     : Nat
      lib.bar : Nat
      lib.foo : [Any]
      qux     : Nat

```
``` ucm
scratch/main> grep "hi"

  😶 I couldn't find any matches.

```

``` ucm
scratch/main> grep "hi"scratch/main> text.find.all "hi"scratch/main> view 1-5
```



🛑

The transcript failed due to an error in the stanza above. The error is:


  😶 I couldn't find any matches.

