
# The `text.find` command

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

The `text.find` (or `grep`) command can be used to search for text or numeric literals appearing anywhere in your project. Just supply one or more tokens to search for. Unlike regular grep over the text of your code, this ignores local variables and function names that happen to match your search tokens (use `dependents` or `find` for that purpose). It's only searching for text or numeric literals that match.

```ucm
scratch/main> help text.find.all
```

You can use `grep.all` to search in `lib` as well. 

Here's an example:

```unison
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

```ucm:hide
scratch/main> add
```

```ucm
scratch/main> grep "hi"
scratch/main> text.find.all "hi"
scratch/main> view 1-5
```

```ucm
scratch/main> grep quaffle 
scratch/main> view 1
scratch/main> text.find interesting
scratch/main> view 1
```