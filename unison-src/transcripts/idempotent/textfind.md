# The `text.find` command

``` ucm :hide
> builtins.merge lib.builtin
```

The `text.find` (or `grep`) command can be used to search for text or numeric literals appearing anywhere in your project. Just supply one or more tokens to search for. Unlike regular grep over the text of your code, this ignores local variables and function names that happen to match your search tokens (use `dependents` or `find` for that purpose). It's only searching for text or numeric literals that match.

``` ucm
> help grep

  text.find (or grep)
  `text.find token1 "99" token2` finds terms with literals (text
  or numeric) containing `token1`, `99`, and `token2`.

  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.

  Use `text.find.all` to include search of `lib`.
```

``` ucm
> help text.find.all

  text.find.all (or grep.all)
  `text.find.all token1 "99" token2` finds terms with literals
  (text or numeric) containing `token1`, `99`, and `token2`.

  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.

  Use `text.find` to exclude `lib` from search.
```

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

libfoo = [Any 46, Any "hi", Any "zoink"]
libbar = 3

magicNumber = 230971246918247

openSesame : Nat -> Text
openSesame = cases
  230971246918247 -> "You guessed the password!"
  _ -> "Try again."
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar         : Nat
  + baz         : [Text]
  + foo         : Nat
  + libbar      : Nat
  + libfoo      : [Any]
  + magicNumber : Nat
  + openSesame  : Nat -> Text
  + qux         : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> move libfoo lib.foo

  Done.

> move libbar lib.bar

  Done.
```

``` ucm
> grep hi

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0

> grep "hi"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

> text.find.all hi

  🔎

  These definitions from the current namespace have matches:

    1. bar
    2. lib.foo

  Tip: Try `edit 1` or `edit 1-2` to bring these into your
       scratch file.

> view 1-5

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0

  lib.foo : [Any]
  lib.foo = [Any 46, Any "hi", Any "zoink"]

> grep oog

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0
```

``` ucm
> grep quaffle

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. baz

  Tip: Try `edit 1` to bring this into your scratch file.

> view 1-5

  baz : [Text]
  baz = ["an", "quaffle", "tres"]

> text.find "interesting const"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. foo

  Tip: Try `edit 1` to bring this into your scratch file.

> view 1-5

  foo : Nat
  foo =
    _ = "an interesting constant"
    1

> text.find "99" "23"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0
```

Now some failed searches:

``` ucm :error
> grep lsdkfjlskdjfsd

  😶 I couldn't find any matches.

  Tip: `text.find.all` will search `lib` as well.
```

Notice it gives the tip about `text.find.all`. But not here:

``` ucm :error
> grep.all lsdkfjlskdjfsd

  😶 I couldn't find any matches.
```

Searching for numeric literals should find them both in expression position (rval) and in pattern position:

``` ucm
> grep "230971246918247"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. magicNumber
    2. openSesame

  Tip: Try `edit 1` or `edit 1-2` to bring these into your
       scratch file.
```
