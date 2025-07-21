``` ucm :hide
scratch/main> builtins.merge
```

Some tests of pattern behavior.

``` unison
p1 = join [literal "blue", literal "frog"]

> Pattern.run (many p1) "bluefrogbluegoat"
> Pattern.run (many.corrected p1) "bluefrogbluegoat"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + p1 : Pattern Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.

    3 | > Pattern.run (many p1) "bluefrogbluegoat"
          ⧩
          Some ([], "goat")

    4 | > Pattern.run (many.corrected p1) "bluefrogbluegoat"
          ⧩
          Some ([], "bluegoat")
```
