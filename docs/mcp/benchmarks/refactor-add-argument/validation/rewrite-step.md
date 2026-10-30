<!-- Not a solution: step 1 of validation/good.md, kept apart because it changes nothing in the codebase. It shows that
     one rule finds and rewrites every use, including the bare `bullets` in `List.map bullets`, `|> bullets`,
     `bullets << f`, the docs and the tests, and leaves `Report.bulletCount` alone. `rewrite` writes the rewritten
     definitions to `rewrites/<time>-<guid>.u` (the name is random, so the validation goes on with the text in good.md).
     The rule's helper `bulletsLimit` is not in that file, so it has to be defined there again, as `bullets`. -->

``` unison
bulletsLimit : Nat -> [Text] -> Text
bulletsLimit limit items =
  shown = List.take limit items
  extra = List.size (List.drop limit items)
  more = if extra == 0 then [] else ["... and " ++ Nat.toText extra ++ " more"]
  Text.join "\n" (List.map (item -> "- " ++ item) shown ++ more)

addLimit = @rewrite term bullets ==> bulletsLimit maxNat
```

``` ucm
scratch/main> rewrite.find :addLimit
```

``` ucm
scratch/main> rewrite :addLimit
```
