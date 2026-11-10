<!-- Same end state by hand, through the update conflict flow. The first `update` can't finish: it leaves the temporary branch
     `update-main` and a file (named `update-main-55ca8ffa.u`) with the new `bullets` and every dependent that no longer
     typechecks. `good-manual.u` is that file after editing by hand: each call site gets `maxNat` as the first argument,
     including the bare uses, the doc examples and tests are edited too, and a test for the limit is added. A worker edits
     the file in place; here a copy stands in. Run it with the directory of this file as the working directory.
     The second `update` saves the file, merges back into `main` and deletes the temporary branch. -->

``` unison
bullets : Nat -> [Text] -> Text
bullets limit items =
  shown = List.take limit items
  extra = List.size (List.drop limit items)
  more = if extra == 0 then [] else ["... and " ++ Nat.toText extra ++ " more"]
  Text.join "\n" (List.map (item -> "- " ++ item) shown ++ more)
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load good-manual.u
scratch/update-main> update
scratch/main> test
scratch/main> branches
scratch/main> todo
```
