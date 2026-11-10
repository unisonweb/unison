<!-- expected: outcome FAIL -->
<!-- Finishes the update conflict flow by deleting the docs and the tests of `bullets` from the file (`bad-dropped-dependents.u`,
     a stand-in for the edited `update-main-55ca8ffa.u`), which deletes them from the branch. Run with the directory of this
     file as the working directory. -->

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
scratch/update-main> load bad-dropped-dependents.u
scratch/update-main> update
scratch/main> test
```
