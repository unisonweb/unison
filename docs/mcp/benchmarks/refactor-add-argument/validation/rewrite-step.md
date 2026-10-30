<!-- Not a solution: step 1 of validation/good.md, kept apart because a `unison` block after `rewrite` is ignored
     by the transcript runner (so the rewritten file has to go into a second transcript). It changes nothing in the
     codebase. It shows that one rule finds and rewrites every use, including the bare `bullets` in `List.map bullets`,
     `|> bullets`, `bullets << f`, the docs and the tests, and leaves `Report.bulletCount` alone. -->

``` unison
bulletsLimit : Nat -> [Text] -> Text
bulletsLimit limit items =
  shown = List.take limit items
  extra = List.size (List.drop limit items)
  more = if extra == 0 then [] else ["... and " ++ Nat.toText extra ++ " more"]
  Text.join "\n" (List.map (item -> "- " ++ item) shown ++ more)

addLimit = @rewrite term bullets ==> bulletsLimit maxNat

bullets.doc : Doc
bullets.doc =
  {{
  {{ docExample 1 do items -> bullets items }} renders `items` as a bulleted
  list, one `- ` line per item. For example:
  
  @eval{ bullets ["tea", "milk"] }
  }}

test> bullets.tests.basic = test.verify do
  ensureEqual (bullets ["a", "b"]) "- a\n- b"
  ensureEqual (bullets []) ""

Report.allSections : [[Text]] -> [Text]
Report.allSections sections = List.map bullets sections

Report.shopping : [Text] -> Text
Report.shopping items = items |> List.map Text.toLowercase |> bullets

Report.show : [Text] ->{IO, Exception} ()
Report.show items = printLine (bullets items)

Report.sideBySide.doc =
  use Text ++
  {{
  Both lists, one after the other:
  
  @eval{ bullets ["a"] ++ "\n\n" ++ bullets ["b"] }
  }}

Report.sideBySide : [Text] -> [Text] -> Text
Report.sideBySide left right =
  use Text ++
  bullets left ++ "\n\n" ++ bullets right

Report.team : [Text] -> Text
Report.team members =
  use Text ++
  "Team\n" ++ bullets members

Report.trimmed : [Text] -> Text
Report.trimmed = bullets << List.map trim
```

``` ucm
scratch/main> sfind addLimit
```

``` ucm
scratch/main> rewrite addLimit
```
