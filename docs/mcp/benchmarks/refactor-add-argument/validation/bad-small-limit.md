<!-- expected: behavior FAIL, outcome FAIL -->
<!-- Existing callers pass 100, not maxNat: the tests with short lists still pass. -->

``` unison
bullets : Nat -> [Text] -> Text
bullets limit items =
  shown = List.take limit items
  extra = List.size (List.drop limit items)
  more = if extra == 0 then [] else ["... and " ++ Nat.toText extra ++ " more"]
  Text.join "\n" (List.map (item -> "- " ++ item) shown ++ more)

bullets.doc : Doc
bullets.doc =
  {{
  {{ docExample 2 do limit items -> bullets limit items }} renders `items` as a
  bulleted list, one `- ` line per item, at most `limit` of them. If items are
  left out, a last line says how many. For example:
  
  @eval{ bullets maxNat ["tea", "milk"] }
  
  @eval{ bullets 1 ["tea", "milk"] }
  }}

test> bullets.tests.basic = test.verify do
  ensureEqual (bullets maxNat ["a", "b"]) "- a\n- b"
  ensureEqual (bullets maxNat []) ""

test> bullets.tests.limit = test.verify do
  ensureEqual (bullets 2 ["a", "b", "c", "d"]) "- a\n- b\n... and 2 more"
  ensureEqual (bullets 4 ["a", "b", "c", "d"]) "- a\n- b\n- c\n- d"
  ensureEqual (bullets 0 ["a"]) "... and 1 more"

Report.allSections : [[Text]] -> [Text]
Report.allSections sections = List.map (bullets 100) sections

Report.shopping : [Text] -> Text
Report.shopping items =
  items |> List.map Text.toLowercase |> bullets 100

Report.show : [Text] ->{IO, Exception} ()
Report.show items = printLine (bullets 100 items)

Report.sideBySide.doc =
  use Text ++
  {{
  Both lists, one after the other:
  
  @eval{ bullets maxNat ["a"] ++ "\n\n" ++ bullets maxNat ["b"] }
  }}

Report.sideBySide : [Text] -> [Text] -> Text
Report.sideBySide left right =
  use Text ++
  bullets 100 left ++ "\n\n" ++ bullets 100 right

Report.team : [Text] -> Text
Report.team members =
  use Text ++
  "Team\n" ++ bullets 100 members

Report.trimmed : [Text] -> Text
Report.trimmed = bullets 100 << List.map trim
```

``` ucm
scratch/main> update
scratch/main> test
```
