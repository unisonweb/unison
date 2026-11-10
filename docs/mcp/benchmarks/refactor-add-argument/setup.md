```` unison
bullets : [Text] -> Text
bullets items = Text.join "\n" (List.map (item -> "- " ++ item) items)

bullets.doc = {{
{{ docExample 1 (do (items -> bullets items)) }} renders `items` as a bulleted list, one `- ` line per item. For example:

@eval{ bullets ["tea", "milk"] }
}}

Report.team : [Text] -> Text
Report.team members = "Team\n" ++ bullets members

Report.shopping : [Text] -> Text
Report.shopping items = items |> List.map Text.toLowercase |> bullets

Report.allSections : [[Text]] -> [Text]
Report.allSections sections = List.map bullets sections

Report.sideBySide : [Text] -> [Text] -> Text
Report.sideBySide left right = bullets left ++ "\n\n" ++ bullets right

Report.sideBySide.doc = {{
Both lists, one after the other:

@eval{ bullets ["a"] ++ "\n\n" ++ bullets ["b"] }
}}

Report.trimmed : [Text] -> Text
Report.trimmed = bullets << List.map Text.trim

Report.show : [Text] ->{IO, Exception} ()
Report.show items = printLine (bullets items)

Report.bulletCount : [Text] -> Nat
Report.bulletCount items =
  bullets = List.map (item -> "- " ++ item) items
  List.size bullets

Report.footer : Text -> Text
Report.footer name = "-- " ++ name ++ " --"

test> bullets.tests.basic = test.verify do
  ensureEqual (bullets ["a", "b"]) "- a\n- b"
  ensureEqual (bullets []) ""

test> Report.tests.team = test.verify do
  ensureEqual (Report.team ["ann", "bo"]) "Team\n- ann\n- bo"

test> Report.tests.sections = test.verify do
  ensureEqual (Report.allSections [["x"], ["y", "z"]]) ["- x", "- y\n- z"]
  ensureEqual (Report.sideBySide ["x"] ["y"]) "- x\n\n- y"

test> Report.tests.trimmed = test.verify do
  ensureEqual (Report.trimmed [" a ", "b "]) "- a\n- b"
  ensureEqual (Report.shopping ["Milk", "EGGS"]) "- milk\n- eggs"
  ensureEqual (Report.bulletCount ["p", "q", "r"]) 3

test> Report.tests.footer = test.verify do
  ensureEqual (Report.footer "end") "-- end --"
````

``` ucm
scratch/main> add
scratch/main> test
```
