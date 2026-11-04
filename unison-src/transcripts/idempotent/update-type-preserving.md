An update that changes no type gives the updated term's dependents their new hashes without typechecking them again
(\#5790). They get the hashes that printing them and parsing them back would give: loading the definitions that `view`
prints, after the update, finds no changes.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
helper : Nat -> Nat
helper n = n + 1

direct : Nat
direct = helper 1

indirect : Nat
indirect = direct + helper 2

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + direct   : Nat
  + helper   : Nat -> Nat
  + indirect : Nat
  + ping     : Nat -> Nat
  + pong     : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

`helper` keeps its type, so its dependents are not typechecked again; `ping` and `pong` stay one cycle.

``` unison
helper : Nat -> Nat
helper n = n + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper : Nat -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Nothing that code uses changed its type, so I'm
  saving the results without typechecking it again...

  Done.

scratch/main> view direct indirect ping pong

  direct : Nat
  direct = helper 1

  indirect : Nat
  indirect =
    use Nat +
    direct + helper 2

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

  pong : Nat -> Nat
  pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` unison
direct : Nat
direct = helper 1

indirect : Nat
indirect = direct + helper 2

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

A term that nothing refers to may change its type in the same update: only the types that dependents rely on have to
stay the same.

``` unison
helper : Nat -> Nat
helper n = n + 3

indirect : Int
indirect = +1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper   : Nat -> Nat
  ~ indirect : Int

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  That's done. Nothing that code uses changed its type, so I'm
  saving the results without typechecking it again...

  Done.

scratch/main> view direct ping

  direct : Nat
  direct = helper 1

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)
```

When a type that a dependent relies on changes, only the dependents that use it are typechecked again (\#904). Here
`direct` and `ping` use `helper` and are typechecked; `pong` is not, and stays in a cycle with `ping`.

``` unison
helper : a -> Nat
helper _ = 4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ helper : a -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view direct indirect ping pong

  direct : Nat
  direct = helper 1

  indirect : Int
  indirect = +1

  ping : Nat -> Nat
  ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

  pong : Nat -> Nat
  pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` unison
direct : Nat
direct = helper 1

indirect : Int
indirect = +1

ping : Nat -> Nat
ping n = if n == 0 then helper 0 else pong (Nat.drop n 1)

pong : Nat -> Nat
pong n = if n == 0 then 0 else ping (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

One update can do both: `twice` keeps its type and `label` doesn't, so only `shown` is typechecked again.

``` unison
twice : Nat -> Nat
twice n = n * 2

label : Nat -> Text
label n = "n"

shown : Text
shown = label (twice 3)

doubled : Nat
doubled = twice 4 + Text.size shown
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doubled : Nat
  + label   : Nat -> Text
  + shown   : Text
  + twice   : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
twice : Nat -> Nat
twice n = n + n

label : a -> Text
label _ = "a"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ label : a -> Text
  ~ twice : Nat -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view shown doubled

  doubled : Nat
  doubled =
    use Nat +
    twice 4 + Text.size shown

  shown : Text
  shown = label (twice 3)
```

``` unison
shown : Text
shown = label (twice 3)

doubled : Nat
doubled = twice 4 + Text.size shown
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

An updated type with no parameters keeps its kind, so only the code that builds or matches on its values is typechecked
again. Here `shapes` builds shapes and `sceneArea` matches on a `Scene`, a type that holds shapes and so gets a new
reference too; `totalArea` and `report` only pass shapes along, and take their new references without being typechecked.

``` unison
type Shape = Circle Nat | Square Nat

area : Shape -> Nat
area = cases
  Circle r -> 3 * r * r
  Square s -> s * s

totalArea : [Shape] -> Nat
totalArea = cases
  [] -> 0
  s +: rest -> area s + totalArea rest

shapes : [Shape]
shapes = [Circle 1, Square 2]

report : Text
report = Nat.toText (totalArea shapes)

type Scene = Scene [Shape]

sceneArea : Scene -> Nat
sceneArea = cases Scene ss -> totalArea ss
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Scene
  + type Shape

  + area      : Shape -> Nat
  + report    : Text
  + sceneArea : Scene -> Nat
  + shapes    : [Shape]
  + totalArea : [Shape] -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type Shape = Circle Nat | Square Nat | Dot

area : Shape -> Nat
area = cases
  Circle r -> 3 * r * r
  Square s -> s * s
  Dot -> 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Shape

  ~ area : Shape -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view totalArea shapes report Scene sceneArea

  type Scene = Scene [Shape]

  report : Text
  report = Nat.toText (totalArea shapes)

  sceneArea : Scene -> Nat
  sceneArea = cases Scene ss -> totalArea ss

  shapes : [Shape]
  shapes = [Circle 1, Square 2]

  totalArea : [Shape] -> Nat
  totalArea = cases
    []        -> 0
    s +: rest -> area s Nat.+ totalArea rest
```

``` unison
totalArea : [Shape] -> Nat
totalArea = cases
  [] -> 0
  s +: rest -> area s + totalArea rest

shapes : [Shape]
shapes = [Circle 1, Square 2]

report : Text
report = Nat.toText (totalArea shapes)

type Scene = Scene [Shape]

sceneArea : Scene -> Nat
sceneArea = cases Scene ss -> totalArea ss
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

A type with parameters could change its kind, so its dependents are all typechecked again.

``` unison
type Box a = Box a

boxed : Box Nat
boxed = Box 1

boxes : [Box Nat]
boxes = [boxed]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Box a

  + boxed : Box Nat
  + boxes : [Box Nat]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type Box a = Box a | Empty
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Box a

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

When no dependent builds or matches on a value of the type, none is typechecked again; a type named in an annotation
inside a term takes the new reference too.

``` unison
type Tag = Tag

tagLabel : Tag -> Text
tagLabel t =
  kept : Tag
  kept = t
  "tagged"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Tag

  + tagLabel : Tag -> Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type Tag = Tag | Other
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Tag

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. None of that code looks inside the types that
  changed, so I'm saving the results without typechecking it
  again...

  Done.

scratch/main> view tagLabel

  tagLabel : Tag -> Text
  tagLabel t =
    kept : Tag
    kept = t
    "tagged"
```

``` unison
tagLabel : Tag -> Text
tagLabel t =
  kept : Tag
  kept = t
  "tagged"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

A test in the frontier is typechecked like any other dependent.

``` unison
pick : Nat -> Nat
pick n = n

test> pick.tests.one = if pick 1 == 1 then [Ok "one"] else [Fail "not one"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + pick           : Nat -> Nat
  + pick.tests.one : [Result]

  Run `update` to apply these changes to your codebase.

    4 | test> pick.tests.one = if pick 1 == 1 then [Ok "one"] else [Fail "not one"]
    
    ✅ Passed one
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
pick : a -> Nat
pick _ = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ pick : a -> Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Some of that code uses a type that changed, so
  I'm typechecking those parts again...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view pick.tests.one

  pick.tests.one : [Result]
  pick.tests.one =
    if pick 1 == 1 then [Ok "one"] else [Fail "not one"]
```

``` unison
test> pick.tests.one = if pick 1 == 1 then [Ok "one"] else [Fail "not one"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | test> pick.tests.one = if pick 1 == 1 then [Ok "one"] else [Fail "not one"]
    
    ✅ Passed one
```

A type link takes the updated type's new reference, as parsing the printed source would give it.

``` unison
type Mark = Mark

markLink : Link.Type
markLink = typeLink Mark
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Mark

  + markLink : Link.Type

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type Mark = Mark | Unmarked
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Mark

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. None of that code looks inside the types that
  changed, so I'm saving the results without typechecking it
  again...

  Done.

scratch/main> view markLink

  markLink : Link.Type
  markLink = typeLink Mark
```

``` unison
markLink : Link.Type
markLink = typeLink Mark
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```
