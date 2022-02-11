Regression tests to make sure keywords are allowed to start identifiers in terms and types.

In particular, following a keyword with a `wordyIdChar` should be a valid identifier.

Related issues:

- https://github.com/unisonweb/unison/issues/2091
- https://github.com/unisonweb/unison/issues/2727

## Keyword list

Checks the following keywords:

- `type`
- `ability`
- `structural`
- `unique`
- `if`
- `then`
- `else`
- `forall`
- `handle`
- `with`
- `where`
- `use`
- `true`
- `false`
- `alias`
- `typeLink`
- `termLink`
- `let`
- `namespace`
- `match`
- `cases`

Note that although `∀` is a keyword, it cannot actually appear at the start of
identifier.

## Tests

`type`:

```unison
typeFoo = 99
type1 = "I am a variable"
type_ = 292
type! = 3943
type' = 238448
-- this type is the same as `structural type Optional a = Some a | None`, but with very confusing names
structural type type! type_ = type' type_ | type''
-- like `Ask`, but `unique` and with very confusing names
unique ability type'' type_ where type' : type_
```

`ability`:

```unison
abilityFoo = 99
ability1 = "I am a variable"
ability_ = 292
ability! = 3943
ability' = 238448
structural type ability! ability_ = ability' ability_ | ability''
unique ability ability'' ability_ where ability' : ability_
```

`structural`

```unison
structuralFoo = 99
structural1 = "I am a variable"
structural_ = 292
structural! = 3943
structural' = 238448
structural type structural! structural_ = structural' structural_ | structural''
unique ability structural'' structural_ where structural' : structural_
```

`unique`

```unison
uniqueFoo = 99
unique1 = "I am a variable"
unique_ = 292
unique! = 3943
unique' = 238448
structural type unique! unique_ = unique' unique_ | unique''
unique ability unique'' unique_ where unique' : unique_
```

`if`

```unison
ifFoo = 99
if1 = "I am a variable"
if_ = 292
if! = 3943
if' = 238448
structural type if! if_ = if' if_ | if''
unique ability if'' if_ where if' : if_
```

`then`

```unison
thenFoo = 99
then1 = "I am a variable"
then_ = 292
then! = 3943
then' = 238448
structural type then! then_ = then' then_ | then''
unique ability then'' then_ where then' : then_
```

`else`

```unison
elseFoo = 99
else1 = "I am a variable"
else_ = 292
else! = 3943
else' = 238448
structural type else! else_ = else' else_ | else''
unique ability else'' else_ where else' : else_
```

`forall`

```unison
forallFoo = 99
forall1 = "I am a variable"
forall_ = 292
forall! = 3943
forall' = 238448
structural type forall! forall_ = forall' forall_ | forall''
unique ability forall'' forall_ where forall' : forall_
```

`handle`

```unison
handleFoo = 99
handle1 = "I am a variable"
handle_ = 292
handle! = 3943
handle' = 238448
structural type handle! handle_ = handle' handle_ | handle''
unique ability handle'' handle_ where handle' : handle_
```

`with`

```unison
withFoo = 99
with1 = "I am a variable"
with_ = 292
with! = 3943
with' = 238448
structural type with! with_ = with' with_ | with''
unique ability with'' with_ where with' : with_
```

`where`

```unison
whereFoo = 99
where1 = "I am a variable"
where_ = 292
where! = 3943
where' = 238448
structural type where! where_ = where' where_ | where''
unique ability where'' where_ where where' : where_
```

`use`

```unison
useFoo = 99
use1 = "I am a variable"
use_ = 292
use! = 3943
use' = 238448
structural type use! use_ = use' use_ | use''
unique ability use'' use_ where use' : use_
```

`true`

```unison
trueFoo = 99
true1 = "I am a variable"
true_ = 292
true! = 3943
true' = 238448
structural type true! true_ = true' true_ | true''
unique ability true'' true_ where true' : true_
```

`false`

```unison
falseFoo = 99
false1 = "I am a variable"
false_ = 292
false! = 3943
false' = 238448
structural type false! false_ = false' false_ | false''
unique ability false'' false_ where false' : false_
```

`alias`

```unison
aliasFoo = 99
alias1 = "I am a variable"
alias_ = 292
alias! = 3943
alias' = 238448
structural type alias! alias_ = alias' alias_ | alias''
unique ability alias'' alias_ where alias' : alias_
```

`typeLink`

```unison
typeLinkFoo = 99
typeLink1 = "I am a variable"
typeLink_ = 292
typeLink! = 3943
typeLink' = 238448
structural type typeLink! typeLink_ = typeLink' typeLink_ | typeLink''
unique ability typeLink'' typeLink_ where typeLink' : typeLink_
```

`termLink`

```unison
termLinkFoo = 99
termLink1 = "I am a variable"
termLink_ = 292
termLink! = 3943
termLink' = 238448
structural type termLink! termLink_ = termLink' termLink_ | termLink''
unique ability termLink'' termLink_ where termLink' : termLink_
```

`let`

```unison
letFoo = 99
let1 = "I am a variable"
let_ = 292
let! = 3943
let' = 238448
structural type let! let_ = let' let_ | let''
unique ability let'' let_ where let' : let_
```

`namespace`

```unison
namespaceFoo = 99
namespace1 = "I am a variable"
namespace_ = 292
namespace! = 3943
namespace' = 238448
structural type namespace! namespace_ = namespace' namespace_ | namespace''
unique ability namespace'' namespace_ where namespace' : namespace_
```

`match`

```unison
matchFoo = 99
match1 = "I am a variable"
match_ = 292
match! = 3943
match' = 238448
structural type match! match_ = match' match_ | match''
unique ability match'' match_ where match' : match_
```

`cases`

```unison
casesFoo = 99
cases1 = "I am a variable"
cases_ = 292
cases! = 3943
cases' = 238448
structural type cases! cases_ = cases' cases_ | cases''
unique ability cases'' cases_ where cases' : cases_
```

