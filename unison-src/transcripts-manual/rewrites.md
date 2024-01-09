
```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
.> add
```

## Structural find and replace

Here's a scratch file with some rewrite rules: 

```unison:hide /private/tmp/rewrites-tmp.u
ex1 = List.map (x -> x + 1) [1,2,3,4,5,6,7] 

eitherToOptional e a =
  @rewrite
    term Left e ==> None
    term Right a ==> Some a
    case Left e ==> None
    case Right a ==> Some a
    signature e a . Either e a ==> Optional a

Either.mapRight : (a ->{g} b) -> Either e a ->{g} Either e b
Either.mapRight f = cases
  Left e -> Left e
  Right a -> Right (f a)

rule1 f x = @rewrite 
  term x + 1 ==> Nat.increment x
  term (a -> f a) ==> f -- eta reduction

unique type Optional2 a = Some2 a | None2

rule2 x = @rewrite signature Optional ==> Optional2
```

Let's rewrite these:

```ucm
.> rewrite rule1
.> rewrite eitherToOptional
```

```ucm:hide
.> load /private/tmp/rewrites-tmp.u
.> add
```

After adding to the codebase, here's the rewritten source:

```ucm
.> view ex1 Either.mapRight rule1
```

Another example, showing that we can rewrite to definitions that only exist in the file:

```unison:hide /private/tmp/rewrites-tmp.u
unique ability Woot1 where woot1 : () -> Nat
unique ability Woot2 where woot2 : () -> Nat

woot1to2 x = @rewrite 
  term Woot1.woot1 ==> Woot2.woot2
  term blah ==> blah2
  signature _ . Woot1 ==> Woot2 

wootEx : Nat ->{Woot1} Nat 
wootEx a = 
  _ = !woot1
  blah

blah = 123
blah2 = 456
```

Let's apply the rewrite `woot1to2`:

```ucm
.> rewrite woot1to2
```

```ucm:hide
.> load /private/tmp/rewrites-tmp.u
.> add
```

After adding the rewritten form to the codebase, here's the rewritten `Woot1` to `Woot2`:

```ucm
.> view wootEx 
```

This example shows that rewrite rules can to refer to term definitions that only exist in the file:

```unison:hide /private/tmp/rewrites-tmp.u
foo1 = 
  b = "b"
  123

foo2 = 
  a = "a"
  233

rule = @rewrite
  case None ==> Left "oh no"
  term foo1 ==> foo2
  case None ==> Left "89899"

sameFileEx = 
  _ = "ex"
  foo1
```

```ucm:hide
.> rewrite rule
.> load /private/tmp/rewrites-tmp.u
.> add
```

After adding the rewritten form to the codebase, here's the rewritten definitions:

```ucm
.> view foo1 foo2 sameFileEx 
```

## Capture avoidance

```unison:hide /private/tmp/rewrites-tmp.u
bar1 = 
  b = "bar"
  123

bar2 = 
  a = 39494 
  233

rule bar2 = @rewrite
  case None ==> Left "oh no"
  term bar1 ==> bar2

sameFileEx = 
  _ = "ex"
  bar1
```

In the above example, `bar2` is locally bound by the rule, so when applied, it should not refer to the `bar2` top level binding.

```ucm
.> rewrite rule
```

Instead, it should be an unbound free variable, which doesn't typecheck:

```ucm:error
.> load /private/tmp/rewrites-tmp.u
```

In this example, the `a` is locally bound by the rule, so it shouldn't capture the `a = 39494` binding which is in scope at the point of the replacement:

```unison:hide /private/tmp/rewrites-tmp.u
bar2 = 
  a = 39494 
  233

rule a = @rewrite
  case None ==> Left "oh no"
  term 233 ==> a 
```

```ucm
.> rewrite rule
```

The `a` introduced will be freshened to not capture the `a` in scope, so it remains as an unbound variable and is a type error:

```ucm:error
.> load /private/tmp/rewrites-tmp.u
```

## Structural find

```unison:hide
eitherEx = Left ("hello", "there")
```

```ucm:hide
.> add
```

```unison:hide
findEitherEx x = @rewrite term Left ("hello", x) ==> Left ("hello" Text.++ x) 
findEitherFailure = @rewrite signature a . Either Failure a ==> () 
```

```ucm
.> sfind findEitherEx
.> sfind findEitherFailure
.> find 1-5
```
