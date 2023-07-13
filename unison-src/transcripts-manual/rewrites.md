
```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
.> add
```

## Structural find and replace

Here's a scratch file with some rewrite rules: 

```unison:hide rewrites-tmp.u
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

cleanup = do 
  _ = IO.removeFile.impl "rewrites-tmp.u"
  ()
```

Let's rewrite these:

```ucm
.> rewrite rule1
.> rewrite eitherToOptional
```

```ucm:hide
.> load rewrites-tmp.u
.> add
```

After adding to the codebase, here's the rewritten source:

```ucm
.> view ex1 Either.mapRight rule1
```

Another example, showing that we can rewrite to definitions that only exist in the file:

```unison:hide rewrites-tmp.u
unique ability Woot1 where woot1 : () -> Nat
unique ability Woot2 where woot2 : () -> Nat

woot1to2 x = @rewrite 
  term Woot1.woot1 ==> Woot2.woot2
  signature _ . Woot1 ==> Woot2 

wootEx : Nat ->{Woot1} Nat 
wootEx a = !woot1
```

Let's apply the rewrite `woot1to2`:

```ucm
.> rewrite woot1to2
```

```ucm:hide
.> load rewrites-tmp.u
.> add
```

After adding the rewritten form to the codebase, here's the rewritten `Woot1` to `Woot2`:

```ucm
.> view wootEx 
```

```ucm:hide
.> run cleanup
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