
```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
.> add
```

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

d = {{ 

Here's the rewritten source of {ex1}:

  @source{ex1} 

And here's the rewritten source of {Either.mapRight},
with {type Either} replaced by {type Optional}:

  @source{Either.mapRight}

Lastly, here's the source of the rewrite blocks (demonstrating
the pretty-printing syntax):
  
  @source{rule1, eitherToOptional}
}}

cleanup = do 
  _ = IO.removeFile.impl "rewrites-tmp.u"
  ()
```

```ucm
.> rewrite rule1
.> rewrite eitherToOptional
.> load rewrites-tmp.u
.> display d
.> add
.> run cleanup
```

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