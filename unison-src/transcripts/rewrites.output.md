
```unison
---
title: rewrites-tmp.u
---
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

  ☝️
  
  I found and replaced matches in these definitions: ex1
  
  The rewritten file has been added to the top of /Users/pchiusano/unison/rewrites-tmp.u

.> rewrite eitherToOptional

  ☝️
  
  I found and replaced matches in these definitions:
  Either.mapRight
  
  The rewritten file has been added to the top of /Users/pchiusano/unison/rewrites-tmp.u

.> load rewrites-tmp.u

  I found and typechecked these definitions in rewrites-tmp.u.
  If you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Either.mapRight  : (a ->{g} b)
                         -> Optional a
                         ->{g} Optional b
      cleanup          : '{IO} ()
      d                : Doc2
      eitherToOptional : ∀ e1 a e a1 a2 a3 b a4 a5 b1.
                           e1
                           -> a1
                           -> Rewrites
                             ( RewriteTerm
                               (Either e1 b1) (Optional a5),
                               RewriteTerm
                               (Either a4 a1) (Optional a1),
                               RewriteCase
                               (Either e1 b) (Optional a3),
                               RewriteCase
                               (Either a2 a1) (Optional a1),
                               RewriteSignature
                               (Either e a) (Optional a))
      ex1              : [Nat]
      rule1            : ∀ o g i g1.
                           (i ->{g} o)
                           -> Nat
                           -> Rewrites
                             ( RewriteTerm Nat Nat,
                               RewriteTerm
                               (i ->{g, g1} o) (i ->{g} o))

.> display d

  Here's the rewritten source of ex1:
  
      ex1 : [Nat]
      ex1 = List.map Nat.increment [1, 2, 3, 4, 5, 6, 7]
  
  And here's the rewritten source of Either.mapRight, with
  Either replaced by Optional:
  
      Either.mapRight :
        (a ->{g} b) -> Optional a ->{g} Optional b
      Either.mapRight f = cases
        None   -> None
        Some a -> Some (f a)
  
  Lastly, here's the source of the rewrite blocks (demonstrating
  the pretty-printing syntax):
  
      eitherToOptional :
        ∀ e1 a e a1 a2 a3 b a4 a5 b1.
          e1
          -> a1
          -> Rewrites
            ( RewriteTerm (Either e1 b1) (Optional a5),
              RewriteTerm (Either a4 a1) (Optional a1),
              RewriteCase (Either e1 b) (Optional a3),
              RewriteCase (Either a2 a1) (Optional a1),
              RewriteSignature (Either e a) (Optional a))
      eitherToOptional e a =
        @rewrite
          term Left e ==> None
          term Right a ==> Some a
          case Left e ==> None
          case Right a ==> Some a
          signature e a . Either e a ==> Optional a
      
      rule1 :
        ∀ o g i g1.
          (i ->{g} o)
          -> Nat
          -> Rewrites
            ( RewriteTerm Nat Nat,
              RewriteTerm (i ->{g, g1} o) (i ->{g} o))
      rule1 f x =
        use Nat +
        @rewrite
          term x + 1 ==> Nat.increment x
          term a -> f a ==> f

.> run cleanup

  ()

```
