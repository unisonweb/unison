This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added  to either `reparses-with-same-hash.u` or `reparses.u` as regression tests.

```unison
---
title: /private/tmp/roundtrip.u
---
x = ()

```


```ucm

  I found and typechecked these definitions in
  /private/tmp/roundtrip.u. If you do an `add` or `update`,
  here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : ()

```
So we can see the pretty-printed output:

```ucm
.a1> edit 1-1000 

  ☝️
  
  I added these definitions to the top of
  /private/tmp/roundtrip.u
  
    structural ability Abort where abort : {Abort} a
    
    structural type Fix_2337
      = Fix_2337 Boolean Boolean
    
    structural ability Fix_2392 where zonk : {Fix_2392} Nat
    
    structural type Fix_2392a x y
      = Oog Nat Nat (Nat, Nat)
    
    structural type Fully.qualifiedName
      = Dontcare () Nat
    
    structural type Id a
      = Id a
    
    structural type SomethingUnusuallyLong
      = SomethingUnusuallyLong Text Text Text
    
    structural type UUID
      = UUID Nat (Nat, Nat)
    
    structural ability Zoink where
      nay : Text -> (Nat, Nat) ->{Zoink} Nat
      yay.there : Text ->{Zoink} Nat
    
    Abort.toDefault! : a -> '{g, Abort} a ->{g} a
    Abort.toDefault! default thunk =
      h x = Abort.toDefault! (handler_1778 default x) thunk
      handle !thunk with h
    
    Abort.toOptional : '{g, Abort} a -> '{g} Optional a
    Abort.toOptional thunk = do toOptional! thunk
    
    Abort.toOptional! : '{g, Abort} a ->{g} Optional a
    Abort.toOptional! thunk = toDefault! None '(Some !thunk)
    
    ex1 : Nat
    ex1 =
      use Foo.bar qux1 qux3
      use Nat +
      a = qux3 + qux3
      qux1 + qux1 + Foo.bar.qux2
    
    ex2 : Nat
    ex2 =
      use Foo.bar qux1
      use Nat +
      a =
        use Foo.bar qux3
        z = 203993
        qux3 + qux3
      qux1 + qux1 + Foo.bar.qux2
    
    ex3 : ()
    ex3 =
      a = do
        use Foo.bar qux3
        use Nat +
        x = qux3 + qux3
        x + x
      ()
    
    ex3a : ()
    ex3a =
      use Foo.bar qux3
      use Nat +
      a = do qux3 + qux3
      ()
    
    fix_1035 : Text
    fix_1035 =
      use Text ++
      "aaaaaaaaaaaaaaaaaaaaaa"
        ++ "bbbbbbbbbbbbbbbbbbbbbb"
        ++ "cccccccccccccccccccccc"
        ++ "dddddddddddddddddddddd"
    
    fix_1536 : 'Nat
    fix_1536 = do
      y = 0
      y
    
    fix_1778 : 'Optional Nat
    fix_1778 =
      (do
        abort
        0) |> toOptional
    
    fix_2048 : Doc2
    fix_2048 =
      {{
      **my text** __my text__ **MY_TEXT** ___MY__TEXT___
      ~~MY~TEXT~~ **MY*TEXT**
      }}
    
    fix_2224 : [()] -> ()
    fix_2224 = cases
      x +: (x' +: rest) -> x
      _                 -> ()
    
    fix_2224a : [()] -> ()
    fix_2224a = cases
      rest :+ x' :+ x -> ()
      _               -> ()
    
    fix_2224b : [[()]] -> ()
    fix_2224b = cases
      rest :+ (rest' :+ x) -> x
      _                    -> ()
    
    fix_2271 : Doc2
    fix_2271 =
      {{ # Full doc body indented
      
        ``` raw
        myVal1 = 42 
        myVal2 = 43
        myVal4 = 44
        ```
        
        ``` raw
        indented1= "hi"
        indented2="this is two indents"
        ```
        
        I am two spaces over }}
    
    Fix_2337.f : Fix_2337 -> Boolean
    Fix_2337.f = cases Fix_2337 a b -> a
    
    Fix_2392.f :
      Nat -> Fix_2392a ('{Fix_2392} a) ('{Fix_2392} b) -> Nat
    Fix_2392.f n _ = n
    
    fix_2650 : Nat
    fix_2650 =
      addNumbers : 'Nat
      addNumbers = do
        use Nat +
        y = 12
        13 + y
      !addNumbers
    
    fix_2650a : tvar -> fun -> ()
    fix_2650a tvar fun = ()
    
    fix_2650b : tvar -> '()
    fix_2650b tvar =
      do
        fix_2650a tvar cases
          Some _ ->
            "oh boy isn't this a very very very very very very very long string?"
          None -> ""
    
    fix_2650c : Optional Nat -> ()
    fix_2650c = cases
      Some
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooong| loooooooooooooooooooooooooooooooooooooooooooooooooooooooong
        == 1  ->
        ()
      _ -> ()
    
    fix_3627 : Nat -> Nat -> Nat
    fix_3627 = cases
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ->
        aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
          Nat.+ bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    
    Fix_525.bar.quaffle : Nat
    Fix_525.bar.quaffle = 32
    
    fix_525_exampleTerm : Text -> Nat
    fix_525_exampleTerm quaffle =
      use Nat +
      Fix_525.bar.quaffle + 1
    
    fix_525_exampleType :
      Id qualifiedName -> Id Fully.qualifiedName
    fix_525_exampleType z = Id (Dontcare () 19)
    
    Foo.bar.qux1 : Nat
    Foo.bar.qux1 = 42
    
    Foo.bar.qux2 : Nat
    Foo.bar.qux2 = 44
    
    Foo.bar.qux3 : Nat
    Foo.bar.qux3 = 46
    
    Foo'.bar.qux1 : Text
    Foo'.bar.qux1 = "43"
    
    Foo'.bar.qux2 : Text
    Foo'.bar.qux2 = "45"
    
    Foo'.bar.qux3 : Text
    Foo'.bar.qux3 = "47"
    
    handler_1778 : a -> Request {Abort} a -> a
    handler_1778 default = cases
      { a }          -> a
      { abort -> _ } -> default
    
    nested_fences : Doc2
    nested_fences =
      {{ ```` raw
      ```unison
      r = "boopydoo"
      ```
      ```` }}
    
    raw_a : Text
    raw_a =
      """
      a
      b
      """
    
    raw_b : Text
    raw_b =
      """
      a
      b
      c -- note blank line
      
      """
    
    raw_c : Text
    raw_c =
      """
      ignored (wonky case)
      Use an extra blank line if you'd like a trailing newline. Like so:
      
      """
    
    raw_d : Text
    raw_d =
      """
      ignored (works great)
      Use an extra blank line if you'd like a trailing newline. Like so:
      
      """
    
    simplestPossibleExample : Nat
    simplestPossibleExample =
      use Nat +
      1 + 1
    
    somethingVeryLong : 'Nat
    somethingVeryLong =
      go x =
        do
          match (a -> a) x with
            SomethingUnusuallyLong
              lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij
              | lijaefliejalfijelfj == aefilaeifhlei    -> 0
              | lijaefliejalfijelfj == liaehjffeafijij  -> 1
            _ -> 2
      go (SomethingUnusuallyLong "one" "two" "three")
    
    use_clauses_example : Int -> Text -> Nat
    use_clauses_example oo quaffle =
      use Nat +
      Fix_525.bar.quaffle + Fix_525.bar.quaffle + 1
    
    use_clauses_example2 : Int -> Nat
    use_clauses_example2 oo =
      use Nat +
      quaffle = "hi"
      Fix_525.bar.quaffle
        + Fix_525.bar.quaffle
        + Fix_525.bar.quaffle
        + 1
    
    UUID.random : 'UUID
    UUID.random = do UUID 0 (0, 0)
    
    UUID.randomUUIDBytes : 'Bytes
    UUID.randomUUIDBytes = do
      use Bytes ++
      (UUID a (b, _)) = !random
      encodeNat64be a ++ encodeNat64be b
    
    (|>) : a -> (a ->{e} b) ->{e} b
    a |> f = f a
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

```
This diff should be empty if the two namespaces are equivalent. If it's nonempty, the diff will show us the hashes that differ.

```ucm
.> diff.namespace a1 a2

  Updates:
  
    1. fix_2650a : tvar -> fun -> ()
       ↓
    2. fix_2650a : tvar -> fun -> ()
    
    3. fix_2650b : tvar -> '()
       ↓
    4. fix_2650b : tvar -> '()

```

```



🛑

The transcript was expecting an error in the stanza above, but did not encounter one.
