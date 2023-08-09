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

  The namespaces are identical.

```
Now check that definitions in 'reparses.u' at least parse on round trip:

This just makes 'roundtrip.u' the latest scratch file.

```unison
---
title: /private/tmp/roundtrip.u
---
x = ()

```


```ucm
.a3> edit 1-5000

  ☝️
  
  I added these definitions to the top of
  /private/tmp/roundtrip.u
  
    structural ability Abort where abort : {Abort} a
    
    (>>>>) : Nat -> Nat -> ()
    (>>>>) n = cases _ -> bug ""
    
    catchAll : x -> Nat
    catchAll x = 99
    
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
    
    fix_3110a : x -> f -> ()
    fix_3110a x f =
      _ = 99
      ()
    
    fix_3110b : ()
    fix_3110b =
      fix_3110a
        [1, 2, 3] (x -> let
          y = Nat.increment x
          ())
    
    fix_3110c : ()
    fix_3110c =
      fix_3110a [1, 2, 3] (x -> ignore (Nat.increment x))
    
    fix_3110d : ()
    fix_3110d = fix_3110a [1, 2, 3] '(x -> do
        y = Nat.increment x
        ())
    
    fix_3710 : '(Nat, Nat, Nat, Nat, Nat, Nat)
    fix_3710 = do
      (a, b) = (1, 2)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    fix_3710a : (Nat, Nat, Nat, Nat, Nat, Nat)
    fix_3710a =
      (a, b) = (1, 2)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    fix_3710b : x -> (Nat, x, Nat, Nat, Nat, Nat)
    fix_3710b x =
      (a, b) = (1, x)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    fix_3710c : x -> '(Nat, x, Nat, Nat, Nat, Nat)
    fix_3710c x = do
      (a, b) = (1, x)
      (c, d) = (3, 4)
      (e, f) = (5, 6)
      (a, b, c, d, e, f)
    
    fix_3710d : Optional a -> a
    fix_3710d = cases
      Some x -> x
      None   -> bug "oops"
    
    forkAt : loc -> c -> Nat
    forkAt loc c =
      x = 99
      390439034
    
    ignore : x -> ()
    ignore x = ()
    
    longlines : x -> x
    longlines x =
      u = 92393
      x
    
    longlines1 : 'Text
    longlines1 =
      do
        longlines
          !(longlines_helper
             "This has to laksdjf alsdkfj alskdjf asdf be a long enough string to force a line break")
    
    longlines2 : (Text, '{g} Bytes)
    longlines2 =
      ( "adsf"
      , '(toUtf8
            "adsfsfdgsfdgsdfgsdfgsfdgsfdgsdgsgsgfsfgsgsfdgsgfsfdgsgfsfdgsdgsdfgsgf")
      )
    
    longlines_helper : x -> 'x
    longlines_helper x = do x
    
    multiline_fn :
      a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Nat
    multiline_fn a b c d e f g h i j = 42
    
    multiline_list : [Nat]
    multiline_list =
      use Nat +
      [ 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
          + 1
      , multiline_fn
          12939233
          2102020
          329292
          429292
          522020
          62929292
          72020202
          820202
          920202
          1020202
      ]
    
    softhang : a -> b -> Nat
    softhang a b = 42
    
    softhang2 : x -> f -> Nat
    softhang2 x f = 0
    
    softhang21 : Nat
    softhang21 =
      use Nat +
      handle
        x = 1
        y = abort
        x + y
      with cases
        { a }          -> a
        { abort -> _ } -> 0
    
    softhang21a : Text
    softhang21a =
      use Nat +
      handle
        x = 1
        y = abort
        x + y
      with cases
        { a } ->
          "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "
        { abort -> _ } ->
          "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "
    
    softhang22 : Nat
    softhang22 = softhang2 [0, 1, 2, 3, 4, 5] cases
      0 -> 0
      1 -> 1
      n -> n Nat.+ 100
    
    softhang23 : 'Nat
    softhang23 = do
      use Nat +
      catchAll do
        x = 1
        y = 2
        x + y
    
    softhang24 : 'Nat
    softhang24 = do match 0 with
      0 -> 0
      1 -> 1
      n -> n
    
    softhang25 : Text
    softhang25 = match Nat.increment 1 with
      2 -> "yay"
      n -> "oh no"
    
    softhang26 : Nat
    softhang26 = softhang2 [1, 2, 3, 4] cases
      0 -> 1
      n -> n Nat.+ 1
    
    softhang27 : somewhere -> Nat
    softhang27 somewhere = forkAt somewhere do
      use Nat +
      x = 1
      y = 2
      x + y
    
    softhang28 : Nat
    softhang28 = 
      softhang2 [0, 1, 2, 3, 4, 5] cases
        0 -> 0
        1 -> 1
        n ->
          forkAt
            0
            (n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n
              Nat.+ n)
    
    softhang_a : x -> 'Nat
    softhang_a x = do
      use Nat +
      a = 1
      b = 2
      softhang a do
        c = 3
        a + b
    
    softhang_b : x -> 'Nat
    softhang_b x =
      do
        use Nat +
        a = 1
        b = 2
        softhang
          (100
          + 200
          + 300
          + 400
          + 500
          + 600
          + 700
          + 800
          + 900
          + 1000
          + 1100
          + 1200
          + 1300
          + 1400
          + 1500)
          do
          c = 3
          a + b
    
    softhang_c : x -> 'Nat
    softhang_c x = do
      use Nat +
      a = 1
      b = 2
      1 + (softhang a do
        c = 3
        a + b)
    
    softhang_d : x -> '(b -> Nat)
    softhang_d x = do
      use Nat +
      a = 1
      b = 2
      c = softhang do
        c = 3
        a + b
      c
    
    test3 : '('('r))
    test3 = do
      run : Nat -> a
      run x = bug x
      runrun = 42
      a = "asldkfj"
      b = "asdflkjasdf"
      ''(run runrun ''runrun)
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

```
These are currently all expected to have different hashes on round trip, though we'd prefer if they round tripped with the same hash.

```ucm
.> diff.namespace a1 a3

  Added definitions:
  
    1.  >>>>             : Nat -> Nat -> ()
    2.  catchAll         : x -> Nat
    3.  fix_2650         : Nat
    4.  fix_2650a        : tvar -> fun -> ()
    5.  fix_2650b        : tvar -> '()
    6.  fix_2650c        : Optional Nat -> ()
    7.  fix_3110a        : x -> f -> ()
    8.  fix_3110b        : ()
    9.  fix_3110c        : ()
    10. fix_3110d        : ()
    11. fix_3710         : '(Nat, Nat, Nat, Nat, Nat, Nat)
    12. fix_3710a        : (Nat, Nat, Nat, Nat, Nat, Nat)
    13. fix_3710b        : x -> (Nat, x, Nat, Nat, Nat, Nat)
    14. fix_3710c        : x -> '(Nat, x, Nat, Nat, Nat, Nat)
    15. fix_3710d        : Optional a -> a
    16. forkAt           : loc -> c -> Nat
    17. ignore           : x -> ()
    18. longlines        : x -> x
    19. longlines1       : 'Text
    20. longlines2       : (Text, '{g} Bytes)
    21. longlines_helper : x -> 'x
    22. multiline_fn     : a
                         -> b
                         -> c
                         -> d
                         -> e
                         -> f
                         -> g
                         -> h
                         -> i
                         -> j
                         -> Nat
    23. multiline_list   : [Nat]
    24. softhang         : a -> b -> Nat
    25. softhang2        : x -> f -> Nat
    26. softhang21       : Nat
    27. softhang21a      : Text
    28. softhang22       : Nat
    29. softhang23       : 'Nat
    30. softhang24       : 'Nat
    31. softhang25       : Text
    32. softhang26       : Nat
    33. softhang27       : somewhere -> Nat
    34. softhang28       : Nat
    35. softhang_a       : x -> 'Nat
    36. softhang_b       : x -> 'Nat
    37. softhang_c       : x -> 'Nat
    38. softhang_d       : x -> '(b -> Nat)
    39. test3            : '('('r))
  
  Removed definitions:
  
    40. structural type Fix_2337
    41. structural ability Fix_2392
    42. structural type Fix_2392a x y
    43. structural type Id a
    44. structural type SomethingUnusuallyLong
    45. structural type UUID
    46. structural ability Zoink
    47. structural type Fully.qualifiedName
    48. Fully.qualifiedName.Dontcare                  : '(Nat
                                                      -> qualifiedName)
    49. Fix_2337.Fix_2337                             : Boolean
                                                      -> Boolean
                                                      -> Fix_2337
    50. Id.Id                                         : a
                                                      -> Id a
    51. Fix_2392a.Oog                                 : Nat
                                                      -> Nat
                                                      -> ( Nat,
                                                        Nat)
                                                      -> Fix_2392a
                                                        x y
    52. SomethingUnusuallyLong.SomethingUnusuallyLong : Text
                                                      -> Text
                                                      -> Text
                                                      -> SomethingUnusuallyLong
    53. UUID.UUID                                     : Nat
                                                      -> ( Nat,
                                                        Nat)
                                                      -> UUID
    54. Zoink.nay                                     : Text
                                                      -> ( Nat,
                                                        Nat)
                                                      ->{Zoink} Nat
    55. Zoink.yay.there                               : Text
                                                      ->{Zoink} Nat
    56. Fix_2392.zonk                                 : {Fix_2392} Nat
    57. ex1                                           : Nat
    58. ex2                                           : Nat
    59. ex3                                           : ()
    60. ex3a                                          : ()
    61. Fix_2337.f                                    : Fix_2337
                                                      -> Boolean
    62. Fix_2392.f                                    : Nat
                                                      -> Fix_2392a
                                                        ('{Fix_2392} a)
                                                        ('{Fix_2392} b)
                                                      -> Nat
    63. fix_1035                                      : Text
    64. fix_1536                                      : 'Nat
    65. fix_1778                                      : 'Optional
                                                        Nat
    66. fix_2048                                      : Doc2
    67. fix_2224                                      : [()]
                                                      -> ()
    68. fix_2224a                                     : [()]
                                                      -> ()
    69. fix_2224b                                     : [[()]]
                                                      -> ()
    70. fix_2271                                      : Doc2
    71. fix_3627                                      : Nat
                                                      -> Nat
                                                      -> Nat
    72. fix_525_exampleTerm                           : Text
                                                      -> Nat
    73. fix_525_exampleType                           : Id
                                                        qualifiedName
                                                      -> Id
                                                        qualifiedName
    74. handler_1778                                  : a
                                                      -> Request
                                                        {Abort}
                                                        a
                                                      -> a
    75. nested_fences                                 : Doc2
    76. Fix_525.bar.quaffle                           : Nat
    77. Foo.bar.qux1                                  : Nat
    78. Foo'.bar.qux1                                 : Text
    79. Foo.bar.qux2                                  : Nat
    80. Foo'.bar.qux2                                 : Text
    81. Foo.bar.qux3                                  : Nat
    82. Foo'.bar.qux3                                 : Text
    83. UUID.random                                   : 'UUID
    84. UUID.randomUUIDBytes                          : 'Bytes
    85. raw_a                                         : Text
    86. raw_b                                         : Text
    87. raw_c                                         : Text
    88. raw_d                                         : Text
    89. simplestPossibleExample                       : Nat
    90. somethingVeryLong                             : 'Nat
    91. Abort.toDefault!                              : a
                                                      -> '{g,
                                                      Abort} a
                                                      ->{g} a
    92. Abort.toOptional                              : '{g,
                                                      Abort} a
                                                      -> '{g} Optional
                                                        a
    93. Abort.toOptional!                             : '{g,
                                                      Abort} a
                                                      ->{g} Optional
                                                        a
    94. use_clauses_example                           : Int
                                                      -> Text
                                                      -> Nat
    95. use_clauses_example2                          : Int
                                                      -> Nat
    96. |>                                            : a
                                                      -> (a
                                                      ->{e} b)
                                                      ->{e} b

```
## Other regression tests not covered by above

### Comment out builtins in the edit command

Regression test for https://github.com/unisonweb/unison/pull/3548

