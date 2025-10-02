``` ucm :hide
> builtins.merge
```

``` unison :hide
x = 30

y : Nat
y =
  z = x + 2
  z + 10

structural type Optional a = Some a | None

ability Ask a where
  ask : a
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> debug.term.abt Nat.+

  Builtin term: ##Nat.+

> debug.term.abt y

  (let Ref(ReferenceBuiltin "Nat.+") Ref(ReferenceDerived (Id "qpo3o788girkkbb43uf6ggqberfduhtnqbt7096eojlrp27jieco09mdasb7b0b06ej9hj60a00nnbbdo8he0b4e0m7vtopifiuhdig" 0)) 2 in (User "z". Ref(ReferenceBuiltin "Nat.+") (Var User "z") 10)):ReferenceBuiltin "Nat"

> debug.term.abt Some

  Constructor #0 of the following type:
  DataDeclaration
      { modifier = Structural
      , annotation = External
      , bound =
          [ User "a" ]
      , constructors' =
          [
              ( External
              , User "Constructor0"
              ,
                  ( User "a". Var User "a" -> ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ,
              ( External
              , User "Constructor1"
              ,
                  ( User "a". ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ]
      }

> debug.term.abt ask

  Constructor #0 of the following type:
  EffectDeclaration
      { toDataDecl = DataDeclaration
          { modifier = Unique "oe3grapnkl7hmodmhgpat73j697ltvnk"
          , annotation = External
          , bound =
              [ User "a" ]
          , constructors' =
              [
                  ( External
                  , User "Constructor0"
                  ,
                      ( User "a".
                          (
                              {
                                  [ ReferenceDerived
                                      ( Id "tl3k480g06phii2dv4mmmsg0bimdounfml8p5om9vdsuhph96344lr1o845fucikf1me2akuqaslnibc26mkhcmiuvmk821k7ghnnjo" 0 )
                                      ( Var User "a" )
                                  ]
                              } Var User "a"
                          )
                      )
                  )
              ]
          }
      }

> debug.type.abt Nat

  Builtin type: ##Nat

> debug.type.abt Optional

  DataDeclaration
      { modifier = Structural
      , annotation = External
      , bound =
          [ User "a" ]
      , constructors' =
          [
              ( External
              , User "Constructor0"
              ,
                  ( User "a". Var User "a" -> ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ,
              ( External
              , User "Constructor1"
              ,
                  ( User "a". ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ]
      }

> debug.type.abt Ask

  EffectDeclaration
      { toDataDecl = DataDeclaration
          { modifier = Unique "oe3grapnkl7hmodmhgpat73j697ltvnk"
          , annotation = External
          , bound =
              [ User "a" ]
          , constructors' =
              [
                  ( External
                  , User "Constructor0"
                  ,
                      ( User "a".
                          (
                              {
                                  [ ReferenceDerived
                                      ( Id "tl3k480g06phii2dv4mmmsg0bimdounfml8p5om9vdsuhph96344lr1o845fucikf1me2akuqaslnibc26mkhcmiuvmk821k7ghnnjo" 0 )
                                      ( Var User "a" )
                                  ]
                              } Var User "a"
                          )
                      )
                  )
              ]
          }
      }
```
