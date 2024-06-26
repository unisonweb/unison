# namespace.dependencies command

```unison
external.mynat = 1
mynamespace.dependsOnText = external.mynat Nat.+ 10
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    external.mynat            : Nat
    mynamespace.dependsOnText : Nat

.mynamespace> namespace.dependencies

  External dependency                                                                                              Dependents in .mynamespace
  .__projects._f873814e_abb9_4340_a9ac_dc6afc8ecb35.branches._044aa60c_f8bb_4d48_8a31_7be34331fa69.builtin.Nat     1. dependsOnText
                                                                                                                   
  .__projects._f873814e_abb9_4340_a9ac_dc6afc8ecb35.branches._044aa60c_f8bb_4d48_8a31_7be34331fa69.builtin.Nat.+   1. dependsOnText
                                                                                                                   
  .external.mynat                                                                                                  1. dependsOnText

```
