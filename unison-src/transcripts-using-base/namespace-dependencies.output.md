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
  .__projects._00d11f84_cb58_44e7_9f94_a609484f5480.branches._4d14ef03_be64_4fbe_bbfd_0c32f444600d.builtin.Nat     1. dependsOnText
                                                                                                                   
  .__projects._00d11f84_cb58_44e7_9f94_a609484f5480.branches._4d14ef03_be64_4fbe_bbfd_0c32f444600d.builtin.Nat.+   1. dependsOnText
                                                                                                                   
  .external.mynat                                                                                                  1. dependsOnText

```
