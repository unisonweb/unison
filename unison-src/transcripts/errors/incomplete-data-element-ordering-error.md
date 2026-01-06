```ucm
scratch/main> builtins.merge
```

These should error, this is because each element in the cycle is identical to one another except for the internal references.

We can't allow these terms into the codebase because in certain cases there are multiple valid distinct components which
would receive the same hash.

```unison
structural type CycleA =
  OneA Int CycleB

structural type CycleB =
  OneB Int CycleA
```
