We can create a patch from the diff between two namespaces.

```unison
one.a = 1
one.b = 2
oneconflicts.b = 20
one.c = 3
one.d = 4
one.e = 4

two.a = 100
two.b = 200
two.c = 300
twoconflicts.c = 30
two.d = 5
two.e = 6
```

```ucm
.> find one.

  1. one.a : Nat
  2. one.b#cp6 : Nat
  3. one.b#dcg : Nat
  4. one.c : Nat
  5. one.d : Nat


.> find two.

  1. two.a : Nat
  2. two.b : Nat
  3. two.c#k86 : Nat
  4. two.c#qpo : Nat
  5. two.d : Nat
  6. two.e : Nat


.> diff.namespace.to-patch one two thepatch

```



🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to diff.namespace.to-patch. Type `help` or `?`
to get help.
