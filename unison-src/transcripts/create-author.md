```ucm:hide
.> alias.type ##Nat basics.Nat
.> alias.type #5hi1vvs5t1 basics.Author
.> alias.type #rc29vdqe01 basics.GUID
.> alias.type #aohndsu9bl basics.CopyrightHolder
```
<!-- pending bugfix
```
.> alias.term #aohndsu9bl#0 basics.CopyrightHolder
```
-->

Demonstrating `create.author`:

```unison:hide
def1 = 1
def2 = 2
```

```ucm
.foo> add
.foo> create.author alicecoder "Alice McGee"
.foo> view 3
.foo> link metadata.authors.alicecoder def1 def2
```
