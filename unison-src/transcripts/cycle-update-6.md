Not yet working: properly updating implicit terms with conflicted names.

```ucm
.> builtins.merge
```

```unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2

inner.pong : 'Nat
inner.pong _ = !ping + 3
```

N.B. The `find.verbose pong` is just to print the hash, for easy copying.

```ucm
.> add
.> find.verbose pong
.> merge inner
```

```unison
ping : 'Nat
ping _ = ! #4t465jk908dsue9fgdfi06fihppsme16cvaua29hjm1585de1mvt11dftqrab5chhla3reilsj4c0e7vlkkcct56khgaa5saeu4du48 + 4
```

```ucm
.> update
.> view ping pong
```

Here we see that we didn't properly update `pong` to point to the new `ping` because it was conflicted.
