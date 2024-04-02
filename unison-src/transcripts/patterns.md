```ucm:hide
.> builtins.merge
```

Some tests of pattern behavior.

```unison
p1 = join [literal "blue", literal "frog"]

> Pattern.run (many p1) "bluefrogbluegoat" 
> Pattern.run (many.corrected p1) "bluefrogbluegoat"
```
