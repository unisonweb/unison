# Tracing Unison programs

```ucm:hide
.> builtins.merge
```

```unison
fac n = 
  if n < 2 then
    trace "done" 1
  else 
    trace "next" ()
    fac (drop 1 n) * n

> fac 10
```
