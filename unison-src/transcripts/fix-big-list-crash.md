#### Big list crash

Big lists have been observed to crash, while in the garbage collection step.

```unison

unique type Direction = U | D | L | R

a = 0
x = [a + 1, a + 2]
```
