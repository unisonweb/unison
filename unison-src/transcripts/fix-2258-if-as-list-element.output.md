Tests that `if` statements can appear as list and tuple elements.

```unison
> [ if true then 1 else 0 ]

> [ if true then 1 else 0, 1]

> [1, if true then 1 else 0]

> (if true then 1 else 0, 0)

> (0, if true then 1 else 0)

> (1)

> (1,2)

> (1,2,3)

> [1,2,3]

> []

> [1]

> [1,2]

> [1,2,3]

> [
  1,
  2,
  3
  ]

> [
  1,
  2,
  3,]

> (1,2,3,)

> (1,
   2,)

```

