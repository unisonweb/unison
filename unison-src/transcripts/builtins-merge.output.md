The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Boolean       (builtin type)
  2.  Boolean/      (1 definition)
  3.  Bytes         (builtin type)
  4.  Bytes/        (9 definitions)
  5.  Char          (builtin type)
  6.  Char/         (2 definitions)
  7.  Debug/        (1 definition)
  8.  Doc           (type)
  9.  Doc/          (7 definitions)
  10. Either        (type)
  11. Either/       (2 definitions)
  12. Float         (builtin type)
  13. Float/        (36 definitions)
  14. Int           (builtin type)
  15. Int/          (19 definitions)
  16. IsPropagated  (type)
  17. IsPropagated/ (1 definition)
  18. IsTest        (type)
  19. IsTest/       (1 definition)
  20. Link          (type)
  21. Link/         (4 definitions)
  22. List          (builtin type)
  23. List/         (10 definitions)
  24. Nat           (builtin type)
  25. Nat/          (18 definitions)
  26. Optional      (type)
  27. Optional/     (2 definitions)
  28. Request       (builtin type)
  29. Test/         (3 definitions)
  30. Text          (builtin type)
  31. Text/         (15 definitions)
  32. Tuple         (type)
  33. Tuple/        (1 definition)
  34. Unit          (type)
  35. Unit/         (1 definition)
  36. Universal/    (6 definitions)
  37. bug           (a -> b)
  38. io/           (123 definitions)
  39. metadata/     (2 definitions)
  40. todo          (a -> b)

```
