The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Any           (builtin type)
  2.  Any/          (1 definition)
  3.  Boolean       (builtin type)
  4.  Boolean/      (1 definition)
  5.  Bytes         (builtin type)
  6.  Bytes/        (17 definitions)
  7.  Char          (builtin type)
  8.  Char/         (3 definitions)
  9.  Code          (builtin type)
  10. Code/         (6 definitions)
  11. Debug/        (1 definition)
  12. Doc           (type)
  13. Doc/          (6 definitions)
  14. Either        (type)
  15. Either/       (2 definitions)
  16. Float         (builtin type)
  17. Float/        (36 definitions)
  18. Int           (builtin type)
  19. Int/          (29 definitions)
  20. IsPropagated  (type)
  21. IsPropagated/ (1 definition)
  22. IsTest        (type)
  23. IsTest/       (1 definition)
  24. Link          (type)
  25. Link/         (4 definitions)
  26. List          (builtin type)
  27. List/         (10 definitions)
  28. Nat           (builtin type)
  29. Nat/          (28 definitions)
  30. Optional      (type)
  31. Optional/     (2 definitions)
  32. Request       (builtin type)
  33. SeqView       (type)
  34. SeqView/      (2 definitions)
  35. Test/         (3 definitions)
  36. Text          (builtin type)
  37. Text/         (18 definitions)
  38. Tuple         (type)
  39. Tuple/        (1 definition)
  40. Unit          (type)
  41. Unit/         (1 definition)
  42. Universal/    (6 definitions)
  43. Value         (builtin type)
  44. Value/        (5 definitions)
  45. bug           (a -> b)
  46. crypto/       (12 definitions)
  47. io2/          (120 definitions)
  48. metadata/     (2 definitions)
  49. todo          (a -> b)

```
