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
  6.  Bytes/        (29 definitions)
  7.  Char          (builtin type)
  8.  Char/         (3 definitions)
  9.  Code          (builtin type)
  10. Code/         (6 definitions)
  11. Debug/        (1 definition)
  12. Doc           (type)
  13. Doc/          (6 definitions)
  14. Either        (type)
  15. Either/       (2 definitions)
  16. Exception     (type)
  17. Exception/    (1 definition)
  18. Float         (builtin type)
  19. Float/        (38 definitions)
  20. Int           (builtin type)
  21. Int/          (31 definitions)
  22. IsPropagated  (type)
  23. IsPropagated/ (1 definition)
  24. IsTest        (type)
  25. IsTest/       (1 definition)
  26. Link          (type)
  27. Link/         (4 definitions)
  28. List          (builtin type)
  29. List/         (10 definitions)
  30. Nat           (builtin type)
  31. Nat/          (28 definitions)
  32. Optional      (type)
  33. Optional/     (2 definitions)
  34. Request       (builtin type)
  35. SeqView       (type)
  36. SeqView/      (2 definitions)
  37. Test/         (3 definitions)
  38. Text          (builtin type)
  39. Text/         (18 definitions)
  40. Tuple         (type)
  41. Tuple/        (1 definition)
  42. Unit          (type)
  43. Unit/         (1 definition)
  44. Universal/    (6 definitions)
  45. Value         (builtin type)
  46. Value/        (5 definitions)
  47. bug           (a -> b)
  48. crypto/       (12 definitions)
  49. io2/          (121 definitions)
  50. metadata/     (2 definitions)
  51. todo          (a -> b)
  52. unsafe/       (1 definition)

```
