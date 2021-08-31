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
  6.  Bytes/        (33 definitions)
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
  34. Ref           (builtin type)
  35. Ref/          (2 definitions)
  36. Request       (builtin type)
  37. Scope         (builtin type)
  38. Scope/        (2 definitions)
  39. SeqView       (type)
  40. SeqView/      (2 definitions)
  41. Test/         (3 definitions)
  42. Text          (builtin type)
  43. Text/         (18 definitions)
  44. Tuple         (type)
  45. Tuple/        (1 definition)
  46. Unit          (type)
  47. Unit/         (1 definition)
  48. Universal/    (6 definitions)
  49. Value         (builtin type)
  50. Value/        (5 definitions)
  51. bug           (a -> b)
  52. crypto/       (12 definitions)
  53. io2/          (122 definitions)
  54. metadata/     (2 definitions)
  55. todo          (a -> b)
  56. unsafe/       (1 definition)

```
