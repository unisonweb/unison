The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Author           (type)
  2.  Author/          (7 definitions)
  3.  Boolean          (builtin type)
  4.  Boolean/         (1 definition)
  5.  Bytes            (builtin type)
  6.  Bytes/           (9 definitions)
  7.  Char             (builtin type)
  8.  Char/            (2 definitions)
  9.  CopyrightHolder  (type)
  10. CopyrightHolder/ (7 definitions)
  11. Debug/           (1 definition)
  12. Doc              (type)
  13. Doc/             (7 definitions)
  14. Either           (type)
  15. Either/          (2 definitions)
  16. Float            (builtin type)
  17. Float/           (36 definitions)
  18. GUID             (type)
  19. GUID/            (1 definition)
  20. Int              (builtin type)
  21. Int/             (19 definitions)
  22. IsPropagated     (type)
  23. IsPropagated/    (1 definition)
  24. IsTest           (type)
  25. IsTest/          (1 definition)
  26. License          (type)
  27. License/         (10 definitions)
  28. LicenseType      (type)
  29. LicenseType/     (1 definition)
  30. Link             (type)
  31. Link/            (4 definitions)
  32. List             (builtin type)
  33. List/            (10 definitions)
  34. Nat              (builtin type)
  35. Nat/             (18 definitions)
  36. Optional         (type)
  37. Optional/        (2 definitions)
  38. Request          (builtin type)
  39. Test/            (3 definitions)
  40. Text             (builtin type)
  41. Text/            (15 definitions)
  42. Tuple            (type)
  43. Tuple/           (1 definition)
  44. Unit             (type)
  45. Unit/            (1 definition)
  46. Universal/       (6 definitions)
  47. Year             (type)
  48. Year/            (1 definition)
  49. bug              (a -> b)
  50. io/              (123 definitions)
  51. metadata/        (2 definitions)
  52. todo             (a -> b)

```
