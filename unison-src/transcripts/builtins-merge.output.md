The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  bug        (a -> b)
  2.  todo       (a -> b)
  3.  Any        (builtin type)
  4.  Boolean    (builtin type)
  5.  Bytes      (builtin type)
  6.  Char       (builtin type)
  7.  Code       (builtin type)
  8.  Request    (builtin type)
  9.  Float      (builtin type)
  10. Int        (builtin type)
  11. Nat        (builtin type)
  12. List       (builtin type)
  13. Text       (builtin type)
  14. Value      (builtin type)
  15. Unit       (type)
  16. Optional   (type)
  17. SeqView    (type)
  18. Either     (type)
  19. Tuple      (type)
  20. Link       (type)
  21. Doc        (type)
  22. Any/       (1 definition)
  23. Boolean/   (1 definition)
  24. Bytes/     (17 definitions)
  25. Char/      (2 definitions)
  26. Code/      (6 definitions)
  27. Debug/     (1 definition)
  28. Doc/       (6 definitions)
  29. Either/    (2 definitions)
  30. Float/     (36 definitions)
  31. Int/       (29 definitions)
  32. Link/      (4 definitions)
  33. List/      (10 definitions)
  34. Nat/       (28 definitions)
  35. Optional/  (2 definitions)
  36. SeqView/   (2 definitions)
  37. Test/      (3 definitions)
  38. Text/      (17 definitions)
  39. Tuple/     (1 definition)
  40. Unit/      (1 definition)
  41. Universal/ (6 definitions)
  42. Value/     (5 definitions)
  43. crypto/    (12 definitions)
  44. io2/       (115 definitions)

```
