The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Any        (builtin type)
  2.  Any/       (1 definition)
  3.  Boolean    (builtin type)
  4.  Boolean/   (1 definition)
  5.  Bytes      (builtin type)
  6.  Bytes/     (17 definitions)
  7.  Char       (builtin type)
  8.  Char/      (2 definitions)
  9.  Code       (builtin type)
  10. Code/      (6 definitions)
  11. Debug/     (1 definition)
  12. Doc        (type)
  13. Doc/       (6 definitions)
  14. Either     (type)
  15. Either/    (2 definitions)
  16. Float      (builtin type)
  17. Float/     (36 definitions)
  18. Int        (builtin type)
  19. Int/       (29 definitions)
  20. Link       (type)
  21. Link/      (4 definitions)
  22. List       (builtin type)
  23. List/      (10 definitions)
  24. Nat        (builtin type)
  25. Nat/       (28 definitions)
  26. Optional   (type)
  27. Optional/  (2 definitions)
  28. Request    (builtin type)
  29. SeqView    (type)
  30. SeqView/   (2 definitions)
  31. Test/      (3 definitions)
  32. Text       (builtin type)
  33. Text/      (17 definitions)
  34. Tuple      (type)
  35. Tuple/     (1 definition)
  36. Unit       (type)
  37. Unit/      (1 definition)
  38. Universal/ (6 definitions)
  39. Value      (builtin type)
  40. Value/     (5 definitions)
  41. bug        (a -> b)
  42. crypto/    (12 definitions)
  43. io2/       (102 definitions)
  44. todo       (a -> b)

```
