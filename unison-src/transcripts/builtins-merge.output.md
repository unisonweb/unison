The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Boolean    (builtin type)
  2.  Boolean/   (1 definition)
  3.  Bytes      (builtin type)
  4.  Bytes/     (17 definitions)
  5.  Char       (builtin type)
  6.  Char/      (2 definitions)
  7.  Code       (builtin type)
  8.  Code/      (6 definitions)
  9.  Debug/     (1 definition)
  10. Doc        (type)
  11. Doc/       (6 definitions)
  12. Either     (type)
  13. Either/    (2 definitions)
  14. Float      (builtin type)
  15. Float/     (36 definitions)
  16. Int        (builtin type)
  17. Int/       (29 definitions)
  18. Link       (type)
  19. Link/      (4 definitions)
  20. List       (builtin type)
  21. List/      (10 definitions)
  22. Nat        (builtin type)
  23. Nat/       (28 definitions)
  24. Optional   (type)
  25. Optional/  (2 definitions)
  26. Request    (builtin type)
  27. SeqView    (type)
  28. SeqView/   (2 definitions)
  29. Test/      (3 definitions)
  30. Text       (builtin type)
  31. Text/      (17 definitions)
  32. Tuple      (type)
  33. Tuple/     (1 definition)
  34. Unit       (type)
  35. Unit/      (1 definition)
  36. Universal/ (6 definitions)
  37. Value      (builtin type)
  38. Value/     (5 definitions)
  39. bug        (a -> b)
  40. crypto/    (12 definitions)
  41. io2/       (92 definitions)
  42. todo       (a -> b)

```
