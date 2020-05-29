The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  Boolean    (builtin type)
  2.  Boolean/   (1 definition)
  3.  Bytes      (builtin type)
  4.  Bytes/     (9 definitions)
  5.  Char       (builtin type)
  6.  Char/      (2 definitions)
  7.  Debug/     (1 definition)
  8.  Doc        (type)
  9.  Doc/       (6 definitions)
  10. Float      (builtin type)
  11. Float/     (36 definitions)
  12. Int        (builtin type)
  13. Int/       (28 definitions)
  14. Link       (type)
  15. Link/      (4 definitions)
  16. List       (builtin type)
  17. List/      (10 definitions)
  18. Nat        (builtin type)
  19. Nat/       (27 definitions)
  20. Optional   (type)
  21. Optional/  (2 definitions)
  22. Request    (builtin type)
  23. Test/      (3 definitions)
  24. Text       (builtin type)
  25. Text/      (15 definitions)
  26. Tuple      (type)
  27. Tuple/     (1 definition)
  28. Unit       (type)
  29. Unit/      (1 definition)
  30. Universal/ (6 definitions)
  31. bug        (a -> b)
  32. todo       (a -> b)

```
