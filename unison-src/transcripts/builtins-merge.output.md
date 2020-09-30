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
  10. Either     (type)
  11. Either/    (2 definitions)
  12. Float      (builtin type)
  13. Float/     (36 definitions)
  14. Int        (builtin type)
  15. Int/       (28 definitions)
  16. Link       (type)
  17. Link/      (4 definitions)
  18. List       (builtin type)
  19. List/      (10 definitions)
  20. Nat        (builtin type)
  21. Nat/       (27 definitions)
  22. Optional   (type)
  23. Optional/  (2 definitions)
  24. Request    (builtin type)
  25. SeqView    (type)
  26. SeqView/   (2 definitions)
  27. Test/      (3 definitions)
  28. Text       (builtin type)
  29. Text/      (15 definitions)
  30. Tuple      (type)
  31. Tuple/     (1 definition)
  32. Unit       (type)
  33. Unit/      (1 definition)
  34. Universal/ (6 definitions)
  35. bug        (a -> b)
  36. crypto/    (35 definitions)
  37. io2/       (78 definitions)
  38. todo       (a -> b)

```
