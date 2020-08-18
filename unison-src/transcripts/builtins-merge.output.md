The `builtins.merge` command adds the known builtins to a `builtin` subnamespace within the current namespace.

```ucm
  ☝️  The namespace .tmp is empty.

.tmp> builtins.merge

  Done.

.tmp> ls builtin

  1.  AlreadyExists     (IOError)
  2.  Boolean           (builtin type)
  3.  Boolean/          (1 definition)
  4.  BufferMode        (type)
  5.  BufferMode/       (4 definitions)
  6.  Bytes             (builtin type)
  7.  Bytes/            (9 definitions)
  8.  Char              (builtin type)
  9.  Char/             (2 definitions)
  10. Debug/            (1 definition)
  11. Doc               (type)
  12. Doc/              (6 definitions)
  13. EOF               (IOError)
  14. Either            (type)
  15. Either/           (2 definitions)
  16. FileMode          (type)
  17. FileMode/         (4 definitions)
  18. Float             (builtin type)
  19. Float/            (36 definitions)
  20. IO/               (34 definitions)
  21. IOError           (type)
  22. IllegalOperation  (IOError)
  23. Int               (builtin type)
  24. Int/              (28 definitions)
  25. Link              (type)
  26. Link/             (4 definitions)
  27. List              (builtin type)
  28. List/             (10 definitions)
  29. Nat               (builtin type)
  30. Nat/              (27 definitions)
  31. NoSuchThing       (IOError)
  32. Optional          (type)
  33. Optional/         (2 definitions)
  34. PermissionDenied  (IOError)
  35. Request           (builtin type)
  36. ResourceBusy      (IOError)
  37. ResourceExhausted (IOError)
  38. SeqView           (type)
  39. SeqView/          (2 definitions)
  40. Test/             (3 definitions)
  41. Text              (builtin type)
  42. Text/             (15 definitions)
  43. Tuple             (type)
  44. Tuple/            (1 definition)
  45. Unit              (type)
  46. Unit/             (1 definition)
  47. Universal/        (6 definitions)
  48. UserError         (IOError)
  49. bug               (a -> b)
  50. todo              (a -> b)

```
