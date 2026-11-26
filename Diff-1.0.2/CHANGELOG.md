# 1.0.2

  - Output correct format when an input file is empty, e.g. `@@ --0,0 +1,3 @@`.

# 1.0.1.1

  - Require `base >= 4.11` (GHC 8.4).

# 1.0

  - Add Unix diff style annotations to output of `prettyContextDiff`,
    e.g `@@ -1,5 +1,4 @@`.  This required three changes to the
    signature of `getContextDiff` due to the addition of a wrapper type
    `Numbered`, which enumerates the elements of the input list.

  - Signature change 1: The element pretty printer type changes from
    `(a -> Doc)` to `(Numbered a -> Doc)`.  An unnumber function is
    provided so that the old behavior can be obtained by changing that
    argument from `pretty` to `(pretty . unnumber)`

  - Signature change 2: The result type of getContextDiff changes from
    `ContextDiff a` to `ContextDiff (Numbered a)`.  A function
    `unNumberContextDiff` is provided to convert the result back to
    the old type.

  - Signature change 3: the context argument is now `Maybe Int` rather
    than `Int`, reflecting the change made to `getContextDiffNew` in 0.5.

  - A `prettyContextDiffOld` function is provided to get the old
    style output.

  - The old broken version of getContextDiffOld is removed.

  - Document the behavior of `groupBy'`.

# 0.5

  - Bring space complexity down to D^2 (Bodigrim).
  - Add `Bifunctor` instance (Jonathan King).  Requires `base` >= 4.8.
  - Fix for the grouped context diff.  It was omitting all trailing contexts.
  - Allow unlimited number of context elements (`getContextDiffNew`).

# 0.4

  - Generalize `Diff a` to `PolyDiff a b`.
    `Diff` has been replaced with a specialized synonym `type Diff a = PolyDiff a a`,
    but it's still not backward compatible if you imported `Diff(..)`.
