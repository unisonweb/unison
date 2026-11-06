# Changes in version 0.13.2.0

 * Strict boxed vector `Data.Vector.Strict` and `Data.Vector.Strict.Mutable` is
   added (#488). it ensures that all values in the vector are evaluated to WHNF.
 * `DoNotUnboxStrict`, `DoNotUnboxLazy`, and `DoNotUnboxNormalForm` wrapper are
   added for defining unbox instances for types that contain not unboxable fields.
   [#503](https://github.com/haskell/vector/issues/506),
   [#508](https://github.com/haskell/vector/pull/508)
 * `spanR` and `breakR` were added [#476](https://github.com/haskell/vector/pull/476).
   They allow parsing vector from the right.
 * We had some improvements on `*.Mutable.{next,prev}Permutation{,By}`
   [#498](https://github.com/haskell/vector/pull/498):
   * Add `*.Mutable.prevPermutation{,By}` and `*.Mutable.nextPermutationBy`
   * Improve time performance. We may now expect good specialization supported by inlining.
     The implementation has also been algorithmically updated: in the previous implementation
     the full enumeration of all the permutations of `[1..n]` took Omega(n*n!), but it now takes O(n!).
   * Add tests for `{next,prev}Permutation`
   * Add benchmarks for `{next,prev}Permutation`
 * Cabal >= 3.0 is now required for building package (#481).
 * `vector:benchmarks-O2` public sublibrary containing benchmarks is added (#481).
 * Type family `Mutable` provides instances for arrays from `primitive`.
 * Various documentation improvements.

# Changes in version 0.13.1.0

 * Specialized variants of `findIndexR` are reexported for all vector
   types. [#469](https://github.com/haskell/vector/pull/469)
 * `UnboxViaPrim` could be used for deriving `Unbox` instances (`V_UnboxViaPrim`
   constructor is exported) [#450](https://github.com/haskell/vector/pull/450)
 * Fields of `Data.Vector.Fusion.Bundle.Size` are now strict
   [#456](https://github.com/haskell/vector/pull/456)
 * Compatibility with future GHC 9.10 release
   [#462](https://github.com/haskell/vector/pull/462)
 * Test suite no longer fails when built with QuickCheck-2.14
   [#461](https://github.com/haskell/vector/pull/461)
 * Doctests now work with current versions of GHC
   [#465](https://github.com/haskell/vector/pull/466)
 * Various documentation improvements


# Changes in version 0.13.0.0

 * `mkType` from `Data.Vector.Generic` is deprecated in favor of
   `Data.Data.mkNoRepType`
 * The role signatures on several `Vector` types were too permissive, so they
   have been tightened up:
   * The role signature for `Data.Vector.Mutable.MVector` is now
     `type role MVector nominal representational` (previously, both arguments
     were `phantom`). [#224](https://github.com/haskell/vector/pull/224)
   * The role signature for `Data.Vector.Primitive.Vector` is now
     `type role Vector nominal` (previously, it was `phantom`).
     The role signature for `Data.Vector.Primitive.Mutable.MVector` is now
     `type role MVector nominal nominal` (previously, both arguments were
     `phantom`). [#316](https://github.com/haskell/vector/pull/316)
   * The role signature for `Data.Vector.Storable.Vector` is now
     `type role Vector nominal` (previous, it was `phantom`), and the signature
     for `Data.Vector.Storable.Mutable.MVector` is now
     `type role MVector nominal nominal` (previous, both arguments were
     `phantom`). [#235](https://github.com/haskell/vector/pull/235)

     We pick `nominal` for the role of the last argument instead of
     `representational` since the internal structure of a `Storable` vector is
     determined by the `Storable` instance of the element type, and it is not
     guaranteed that the `Storable` instances between two representationally
     equal types will preserve this internal structure.  One consequence of this
     choice is that it is no longer possible to `coerce` between
     `Storable.Vector a` and `Storable.Vector b` if `a` and `b` are nominally
     distinct but representationally equal types. We now provide
     `unsafeCoerce{M}Vector` and `unsafeCast` functions to allow this (the onus
     is on the user to ensure that no `Storable` invariants are broken when
     using these functions).
 * Methods of type classes `Data.Vector.Generic.Mutable.MVector` and
   `Data.Vector.Generic.Vector` use concrete monads (`ST`, etc) istead of being
   polymorphic (`PrimMonad`, etc). [#335](https://github.com/haskell/vector/pull/335).
   This makes it possible to derive `Unbox` with:
   * `GeneralizedNewtypeDeriving`
   * via `UnboxViaPrim` and `Prim` instance
   * via `As` and `IsoUnbox` instance: [#378](https://github.com/haskell/vector/pull/378)
 * Add `MonadFix` instance for boxed vectors: [#312](https://github.com/haskell/vector/pull/312)
 * Re-export `PrimMonad` and `RealWorld` from mutable vectors:
   [#320](https://github.com/haskell/vector/pull/320)
 * Add `maximumOn` and `minimumOn`: [#356](https://github.com/haskell/vector/pull/356)
 * The functions `scanl1`, `scanl1'`, `scanr1`, and `scanr1'` for immutable
   vectors are now defined when given empty vectors as arguments,
   in which case they return empty vectors. This new behavior is consistent
   with the one of the corresponding functions in `Data.List`.
   Prior to this change, applying an empty vector to any of those functions
   resulted in an error. This change was introduced in:
   [#382](https://github.com/haskell/vector/pull/382)
 * Change allocation strategy for `unfoldrN`: [#387](https://github.com/haskell/vector/pull/387)
 * Remove `CPP` driven error reporting in favor of `HasCallStack`:
   [#397](https://github.com/haskell/vector/pull/397)
 * Remove redundant `Storable` constraints on to/from `ForeignPtr` conversions:
   [#394](https://github.com/haskell/vector/pull/394)
 * Add `unsafeCast` to `Primitive` vectors: [#401](https://github.com/haskell/vector/pull/401)
 * Make `(!?)` operator strict: [#402](https://github.com/haskell/vector/pull/402)
 * Add `readMaybe`: [#425](https://github.com/haskell/vector/pull/425)
 * Add `groupBy` and `group` for `Data.Vector.Generic` and the specialized
   version in `Data.Vector`, `Data.Vector.Unboxed`, `Data.Vector.Storable` and
   `Data.Vector.Primitive`. [#427](https://github.com/haskell/vector/pull/427)
 * Add `toArraySlice` and `unsafeFromArraySlice` functions for conversion to and
   from the underlying boxed `Array`: [#434](https://github.com/haskell/vector/pull/434)

# Changes in version 0.12.3.1

 * Bugfix for ghcjs and `Double` memset for `Storable` vector:
   [#410](https://github.com/haskell/vector/issues/410)
 * Avoid haddock bug: [#383](https://github.com/haskell/vector/issues/383)
 * Improve haddock and doctests
 * Disable problematic tests with -boundschecks [#407](https://github.com/haskell/vector/pull/407)

# Changes in version 0.12.3.0

 * Fix performance regression due to introduction of `keepAlive#` primop in ghc-9.0: [#372](https://github.com/haskell/vector/pull/372)

 * Add monadic functions for mutable vectors: [#338](https://github.com/haskell/vector/pull/338)

   * Added folds for monadic functions: `mapM_`, `imapM_`, `forM_`, `iforM_`,
     `foldl`, `foldl'`, `foldM`, `foldM'`, `ifoldl`, `ifoldl'`, `ifoldM`,
     `ifoldM'`
   * Added `modifyM` and `unsafeModifyM` for mutable vectors
   * Added `generate` and `generateM` for mutable vectors

# Changes in version 0.12.2.0

 * Add `MINIMAL` pragma to `Vector` & `MVector` type classes: [#11](https://github.com/haskell/vector/issues/11)
 * Export `unstreamM` from`from Data.Vector.Generic`: [#70](https://github.com/haskell/vector/issues/70)
 * Added `unfoldrExactN` and `unfoldrExactNM`: [#140](https://github.com/haskell/vector/issues/140)
 * Added `iforM` and `iforM_`: [#262](https://github.com/haskell/vector/issues/262)
 * Added `MonadFix` instance for boxed vectors: [#178](https://github.com/haskell/vector/issues/178)
 * Added `uncons` and `unsnoc`: [#212](https://github.com/haskell/vector/issues/212)
 * Added `foldMap` and `foldMap'`: [#263](https://github.com/haskell/vector/issues/263)
 * Added `isSameVector` for storable vectors
 * Added `toArray`, `fromArray`, `toMutableArray` and `fromMutableArray`
 * Added `iscanl`, `iscanl'`, `iscanr`, `iscanr'` to `Primitive`, `Storable` and `Unboxed`
 * Added `izipWithM`, `izipWithM_`, `imapM` and `imapM_` to `Primitive` and `Storable`
 * Added `ifoldM`, `ifoldM'`, `ifoldM_` and `ifoldM'_` to `Primitive` and `Storable`
 * Added `eqBy` and `cmpBy`
 * Added `findIndexR` to `Generic`: [#172](https://github.com/haskell/vector/issues/172)
 * Added `catMaybes`: [#329](https://github.com/haskell/vector/issues/329)
 * Added `mapMaybeM` and `imapMaybeM`: [#183](https://github.com/haskell/vector/issues/183)


# Changes in version 0.12.1.2

 * Fix for lost function `Data.Vector.Generic.mkType`: [#287](https://github.com/haskell/vector/issues/287)

# Changes in version 0.12.1.1 (deprecated)
 * add semigrioups dep to test suite so CI actually runs again on GHC < 8

# Changes in version 0.12.1.0 (deprecated)
 * Fix integer overflows in specializations of Bundle/Stream enumFromTo on Integral types
 * Fix possibility of OutOfMemory with `take` and very large arguments.
 * Fix `slice` function causing segfault and not checking the bounds properly.
 * updated specialization rule for EnumFromTo on Float and Double
  to make sure it always matches the version in GHC Base (which changed as of 8.6)
  Thanks to Aleksey Khudyakov @Shimuuar for this fix.
 * fast rejection short circuiting in eqBy operations
 * the O2 test suite now has reasonable memory usage on every GHC version,
    special thanks to Alexey Kuleshevich (@lehins).
 * The `Mutable` type family is now injective on GHC 8.0 or later.
 * Using empty `Storable` vectors no longer results in division-by-zero
   errors.
 * The `Data` instances for `Vector` types now have well defined
   implementations for `toConstr`, `gunfold`, and `dataTypeOf`.
 * New function: `partitionWith`.
 * Add `Unbox` instances for `Identity`, `Const`, `Down`, `Dual`, `Sum`,
   `Product`, `Min`, `Max`, `First`, `Last`, `WrappedMonoid`, `Arg`, `Any`,
   `All`, `Alt`, and `Compose`.
 * Add `NFData1` instances for applicable `Vector` types.

# Changes in version 0.12.0.3
  * Monad Fail support

# Changes in version 0.12.0.2
  * Fixes issue #220, compact heap operations crashing on boxed vectors constructed
    using traverse.
  * backport injective type family support
  * Cleanup the memset code internal to storable vector modules to be
    compatible with future Primitive releases

# Changes in version 0.12.0.1

 * Make sure `length` can be inlined
 * Include modules that test-suites depend on in other-modules

# Changes in version 0.12.0.0

 * Documentation fixes/additions
 * New functions: createT, iscanl/r, iterateNM, unfoldrM, uniq
 * New instances for various vector types: Semigroup, MonadZip
 * Made `Storable` vectors respect memory alignment
 * Changed some macros to ConstraintKinds
   - Dropped compatibility with old GHCs to support this
 * Add `Eq1`, `Ord1`, `Show1`, and `Read1` `Vector` instances, and related
   helper functions.
 * Relax context for `Unbox (Complex a)`.

# Changes in version 0.11.0.0

 * Define `Applicative` instances for `Data.Vector.Fusion.Util.{Box,Id}`
 * Define non-bottom `fail` for `instance Monad Vector`
 * New generalized stream fusion framework
 * Various safety fixes
   - Various overflows due to vector size have been eliminated
   - Memory is initialized on creation of unboxed vectors
 * Changes to SPEC usage to allow building under more conditions

# Changes in version 0.10.12.3

 * Allow building with `primtive-0.6`

# Changes in version 0.10.12.2

 * Add support for `deepseq-1.4.0.0`

# Changes in version 0.10.12.1

 * Fixed compilation on non-head GHCs

# Changes in version 0.10.12.0

 * Export MVector constructor from Data.Vector.Primitive to match Vector's
   (which was already exported).

 * Fix building on GHC 7.9 by adding Applicative instances for Id and Box

# Changes in version 0.10.11.0

 * Support OverloadedLists for boxed Vector in GHC >= 7.8

# Changes in version 0.10.10.0

 * Minor version bump to rectify PVP violation occured in 0.10.9.3 release

# Changes in version 0.10.9.3 (deprecated)

 * Add support for OverloadedLists in GHC >= 7.8

# Changes in version 0.10.9.2

 * Fix compilation with GHC 7.9

# Changes in version 0.10.9.1

 * Implement poly-kinded Typeable

# Changes in version 0.10.0.1

 * Require `primitive` to include workaround for a GHC array copying bug

# Changes in version 0.10

 * `NFData` instances
 * More efficient block fills
 * Safe Haskell support removed
