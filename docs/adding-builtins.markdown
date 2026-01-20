This document explains how to add builtins to the language by working
through the example of adding `MVar` and some associated functions.

## Builtin Data

The logical first step for this example is to add a built-in `MVar`
type, whose values will simply be wrapped values of the Haskell type
with the same name. The 'old' runtime deviates from this approach for
several types, but this is how e.g. `Text` works even there.

Data types, including opaque pseudo data types of this sort are
referred to by `Reference`. Builtin, opaque data types use the
`Builtin` constructor with an appropriate name. The ones in actual
use are listed in the `Unison.Type` module, so we'll add a definition
there:

```haskell
mvarRef :: Reference
mvarRef = Reference.Builtin "MVar"
```

This definition alone won't do anything, however. It is merely
something for other definitions to refer to. If the reference is used
in e.g. the type of a function definitions without giving it an actual
name in the codebase, the type will be displayed with the raw hash,
which looks like `#MVar`.

The builtin reference can be given a name during the `builtins.merge`
ucm command. To make this happen, we must modify the `builtinTypesSrc`
definition in the `Unison.Builtin` module. This is just a list of
values that describe various builtin type related actions to be
performed during that command. In this case, we will add two values to
the list:

```haskell
B' "MVar" CT.Data
```

This specifies that there should be a builtin data type referring to
the `Builtin "MVar"` reference. The codebase name assigned to this is
the same as the reference (MVar here), but nested in the `builtin`
namespace. However, we will also add the value:

```haskell
Rename' "MVar" "io2.MVar"
```
because this is a type to be used with the new IO functions, which are
currently nested under the `io2` namespace. With both of these added
to the list, running `builtins.merge` should have a `builtin.io2.MVar`
type referring to the `Builtin "MVar"` reference.

The reason for both a `B'` and a `Rename'` is that eventually one
would expect the IO functionality to be moved from the `io2`
namespace. However, the builtin reference name may not be changed
easily, so it is preferable to have it named in the eventual expected
way, rather than permanently named `io2.MVar` internally.

## Builtin function declarations

The next step is to declare builtin functions that make use of the new
type. These are declared in a similar way to the type names above.
There is another list in `Unison.Builtin`, `builtinsSrc`, that defines
values specifying what builtin functions should exist.

Like the builtin type list, there are declarations for adding a
builtin function with a given name, and declarations for renaming from
the given name to a different namespace location. For the `MVar`
functions, we'll again give them their intended names as the original,
and rename them to the `io2` namespace for the time being.

Builtin functions also have an associated type as part of the initial
declaration. So for the complete specification of a function, we will
add declarations similar to:

```haskell
B "MVar.new" $ forall1 "a" (\a -> a --> io (mvar a))
Rename "MVar.new" "io2.MVar.new"
B "MVar.take" $ forall1 "a" (\a -> mvar a --> iof a)
Rename "MVar.take" "io2.MVar.take"
```

The `forall1`, `io`, `iof` and `-->` functions are local definitions
in `Unison.Builtin` for assistance in writing the types. `iof`
indicates that an error result may be returned, while `io` should
always succeed. Note that when the `{IO}` ability appears as a type
parameter rather than the return type of a function, you will need to
use `iot` instead.
`mvar` can be defined locally using some other
helpers in scope:

```haskell
mvar :: Type -> Type
mvar a = Type.ref () Type.mvarRef `app` a
```

For the actual `MVar` implementation, we'll be doing many definitions
followed by renames, so it'll be factored into a list of the name and
type, and we can then call the `moveUnder` helper to generate the `B`
declaration and the `Rename`.

## Builtin function implementation -- interpreter

What we have done so far only declares the functions and their types.
There is nothing yet implementing them. This section will proceed
through the implementation backing the declarations of the `MVar.new`
and `MVar.take` above.

In this case, we will implement the operations using the 'foreign
function' machinery. This path is somewhat less optimized, but
doesn't require inventing opcodes and modifying the runtime at
quite as low a level. The builtin 'foreign' functions are declared
in `Unison.Runtime.Builtin`, in a definition `declareForeigns`. We
can declare our builtins there by adding:

```haskell
  declareForeign Tracked 1 MVar_new
  declareForeign Tracked 1 MVar_take
```

These lines do multiple things at once. The first argument to
`declareForeign` determines whether the function should be explicitly
tracked by the Unison Cloud sandboxing functionality or not. As a general
guideline, functions in `{IO}` are `Tracked`, and pure functions are
`Untracked`. The second argument specifies the number of arguments that
the operation takes, so that appropriate wrapper code can be generated.
The last argument is a constructor of the `ForeignFunc` type that
identifies the operation.

This last bit leads us to the `Unison.Runtime.Foreign.Function.Type`
module, where we must add constructors for our new function. This is also
where we give a name to each constructor, which must match the one used
earlier in `Unison.Builtin`.

Finally, we must give the function implementation in
`Unison.Runtime.Foreign.Function`. This looks like

```haskell
  MVar_new -> mkForeign $ \(c :: Val) -> newMVar c
  MVar_take -> mkForeignIOF $ \(mv :: MVar Val) -> takeMVar mv
```

The helper functions `mkForeign` and `mkForeignIOF` assist in wrapping
Haskell functions into a form recognized by the interpreter. The latter
will catch exceptions and yield them in Unison as a `Failure` result (the
overall result type of the function should be an `Either` in Unison).
There is also `mkForeignExn` which will produce a foreign function whose
unison type is `... ->{Exception} a`, and `mkForeignIOExn` which is like
this, except automatically catches IO exceptions to create the `Failure`.

At first, our declarations will cause an error, because some of the
automatic machinery for creating builtin 'foreign' functions does not
exist for `MVar`. To rectify this, there are two options.

First, we can add a case to the `Foreign` type in `Unison.Runtime.Stack`.
For instance:

```haskell
  | WrapMVar !(MVar Val)
```

If this is done, some cases will need to be filled in in various
functions, to support equality, ordering, etc. Also, a `BuiltinForeign`
instance should be added to provide information on how to (un)wrap values
of the type. This is what `mkForeign` needs to work.

A second option is to encode the Haskell type as another already wrapped
type (or possibly one of Unison's closure types). In this case, a
`ForeignConvention` instance should be defined. That specifies several
functions for translating between the Unison representation and the
Haskell type. For instance

```haskell
instance {-# OVERLAPPING #-} ForeignConvention String where
  decodeVal = decodeAsBuiltin unpack
  encodeVal = encodeAsBuiltin pack

  readAtIndex = readAsBuiltin unpack
  writeBack = writeAsBuiltin pack
```

This instance allows wrapping Haskell functions that take/return
`String`, using Unison's `Text` as the representation, which is mediated
by the `(un)pack` functions.

With these in place, the functions should now be usable in the new
runtime.

## Decompilation

If it makes sense for an added type, it is possible to add to Unison's
ability to decompile runtime values or test for universal
equality/ordering. Directly embedded Haskell types are wrapped in the
`Foreign` type, and are decompiled in `Unison.Runtime.Decompile` using
the `decompileForeign` function. For instance, `Text` is decompiled in
the case:

```haskell
  WrapText t -> pure $ text () (Text.toText t)
```

Further cases may be added to match on your newly added type.

## Transcripts

One last thing remains. The additional builtin operations will have
changed some of the transcript output. The transcript runner should be
executed, and modified files should be checked and committed, so that
CI tests will pass (which check transcripts against an expected
result).
