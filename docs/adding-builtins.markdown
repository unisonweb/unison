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
B "MVar.take" $ forall1 "a" (\a -> mvar a --> ioe a)
Rename "MVar.take" "io2.MVar.take"
```

The `forall1`, `io`, `ioe` and `-->` functions are local definitions
in `Unison.Builtin` for assistance in writing the types. `ioe`
indicates that an error result may be returned, while `io` should
always succeed.  `mvar` can be defined locally using some other
helpers in scope:

```haskell
mvar :: Var v => Type v -> Type v
mvar a = Type.ref () Type.mvarRef `app` a
```

For the actual `MVar` implementation, we'll be doing many definitions
followed by renames, so it'll be factored into a list of the name and
type, together with a function that generates the declaration and the
rename.

## Builtin function implementation -- new runtime

What we have done so far only declares the functions and their types.
There is nothing yet implementing them. This section will proceed
through the implementation backing the declaration of the `MVar.new`
declared above.

The first step is to add to the builtin operations list in
`Unison.Runtime.Builtin`. This is a list associating the name chosen
in `Unison.Builtin` with intermediate code forms that the new runtime
should use. In this case we can add:

```haskell
("MVar.new", ioComb mvar'new)
("MVar.take", ioComb mvar'take)
```

and plan to implement `mvar'new` and `mvar'take`. `ioComb` is a helper
function that factors out some of the repetitive aspects of dealing
with IO builtins, which `MVar` functions will need.

To actually implement the intermediate code, we will need to add
constructors to the `IOp` type in `Unison.Runtime.ANF`, which lists
all the builtin operations that will compile to the runtime's 'foreign
function' format, which is less efficient than e.g. arithemtic
operations, but is very flexible. The convention for this type is to
make constructors look sort of like opcode names, so we'll pick
`MVNEWF` for allocating a new filled `MVar`, and `MVTAKE` for taking
an `MVar`.

Then we must wrap these 'opcodes' like so:

```haskell
mvar'new :: IOOP
mvar'new avoid
  = ([BX],)
  . TAbs init
  $ TIOp MVNEWF [init]
  where
  [init] = freshes' avoid 1

mvar'take :: IOOP
mvar'take avoid
  = ([BX],)
  . TAbs mv
  $ io'error'result'direct MVTAKE [mv] ior e r
  where
  [mv,ior,e,r] = freshes' avoid 4
```

The breakdown of what is happening here is as follows:
- `avoid` is a set of variables that are not fresh, and should be
  avoided
- An `IOOP` may take many arguments, and the list in the tuple section
  specifies the calling convention for them. `[BX]` means one boxed
  argument, which in this case is the value of type `a`. `[BX,BX]`
  would be two boxed arguments, and `[BX,UN]` would be one boxed and
  one unboxed argument.
- `TAbs init` abstracts the argument variable, which we got from
  `freshes'` at the bottom. Multiple arguments may be abstracted with
  e.g. `TAbss [x,y,z]`
- `io'error'result'direct` is a helper function for calling an `IOp`
  and wrapping up a possible error result. The first argument is the
  `IOp` to call, the list is the arguments, and the last three
  arguments are variables used in the common result handling code.

Other builtins use slightly different implementations, so looking at
other parts of the file may be instructive, depending on what is being
added.

Finally, we need to provide an implementation for the 'opcode' we
defined. In this case, we need to add a case to `iopToForeign` also in
`Unison.Runtime.Builtin`. This will be simple, just:

```haskell
iopToForeign ANF.MVNEWF = mkForeign $ \(c :: Closure) -> newMVar c
iopToForeign ANF.MVTAKE
  = mkForeignIOE $ \(mv :: MVar Closure) -> takeMVar mv
```

The `mkForeignIOE` function inserts some code for catching exceptions
and explicitly returning them from the function.

However, at first this will cause an error, because some of the
automatic machinery for creating builtin 'foreign' functions does not
exist for `MVar`. To rectify this, we can add a `ForeignConvention`
instance in `Unison.Runtime.Foreign.Function` that specifies how to
automatically marshal `MVar Closure`, which is the representation
we'll be using.

```haskell
instance ForeignConvention (MVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)
```

This takes advantage of the `Closure` instance, and uses helper
functions that apply (un)wrappers from another convention.

With these in place, the functions should now be usable in the new
runtime.

## Transcripts

One last thing remains. The additional builtin operations will have
changed some of the transcript output. The transcript runner should be
executed, and modified files should be checked and committed, so that
CI tests will pass (which check transcripts against an expected
result).
