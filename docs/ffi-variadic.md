# Variadic foreign functions

`FFI.Spec.variadic : Nat -> FFI.Spec a -> FFI.Spec a` marks a complete FFI
signature as a call to a C variadic function. The count is the number of
fixed arguments before `...`. The signature describes **every** argument
in this particular call, including the optional arguments.

Each loaded binding is an ordinary, statically typed Unison function. To
call the same C symbol with another argument count or types, load it with
another specification. There is no dynamically typed argument list.

For example, on macOS ARM64, `ioctl(int, unsigned long, ...)` with a pointer
argument can be described using the base library's signature builders:

```unison
FFI.Spec.variadic 2 (FFI.Spec.fnIO3 FFI.Type.int32 FFI.Type.uint64 FFI.Type.ptr FFI.Type.int32)
```

The count must be at least one, no greater than the total argument count,
and representable as a C `unsigned int`. A count equal to the total is
valid: it denotes a variadic function called with no optional arguments.
It still uses the variadic calling convention.

Use C's default argument promotions in the optional portion:

* C `float` is passed as `double`.
* Integers narrower than C `int` are passed as their promoted integer type
  (normally `int32` on supported platforms).
* Other integer and pointer arguments retain their appropriate C types.

The runtime rejects unpromoted optional arguments. It does not promote
fixed arguments. It also rejects `void` arguments and pinned-array results,
as with ordinary FFI specifications. Validation occurs when loading a symbol,
through the existing `Exception` ability; constructing a specification is pure.

Applying `variadic` again replaces the fixed-argument count. Applying `arr`
to a variadic specification prepends a **fixed** argument and increases
the count. Usually it is clearest to build the complete signature first
and apply `variadic` last.

The runtime uses libffi's `ffi_prep_cif_var`. Existing specifications use
`ffi_prep_cif` as before. The argument-type array is retained alongside the
call interface, including across garbage collection and repeated calls.

This is a new runtime builtin. An older UCM cannot execute it. After building
the modified UCM, `builtins.mergeio` makes it available in a branch; base
can then expose the builtin with its public FFI API. This change does not
add native callbacks or OS signal subscriptions.

## macOS libffi compatibility

[Apple's system libffi](https://github.com/apple-oss-distributions/libffi/blob/main/src/prep_cif.c)
has an upstream validation bug: it checks every
argument after the first for default promotions, including fixed arguments.
Consequently, some valid signatures with a fixed `float` or narrow integer
after the first argument fail to load. `ioctl` and `snprintf` do not encounter
this restriction. Unison reports the load failure instead of changing the
declared fixed argument types or ignoring a libffi error.

For full support, link against a current libffi, for example Homebrew's
`libffi`. The focused tests cover this case and intentionally require a
library that accepts those valid signatures. When building with Stack on
Apple Silicon, the linker can be directed to Homebrew's library with
`--extra-lib-dirs=/opt/homebrew/opt/libffi/lib`; use the corresponding
Homebrew prefix on Intel. Check `otool -L` on the resulting executable to
verify which libffi it links to.

## Validation

The runtime's `ffi.dynamic` tests exercise mixed integer/pointer/double
arguments, register overflow, no optional arguments, fixed calls, interface
lifetime, and invalid specifications. The existing Unix and Windows DLL
transcripts also exercise calls from Unison, including a fixed `float`
argument followed by optional `double` arguments and symbol-load errors.
