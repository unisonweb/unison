## Changelog for the `vault` package

**0.3.1.6**

Added

* Add support for MicroHs

Removed

* Dropped support for `GHC < 8.4`

**0.3.1.5**

* Compatibility with GHC-9.0
     * Bump dependencies to allow `base >= 4.5 && < 4.16`
* Depend on `semigroups` only with `GHC < 8.0`

**0.3.1.4**

* Compatibility with GHC-8.10
     * Bump dependencies to allow `base >= 4.5 && < 4.15`

**0.3.1.3**

* Bump dependencies to allow `hashable-1.3`.
* Compatibility with GHC-8.8
     * Bump dependencies to allow `base >= 4.5 && < 4.14`

**0.3.1.2**

* Bump dependencies to allow `containers-0.6`.
* Compatibility with GHC-8.6
     * Bump dependencies to allow `base >= 4.5 && < 4.13`.


**0.3.1.1**

* Rename source files to allow building the package with [Bazel](https://bazel.build).

**0.3.1.0**

* Compatibility with GHC-8.4:
     * Bump dependencies to allow `base >= 4.5 && < 4.12`.
     * Add `Semigroup` instances.

**0.3.0.7**

* Bump dependencies to allow `base >= 4.5 && < 4.11`.

**0.3.0.6**

* Bump dependencies to allow `base >= 4.5 && < 4.10`.

**0.3.0.5**

* Add support for GHC type roles extension, using the `RoleAnnotations` language pragma.

**0.3.0.4**

* Bump upper version bound for the `base` dependency.

**0.3.0.3**

* Bump upper version bound for the `base` dependency.

**0.3.0.2**

* Fix tarball.

**0.3.0.1**

* Use CPP to reduce code duplication.

**0.3.0.0**

* Split modules into Lazy and Strict variants, no default choice.
* Add Hashable instance to `Data.Unique.Really` for all implementations.
