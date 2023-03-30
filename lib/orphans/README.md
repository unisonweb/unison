Canonical orphans packages.

The package name template is

```
<package-providing-type>-orphans-<package-providing-typeclass>
```

(with some flexibility as to what exactly `<package-providing-typeclass>` is called; for example, although we are 
currently using the `sqlite-simple` package, we call suffix sqlite orphan packages with `-sqlite`, not `-sqlite-simple`,
for no great reason other than aesthetics).

For example, the package that defines `To/FromField` instances (from `sqlite-simple`) for types defined in `unison-core`
is called `unison-core-orphans-sqlite`.
