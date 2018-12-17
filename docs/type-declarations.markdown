draft draft draft

# Type Declarations in Unison

## Structural Types

Structural types are defined uniquely by their structure.  For example, the following types are identical and interoperable:

```haskell
structural Maybe a = Nothing | Just a
structural Optional t = Some t | None
```

The identity of a structural type is determined by sorting the constructors <in some way involving the positional type args and other type hashes they reference>.

## Nominal Types
note: https://en.wikipedia.org/wiki/Contrast_set

### Alternative 1
Nominal types are defined by a GUID generated at the time they are added to a codebase.  They are not interoperable with any other type.

Question: Does this mean that an identically named type with identical contents aded by two people, or added by the same person on different days are totally unrelated?  This seems unfortunate, but maybe unavoidable under this model.

### Alternative 2
A nominal type is defined by a hash of its constructors, which are indexed by name; e.g. the following types are identical:

```haskell
nominal Tint a = Red | Green | Blue | Other a
nominal Hue b = Blue | Green | Red | Other b
```
and these two values are the same:
```haskell
x = Tint.Red
y = Hue.Red
```

Arya: Might be a good idea to newtype these for different application contexts, to help in later refactoring e.g.:
```
nominal Color = Red | Green | Blue
nominal MainLamp = MainLamp Color
nominal SideLamp = SideLamp Color
```
This way, if `Color` is updated to include a `Color.Yellow` constructor, you can opt in to the update independently for `MainLamp` vs `SideLamp` usage sites, rather than having to more deeply understand the code around each `Color` type usage to opt in or out of the upgrade.

## Opaque Types

ehh these are like newtypes but with associated definitions that can see through the newtype

```
opaque type Tagged s t = s where
  tag :: s -> Tagged s t
  tag s = s
  untag :: Tagged s t -> s
  untag st = st
  deepTag :: f s -> f (Tagged s t)
  deepTag fs  = fs
  deepUntag :: f (Tagged s t) -> f s
  deepUntag fst = fst
```

## Algebraic Types

Algebraic types are defined by their structure and a set of laws relating their fields.  Note that the laws may involve more than one type.

```
algebraic Monoid a = Monoid { mempty : a, mappend : a -> a -> a }
where m a -> (mappend m) (mempty m) a == a
      m a -> (mappend m) a (mempty m) == a
      m a b c -> (mappend m) a ((mappend m) b c) ==
                 (mappend m) ((mappend m) a b) c
```
