# The `share.*` commands

These read from Unison Share and never touch the local codebase. The references below are pinned to a release so
that the output is stable. (`share.project-search` and the project README, whose results change over time, aren't
tested here.)

``` ucm
scratch/main> share.source @unison/base/releases/7.20.0:Optional.map

  Optional.map : (a ->{𝕖} b) -> Optional a ->{𝕖} Optional b
  Optional.map f = cases
    None   -> None
    Some a -> Some (f a)

scratch/main> share.source @unison/base/releases/7.20.0:Optional

  structural type Optional a = None | Some a
```

`share.render` prints the docs of a definition, or a doc itself.

```` ucm
scratch/main> share.render @unison/base/releases/7.20.0:Optional.map

  Apply a function to the value in a `Optional` value if it is `Some` . Otherwise, return `None` .

  # Examples

  ```unison
  Optional.map Nat.increment (Some 2)
  ⧨
  Some 3

  ```

  ```unison
  Optional.map Nat.increment None
  ⧨
  None

  ```


scratch/main> share.render @unison/base/releases/7.20.0:Optional.map.doc

  Apply a function to the value in a `Optional` value if it is `Some` . Otherwise, return `None` .

  # Examples

  ```unison
  Optional.map Nat.increment (Some 2)
  ⧨
  Some 3

  ```

  ```unison
  Optional.map Nat.increment None
  ⧨
  None

  ```

````

Names that don't exist, and references without a name where one is needed, are errors.

``` ucm :error
scratch/main> share.source @unison/base/releases/7.20.0:Nope.nothing

  ❗️

  Not found on Share: Nope.nothing
```

``` ucm :error
scratch/main> share.source @unison/base/releases/7.20.0

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I expected a definition name after the `:`, like
    @user/project:some.name

  You can run `help share.source` for more information on using
  `share.source`.
```

``` ucm :error
scratch/main> share.source not-a-reference

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    Expected a reference like @user/project,
    @user/project/releases/1.2.3 or @user/project:some.name, but
    got not-a-reference

  You can run `help share.source` for more information on using
  `share.source`.
```
