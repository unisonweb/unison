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

``` ucm
scratch/main> share.render @unison/base/releases/7.20.0:Optional.map

  Apply a function to the value in a Optional value if it is
  Some. Otherwise, return None.

  # Examples

        Optional.map Nat.increment (Some 2)
        ⧨
        Some 3

        Optional.map Nat.increment None
        ⧨
        None

scratch/main> share.render @unison/base/releases/7.20.0:Optional.map.doc

  Apply a function to the value in a Optional value if it is
  Some. Otherwise, return None.

  # Examples

        Optional.map Nat.increment (Some 2)
        ⧨
        Some 3

        Optional.map Nat.increment None
        ⧨
        None
```

`share.docs` of a type with long documentation: code examples inside a section of nested text keep their lines and
spacing, instead of being wrapped like words (the examples under "Handlers").

``` ucm
scratch/main> share.docs @unison/base/releases/7.20.0:abilities.Random

  ability abilities.Random where

  An ability for generating pseudorandom values.

  # Sample usage

        splitmix 1 do List.replicate 5 boolean
        ⧨
        [true, false, true, true, true]

    This example uses the splitmix algorithm, initialized from
    an arbitrarily chosen seed value, `1`. This provides random
    values to the boolean generator, and List.replicate uses
    that generator 5 times to produce a list of 5 random
    booleans.

    If you'd rather not select a seed and you're in a context
    where you're able to use IO, you can use Random.run instead
    of splitmix. This will use the system's secure random number
    generator to seed the splitmix algorithm, and then use that
    to generate values:

        Random.run do List.replicate 5 boolean

  # Generating random values

    Generate random Bytes of a given length:

        Random.bytes : Nat ->{Random} Bytes

    Generate a random Boolean:

        boolean : '{Random} Boolean

    # Random numbers
    
      Generate a random Nat value in the range 0 to maxNat:
    
          nat : '{Random} Nat
    
      Generate a random Int value in the range minInt to maxInt:
    
          Random.int : '{Random} Int
    
      Generate a random Nat in a given range:
    
          natIn : Nat -> Nat ->{Random} Nat
    
      Generate a random Int in a given range:
    
          intIn : Int -> Int ->{Random} Int
    
      Generate a uniformly distributed Float from `0.0` up to
      but not including `1.0` :
    
          Random.float : '{Random} Float

    # Text and characters
    
      Generate a random Base32Hex string:
    
          text.base32Hex : '{Random} Text
    
      Generate a random Base32Hex character:
    
          char.base32Hex : '{Random} Char
    
      Generate a random decimal digit:
    
          char.digit : '{Random} Char
    
      Generate a random hexadecimal digit:
    
          hex : '{Random} Char
    
      Generate a random Char in a given range:
    
          interval : Char -> Char ->{Random} Char

  # Combinators on generators

    Generate a random value from a list of possible values:

        Random.oneOf : [a] ->{Random} a

    Generate a List of values of a given length from a
    generator:

        listOf : '{g, Random} a -> '{g, Random} Nat ->{g, Random} [a]

    Generate a value from one of two generators with equal
    probability:

        either : '{Random} r -> '{Random} r ->{Random} r

    Shuffle a List of values:

        shuffle : [a] ->{Random} [a]

  # Splitting the generator

    Create a new supply of Random values from an existing one:

        Random.split : '{Random} ('{g, Random} t ->{g} t)

    This is useful for concurrent use: each thread can be given
    its own supply of random numbers.

  # Handlers

    splitmix is a prototypical handler of this ability that uses
    the SplitMix algorithm to generate pseudorandom values. It
    is seeded with a value provided by the user. The generator
    is not cryptographically secure, but it's fast and has good
    statistical properties.

    # Examples
    
          splitmix 853322303 do List.replicate 10 do natIn 0 100
          ⧨
          [57, 26, 37, 79, 80, 96, 5, 89, 30, 80]
    
          splitmix 1 do shuffle (Nat.range 1 11)
          ⧨
          [8, 1, 5, 10, 6, 7, 9, 3, 2, 4]
    
      Random.run is a handler that uses IO to generate the
      initial seed value, and then uses splitmix to generate
      values. This is useful when you have access to the IO
      ability and don't want to choose a seed value yourself.
    
      lcg is a simpler handler of this ability that uses a
      linear congruential generator to generate pseudorandom
      values. It is seeded with a value provided by the user.
      The generator is not cryptographically secure and prone to
      predictability in the output, but it's very simple.

    # Examples
    
          lcg 853322303 do List.replicate 10 boolean
          ⧨
          [true, false, true, false, false, false, true, false, false, false]
    
          lcg 1 do List.map (i -> natIn 0 i Nat.* 100) (Nat.range 1 11)
          ⧨
          [0, 100, 100, 0, 100, 100, 600, 500, 0, 300]
```

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
