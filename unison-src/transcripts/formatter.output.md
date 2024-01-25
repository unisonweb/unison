```unison
{{ # Doc
This is a *doc*! 

term link {x}

type link {type   Optional}

}}
x : 
  Nat 
  -> Nat
x y =
    x   =     1 + 1
    x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc = {{
# Here's a top-level doc

With a paragraph

Or two
}}
-- After

{{ A doc before an ability }}
ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int

{{ 
A Doc before a type 
}}
structural type Optional   a = More Text 
  | Some 
  | Other   a 
  | None Nat 

{{ 
two 
}}
qqq = 2


{{ 
hi 
}}
test = 2


{{ yo }}
structural type MyEither = MyEither

```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type MyEither
        (also named builtin.Unit)
      structural type Optional a
      ability Thing
      MyEither.doc : Doc2
      Optional.doc : Doc2
      Thing.doc    : Doc2
      explicit.doc : Doc2
      oneLiner     : Doc2
      qqq          : Nat
      qqq.doc      : Doc2
      test         : Nat
      test.doc     : Doc2
      x            : Nat -> Nat
      x.doc        : Doc2

```
```ucm
.> debug.format

```
```unison:added-by-ucm scratch.u
x.doc =
  {{ # Doc This is a **doc**!
  
    term link {x}
    
    type link {type Optional} }}
x : Nat -> Nat
x y =
  use Nat +
  x = 1 + 1
  x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc =
  {{ # Here's a top-level doc
  
    With a paragraph
    
    Or two }}
-- After

Thing.doc = {{ A doc before an ability }}
ability Thing where
  more : Nat -> Text ->{Thing} Nat
  doThing : Nat ->{Thing} Int

Optional.doc = {{ A Doc before a type }}
structural type Optional a = More Text | Some | Other a | None Nat 

{{ 
two 
}}
qqq = 2


qqq.doc = {{ two }}
qqq = 22


MyEither.doc = {{ yo }}
structural type MyEither = MyEither = MyEither
```

Formatter should leave things alone if the file doesn't typecheck.

```unison
brokenDoc = {{ hello }} + 1
```

```ucm

  Loading changes detected in scratch.u.

  I couldn't find any definitions matching the name + inside the namespace .
  
      1 | brokenDoc = {{ hello }} + 1
  
  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
  
  To add a library to this project use the command: `fork <.path.to.lib> .lib.<libname>`
  
  Whatever it is, its type should conform to Doc2 -> Nat -> o.
  
  I found some terms in scope with matching names but different types. If one of these is what you meant, try using the fully qualified name and I might be able to give you a more illuminating error message: 
  
    - builtin.Float.+ : Float -> Float -> Float
    - builtin.Int.+ : Int -> Int -> Int
    - builtin.Nat.+ : Nat -> Nat -> Nat

```
```ucm
.> debug.format

```
