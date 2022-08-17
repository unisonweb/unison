
Test for code serialization operations.

Define a function, serialize it, then deserialize it back to an actual
function. Also ask for its dependencies for display later.

```unison
save : a -> Bytes
save x = Value.serialize (Value.value x)

Code.save : Code -> Bytes
Code.save = Code.serialize

Code.get : Link.Term -> Code
Code.get tl = match Code.lookup tl with
  Some co -> co
  None -> throw "could not look up code"

load : Bytes ->{io2.IO, Throw Text} a
load b = match Value.deserialize b with
  Left _ -> throw "could not deserialize value"
  Right v -> match Value.load v with
    Left _ -> throw "could not load value"
    Right x -> x

Code.load : Bytes ->{io2.IO, Throw Text} Code
Code.load b = match Code.deserialize b with
  Left _ -> throw "could not deserialize code"
  Right co -> co

roundtrip : a ->{io2.IO, Throw Text} a
roundtrip x = load (save x)

handleTest : Text -> Request {Throw Text} a -> Result
handleTest t = let
  pfx = "(" ++ t ++ ") "
  cases
    { _ } -> Ok (pfx ++ "passed")
    { Throw.throw s -> _ } -> Fail (pfx ++ s)

identical : Text -> a -> a ->{Throw Text} ()
identical err x y =
  if x == y
  then ()
  else throw ("mismatch" ++ err)

structural type Three a b c = zero a | one b | two c

showThree : Three Nat Nat Nat -> Text
showThree = cases
  zero n -> "zero " ++ toText n
  one n -> "one " ++ toText n
  two n -> "two " ++ toText n

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = cases
  [] -> []
  x +: xs -> f x ++ concatMap f xs

prod : [a] -> [b] -> [(a,b)]
prod l = cases
  [] -> []
  y +: ys -> map (x -> (x,y)) l ++ prod l ys

threes : [Three Nat Nat Nat]
threes = map zero fib10 ++ map one fib10 ++ map two fib10

extensionals
   : (a -> b -> Text)
  -> (a -> b -> c)
  -> (a -> b -> c)
  -> [(a,b)] ->{Throw Text} ()
extensionals sh f g = cases
  [] -> ()
  (x,y) +: xs ->
    identical (" on: " ++ sh x y) (f x y) (g x y)
    extensionals sh f g xs

fib10 : [Nat]
fib10 = [1,2,3,5,8,13,21,34,55,89]

extensionality : Text -> (Three Nat Nat Nat -> Nat -> b) ->{io2.IO} Result
extensionality t f = let
  sh t n = "(" ++ showThree t ++ ", " ++ toText n ++ ")"
  handle
    g = roundtrip f
    extensionals sh f g (prod threes fib10)
  with handleTest t

identicality : Text -> a ->{io2.IO} Result
identicality t x
  = handle identical "" x (roundtrip x) with handleTest t

idempotence : Text -> Link.Term ->{io2.IO} Result
idempotence t tl =
  handle let
    co1 = Code.get tl
    b1 = Code.save co1
    co2 = Code.load b1
    b2 = Code.save co2
    identical "" b1 b2
  with handleTest t
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Three a b c
      Code.get       : Link.Term ->{IO, Throw Text} Code
      Code.load      : Bytes ->{IO, Throw Text} Code
      Code.save      : Code -> Bytes
      concatMap      : (a ->{g} [b]) -> [a] ->{g} [b]
      extensionality : Text
                       -> (Three Nat Nat Nat -> Nat -> b)
                       ->{IO} Result
      extensionals   : (a -> b -> Text)
                       -> (a -> b -> c)
                       -> (a -> b -> c)
                       -> [(a, b)]
                       ->{Throw Text} ()
      fib10          : [Nat]
      handleTest     : Text -> Request {Throw Text} a -> Result
      idempotence    : Text -> Link.Term ->{IO} Result
      identical      : Text -> a -> a ->{Throw Text} ()
      identicality   : Text -> a ->{IO} Result
      load           : Bytes ->{IO, Throw Text} a
      prod           : [a] -> [b] -> [(a, b)]
      roundtrip      : a ->{IO, Throw Text} a
      save           : a -> Bytes
      showThree      : Three Nat Nat Nat -> Text
      threes         : [Three Nat Nat Nat]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Three a b c
    Code.get       : Link.Term ->{IO, Throw Text} Code
    Code.load      : Bytes ->{IO, Throw Text} Code
    Code.save      : Code -> Bytes
    concatMap      : (a ->{g} [b]) -> [a] ->{g} [b]
    extensionality : Text
                     -> (Three Nat Nat Nat -> Nat -> b)
                     ->{IO} Result
    extensionals   : (a -> b -> Text)
                     -> (a -> b -> c)
                     -> (a -> b -> c)
                     -> [(a, b)]
                     ->{Throw Text} ()
    fib10          : [Nat]
    handleTest     : Text -> Request {Throw Text} a -> Result
    idempotence    : Text -> Link.Term ->{IO} Result
    identical      : Text -> a -> a ->{Throw Text} ()
    identicality   : Text -> a ->{IO} Result
    load           : Bytes ->{IO, Throw Text} a
    prod           : [a] -> [b] -> [(a, b)]
    roundtrip      : a ->{IO, Throw Text} a
    save           : a -> Bytes
    showThree      : Three Nat Nat Nat -> Text
    threes         : [Three Nat Nat Nat]

```
```unison
structural ability Zap where
  zap : Three Nat Nat Nat

h : Three Nat Nat Nat -> Nat -> Nat
h y x = match y with
  zero y -> x + y
  one y -> x + y + y
  two y -> x + 3*y

f : Nat ->{Zap} Nat
f x = h zap x

fVal : Value
fVal = Value.value f

fDeps : [Link.Term]
fDeps = Value.dependencies fVal

fSer : Bytes
fSer = Value.serialize fVal

rotate : Three Nat Nat Nat -> Three Nat Nat Nat
rotate = cases
  zero n -> one (n+1)
  one n -> two (n+2)
  two n -> zero (drop n 6)

zapper : Three Nat Nat Nat -> Request {Zap} r -> r
zapper t = cases
  { r } -> r
  { zap -> k } -> handle k t with zapper (rotate t)

bigFun : Nat -> Nat -> Nat -> Nat
bigFun i j k = let
  f x y = i + x + y
  g x y = j + x + y
  h x y = k + x + y
  f j k + g i k + h i j

tests : '{io2.IO} [Result]
tests =
  '[ extensionality "ext f" (t x -> handle f x with zapper t)
   , extensionality "ext h" h
   , identicality "ident compound" (x -> handle f x with zapper (zero 5))
   , identicality "ident fib10" fib10
   , identicality "ident effect" (_ -> zap)
   , identicality "ident zero" zero
   , identicality "ident h" h
   , identicality "ident text" "hello"
   , identicality "ident int" +5
   , identicality "ident float" 0.5
   , identicality "ident termlink" fDeps
   , identicality "ident bool" false
   , identicality "ident bytes" [fSer, Bytes.empty]
   ]

badLoad : '{IO} [Result]
badLoad _ =
  payload = Bytes.fromList[0,0,0,1,0,1,64,175,174,29,188,217,78,209,175,255,137,165,135,165,1,20,151,182,215,54,21,196,43,159,247,106,175,177,213,20,111,178,134,214,188,207,243,196,240,187,111,44,245,111,219,223,98,88,183,163,97,22,18,153,104,185,125,175,157,36,209,151,166,168,102,0,1,0,0,0,0,0,2,0,0,0,0]
  go _ =
    match Value.deserialize payload with
      Left t -> Fail "deserialize exception"
      Right a -> match Value.load a with
        Left terms ->
            bs = Value.serialize (Value.value terms)
            s = size bs
            Ok ("serialized" ++ toText s)
        Right _ ->
            Ok "actually loaded"
  match toEither go with
    Right v -> [v]
    Left _ -> [Fail "Exception"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Zap
      badLoad : '{IO} [Result]
      bigFun  : Nat -> Nat -> Nat -> Nat
      f       : Nat ->{Zap} Nat
      fDeps   : [Link.Term]
      fSer    : Bytes
      fVal    : Value
      h       : Three Nat Nat Nat -> Nat -> Nat
      rotate  : Three Nat Nat Nat -> Three Nat Nat Nat
      tests   : '{IO} [Result]
      zapper  : Three Nat Nat Nat -> Request {Zap} r -> r

```
This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

```ucm
.> add

  ⍟ I've added these definitions:
  
    structural ability Zap
    badLoad : '{IO} [Result]
    bigFun  : Nat -> Nat -> Nat -> Nat
    f       : Nat ->{Zap} Nat
    fDeps   : [Link.Term]
    fSer    : Bytes
    fVal    : Value
    h       : Three Nat Nat Nat -> Nat -> Nat
    rotate  : Three Nat Nat Nat -> Three Nat Nat Nat
    tests   : '{IO} [Result]
    zapper  : Three Nat Nat Nat -> Request {Zap} r -> r

.> io.test tests

    New test results:
  
  ◉ tests   (ext f) passed
  ◉ tests   (ext h) passed
  ◉ tests   (ident compound) passed
  ◉ tests   (ident fib10) passed
  ◉ tests   (ident effect) passed
  ◉ tests   (ident zero) passed
  ◉ tests   (ident h) passed
  ◉ tests   (ident int) passed
  ◉ tests   (ident float) passed
  ◉ tests   (ident termlink) passed
  ◉ tests   (ident bool) passed
  
  ✗ tests   (ident text) mismatch
  ✗ tests   (ident bytes) mismatch
  
  🚫 2 test(s) failing, ✅ 11 test(s) passing
  
  Tip: Use view tests to view the source of a test.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


    New test results:
  
  ◉ tests   (ext f) passed
  ◉ tests   (ext h) passed
  ◉ tests   (ident compound) passed
  ◉ tests   (ident fib10) passed
  ◉ tests   (ident effect) passed
  ◉ tests   (ident zero) passed
  ◉ tests   (ident h) passed
  ◉ tests   (ident int) passed
  ◉ tests   (ident float) passed
  ◉ tests   (ident termlink) passed
  ◉ tests   (ident bool) passed
  
  ✗ tests   (ident text) mismatch
  ✗ tests   (ident bytes) mismatch
  
  🚫 2 test(s) failing, ✅ 11 test(s) passing
  
  Tip: Use view tests to view the source of a test.

