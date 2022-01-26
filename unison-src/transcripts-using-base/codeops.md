
Test for code serialization operations.

```ucm:hide
.> builtins.merge
.> cd builtin
```

Define a function, serialize it, then deserialize it back to an actual
function. Also ask for its dependencies for display later.

```unison
save : a -> Bytes
save x = Value.serialize (Value.value x)

load : Bytes ->{io2.IO, Throw Text} a
load b = match Value.deserialize b with
  Left _ -> throw "could not deserialize value"
  Right v -> match Value.load v with
    Left _ -> throw "could not load value"
    Right x -> x

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
```

```ucm
.> add
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

This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

```ucm
.> add
.> display fDeps
.> io.test tests
.> io.test badLoad
```

```unison
validateTest : Link.Term ->{IO} Result
validateTest l = match Code.lookup l with
  None -> Fail "Couldn't look up link"
  Some co -> match Code.validate [(l, co)] with
    Some f -> Fail "invalid code pre"
    None -> match Code.deserialize (Code.serialize co) with
      Left _ -> Fail "code failed deserialization"
      Right co -> match Code.validate [(l, co)] with
        Some f -> Fail "invalid code post"
        None -> Ok "validated"

vtests : '{IO} [Result]
vtests _ =
  List.map validateTest
    [ termLink fib10
    , termLink compose
    , termLink List.all
    , termLink hex
    , termLink isDirectory
    , termLink delay
    , termLink printLine
    , termLink isNone
    ]
```

```ucm
.> add
.> io.test vtests
```
