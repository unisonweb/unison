The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be used in the target namespace are the names you specify, relative to the current namespace:

```
.> help alias.many

  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates aliases `relative1`, `relative2`, ...
  in the namespace `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases `.quux.foo.foo` and `.quux.bar.bar`.

```

Let's try it!

```ucm
.> cd .builtin

.builtin> find

  1.   builtin type Boolean
  2.   Boolean.not : Boolean -> Boolean
  3.   builtin type Bytes
  4.   Bytes.++ : Bytes -> Bytes -> Bytes
  5.   Bytes.at : Nat -> Bytes -> Optional Nat
  6.   Bytes.drop : Nat -> Bytes -> Bytes
  7.   Bytes.empty : Bytes
  8.   Bytes.flatten : Bytes -> Bytes
  9.   Bytes.fromList : [Nat] -> Bytes
  10.  Bytes.size : Bytes -> Nat
  11.  Bytes.take : Nat -> Bytes -> Bytes
  12.  Bytes.toList : Bytes -> [Nat]
  13.  builtin type Char
  14.  Char.fromNat : Nat -> Char
  15.  Char.toNat : Char -> Nat
  16.  Debug.watch : Text -> a -> a
  17.  unique type Doc
  18.  Doc.Blob : Text -> Doc
  19.  Doc.Evaluate : Term -> Doc
  20.  Doc.Join : [Doc] -> Doc
  21.  Doc.Link : Link -> Doc
  22.  Doc.Signature : Term -> Doc
  23.  Doc.Source : Link -> Doc
  24.  builtin type Float
  25.  Float.* : Float -> Float -> Float
  26.  Float.+ : Float -> Float -> Float
  27.  Float.- : Float -> Float -> Float
  28.  Float./ : Float -> Float -> Float
  29.  Float.abs : Float -> Float
  30.  Float.acos : Float -> Float
  31.  Float.acosh : Float -> Float
  32.  Float.asin : Float -> Float
  33.  Float.asinh : Float -> Float
  34.  Float.atan : Float -> Float
  35.  Float.atan2 : Float -> Float -> Float
  36.  Float.atanh : Float -> Float
  37.  Float.ceiling : Float -> Int
  38.  Float.cos : Float -> Float
  39.  Float.cosh : Float -> Float
  40.  Float.eq : Float -> Float -> Boolean
  41.  Float.exp : Float -> Float
  42.  Float.floor : Float -> Int
  43.  Float.fromText : Text -> Optional Float
  44.  Float.gt : Float -> Float -> Boolean
  45.  Float.gteq : Float -> Float -> Boolean
  46.  Float.log : Float -> Float
  47.  Float.logBase : Float -> Float -> Float
  48.  Float.lt : Float -> Float -> Boolean
  49.  Float.lteq : Float -> Float -> Boolean
  50.  Float.max : Float -> Float -> Float
  51.  Float.min : Float -> Float -> Float
  52.  Float.pow : Float -> Float -> Float
  53.  Float.round : Float -> Int
  54.  Float.sin : Float -> Float
  55.  Float.sinh : Float -> Float
  56.  Float.sqrt : Float -> Float
  57.  Float.tan : Float -> Float
  58.  Float.tanh : Float -> Float
  59.  Float.toText : Float -> Text
  60.  Float.truncate : Float -> Int
  61.  builtin type Int
  62.  Int.* : Int -> Int -> Int
  63.  Int.+ : Int -> Int -> Int
  64.  Int.- : Int -> Int -> Int
  65.  Int./ : Int -> Int -> Int
  66.  Int.eq : Int -> Int -> Boolean
  67.  Int.fromText : Text -> Optional Int
  68.  Int.gt : Int -> Int -> Boolean
  69.  Int.gteq : Int -> Int -> Boolean
  70.  Int.increment : Int -> Int
  71.  Int.isEven : Int -> Boolean
  72.  Int.isOdd : Int -> Boolean
  73.  Int.lt : Int -> Int -> Boolean
  74.  Int.lteq : Int -> Int -> Boolean
  75.  Int.mod : Int -> Int -> Int
  76.  Int.negate : Int -> Int
  77.  Int.signum : Int -> Int
  78.  Int.toFloat : Int -> Float
  79.  Int.toText : Int -> Text
  80.  Int.truncate0 : Int -> Nat
  81.  unique type Link
  82.  builtin type Link.Term
  83.  Link.Term : Term -> Link
  84.  builtin type Link.Type
  85.  Link.Type : Type -> Link
  86.  builtin type List
  87.  List.++ : [a] -> [a] -> [a]
  88.  List.+: : a -> [a] -> [a]
  89.  List.:+ : [a] -> a -> [a]
  90.  List.at : Nat -> [a] -> Optional a
  91.  List.cons : a -> [a] -> [a]
  92.  List.drop : Nat -> [a] -> [a]
  93.  List.empty : [a]
  94.  List.size : [a] -> Nat
  95.  List.snoc : [a] -> a -> [a]
  96.  List.take : Nat -> [a] -> [a]
  97.  builtin type Nat
  98.  Nat.* : Nat -> Nat -> Nat
  99.  Nat.+ : Nat -> Nat -> Nat
  100. Nat./ : Nat -> Nat -> Nat
  101. Nat.drop : Nat -> Nat -> Nat
  102. Nat.eq : Nat -> Nat -> Boolean
  103. Nat.fromText : Text -> Optional Nat
  104. Nat.gt : Nat -> Nat -> Boolean
  105. Nat.gteq : Nat -> Nat -> Boolean
  106. Nat.increment : Nat -> Nat
  107. Nat.isEven : Nat -> Boolean
  108. Nat.isOdd : Nat -> Boolean
  109. Nat.lt : Nat -> Nat -> Boolean
  110. Nat.lteq : Nat -> Nat -> Boolean
  111. Nat.mod : Nat -> Nat -> Nat
  112. Nat.sub : Nat -> Nat -> Int
  113. Nat.toFloat : Nat -> Float
  114. Nat.toInt : Nat -> Int
  115. Nat.toText : Nat -> Text
  116. type Optional a
  117. Optional.None : Optional a
  118. Optional.Some : a -> Optional a
  119. builtin type Request
  120. unique type Test.Result
  121. Test.Result.Fail : Text -> Result
  122. Test.Result.Ok : Text -> Result
  123. builtin type Text
  124. Text.!= : Text -> Text -> Boolean
  125. Text.++ : Text -> Text -> Text
  126. Text.drop : Nat -> Text -> Text
  127. Text.empty : Text
  128. Text.eq : Text -> Text -> Boolean
  129. Text.fromCharList : [Char] -> Text
  130. Text.gt : Text -> Text -> Boolean
  131. Text.gteq : Text -> Text -> Boolean
  132. Text.lt : Text -> Text -> Boolean
  133. Text.lteq : Text -> Text -> Boolean
  134. Text.size : Text -> Nat
  135. Text.take : Nat -> Text -> Text
  136. Text.toCharList : Text -> [Char]
  137. Text.uncons : Text -> Optional (Char, Text)
  138. Text.unsnoc : Text -> Optional (Text, Char)
  139. type Tuple a b
  140. Tuple.Cons : a -> b -> Tuple a b
  141. type Unit
  142. Unit.Unit : ()
  143. Universal.< : a -> a -> Boolean
  144. Universal.<= : a -> a -> Boolean
  145. Universal.== : a -> a -> Boolean
  146. Universal.> : a -> a -> Boolean
  147. Universal.>= : a -> a -> Boolean
  148. Universal.compare : a -> a -> Int
  149. bug : a -> b
  150. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type Nat
    2.  List.size    : [a] -> Nat
    3.  List.snoc    : [a] -> a -> [a]
    4.  List.take    : Nat -> [a] -> [a]
    5.  Nat.*        : Nat -> Nat -> Nat
    6.  Nat.+        : Nat -> Nat -> Nat
    7.  Nat./        : Nat -> Nat -> Nat
    8.  Nat.drop     : Nat -> Nat -> Nat
    9.  Nat.eq       : Nat -> Nat -> Boolean
    10. Nat.fromText : Text -> Optional Nat
    11. Nat.gt       : Nat -> Nat -> Boolean
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar

.runar> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  3.  List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  7.  List.first : [a] -> Optional a
  8.  List.init : [a] -> Optional [a]
  9.  List.intersperse : a -> [a] -> [a]
  10. List.isEmpty : [a] -> Boolean
  11. List.last : [a] -> Optional a
  12. List.replicate : Nat -> a -> [a]
  13. List.splitAt : Nat -> [a] -> ([a], [a])
  14. List.tail : [a] -> Optional [a]
  15. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  

.runar> alias.many 1-15 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  List.adjacentPairs : [a] -> [(a, a)]
    2.  List.all           : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} Boolean
    3.  List.any           : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} Boolean
    4.  List.chunk         : Nat -> [a] -> [[a]]
    5.  List.chunksOf      : Nat -> [a] -> [[a]]
    6.  List.dropWhile     : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} [a]
    7.  List.first         : [a] -> Optional a
    8.  List.init          : [a] -> Optional [a]
    9.  List.intersperse   : a -> [a] -> [a]
    10. List.isEmpty       : [a] -> Boolean
    11. List.last          : [a] -> Optional a
    12. List.replicate     : Nat -> a -> [a]
    13. List.splitAt       : Nat -> [a] -> ([a], [a])
    14. List.tail          : [a] -> Optional [a]
    15. List.takeWhile     : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  
  Tip: You can use `undo` or `reflog` to undo this change.

.runar> cd .mylib

.mylib> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  3.  List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  7.  List.first : [a] -> Optional a
  8.  List.init : [a] -> Optional [a]
  9.  List.intersperse : a -> [a] -> [a]
  10. List.isEmpty : [a] -> Boolean
  11. List.last : [a] -> Optional a
  12. List.replicate : Nat -> a -> [a]
  13. List.size : [a] -> Nat
  14. List.snoc : [a] -> a -> [a]
  15. List.splitAt : Nat -> [a] -> ([a], [a])
  16. List.tail : [a] -> Optional [a]
  17. List.take : Nat -> [a] -> [a]
  18. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  19. builtin type Nat
  20. Nat.* : Nat -> Nat -> Nat
  21. Nat.+ : Nat -> Nat -> Nat
  22. Nat./ : Nat -> Nat -> Nat
  23. Nat.drop : Nat -> Nat -> Nat
  24. Nat.eq : Nat -> Nat -> Boolean
  25. Nat.fromText : Text -> Optional Nat
  26. Nat.gt : Nat -> Nat -> Boolean
  

```
Thanks, `alias.many!
