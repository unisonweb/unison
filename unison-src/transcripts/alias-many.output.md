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
  77.  Int.pow : Int -> Nat -> Int
  78.  Int.shiftLeft : Int -> Nat -> Int
  79.  Int.shiftRight : Int -> Nat -> Int
  80.  Int.signum : Int -> Int
  81.  Int.toFloat : Int -> Float
  82.  Int.toText : Int -> Text
  83.  Int.truncate0 : Int -> Nat
  84.  unique type Link
  85.  builtin type Link.Term
  86.  Link.Term : Term -> Link
  87.  builtin type Link.Type
  88.  Link.Type : Type -> Link
  89.  builtin type List
  90.  List.++ : [a] -> [a] -> [a]
  91.  List.+: : a -> [a] -> [a]
  92.  List.:+ : [a] -> a -> [a]
  93.  List.at : Nat -> [a] -> Optional a
  94.  List.cons : a -> [a] -> [a]
  95.  List.drop : Nat -> [a] -> [a]
  96.  List.empty : [a]
  97.  List.size : [a] -> Nat
  98.  List.snoc : [a] -> a -> [a]
  99.  List.take : Nat -> [a] -> [a]
  100. builtin type Nat
  101. Nat.* : Nat -> Nat -> Nat
  102. Nat.+ : Nat -> Nat -> Nat
  103. Nat./ : Nat -> Nat -> Nat
  104. Nat.drop : Nat -> Nat -> Nat
  105. Nat.eq : Nat -> Nat -> Boolean
  106. Nat.fromText : Text -> Optional Nat
  107. Nat.gt : Nat -> Nat -> Boolean
  108. Nat.gteq : Nat -> Nat -> Boolean
  109. Nat.increment : Nat -> Nat
  110. Nat.isEven : Nat -> Boolean
  111. Nat.isOdd : Nat -> Boolean
  112. Nat.lt : Nat -> Nat -> Boolean
  113. Nat.lteq : Nat -> Nat -> Boolean
  114. Nat.mod : Nat -> Nat -> Nat
  115. Nat.pow : Nat -> Nat -> Nat
  116. Nat.shiftLeft : Nat -> Nat -> Nat
  117. Nat.shiftRight : Nat -> Nat -> Nat
  118. Nat.sub : Nat -> Nat -> Int
  119. Nat.toFloat : Nat -> Float
  120. Nat.toInt : Nat -> Int
  121. Nat.toText : Nat -> Text
  122. type Optional a
  123. Optional.None : Optional a
  124. Optional.Some : a -> Optional a
  125. builtin type Request
  126. unique type Test.Result
  127. Test.Result.Fail : Text -> Result
  128. Test.Result.Ok : Text -> Result
  129. builtin type Text
  130. Text.!= : Text -> Text -> Boolean
  131. Text.++ : Text -> Text -> Text
  132. Text.drop : Nat -> Text -> Text
  133. Text.empty : Text
  134. Text.eq : Text -> Text -> Boolean
  135. Text.fromCharList : [Char] -> Text
  136. Text.gt : Text -> Text -> Boolean
  137. Text.gteq : Text -> Text -> Boolean
  138. Text.lt : Text -> Text -> Boolean
  139. Text.lteq : Text -> Text -> Boolean
  140. Text.size : Text -> Nat
  141. Text.take : Nat -> Text -> Text
  142. Text.toCharList : Text -> [Char]
  143. Text.uncons : Text -> Optional (Char, Text)
  144. Text.unsnoc : Text -> Optional (Text, Char)
  145. type Tuple a b
  146. Tuple.Cons : a -> b -> Tuple a b
  147. type Unit
  148. Unit.Unit : ()
  149. Universal.< : a -> a -> Boolean
  150. Universal.<= : a -> a -> Boolean
  151. Universal.== : a -> a -> Boolean
  152. Universal.> : a -> a -> Boolean
  153. Universal.>= : a -> a -> Boolean
  154. Universal.compare : a -> a -> Int
  155. bug : a -> b
  156. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type Nat
    2.  List.cons  : a -> [a] -> [a]
    3.  List.drop  : Nat -> [a] -> [a]
    4.  List.empty : [a]
    5.  List.size  : [a] -> Nat
    6.  List.snoc  : [a] -> a -> [a]
    7.  List.take  : Nat -> [a] -> [a]
    8.  Nat.*      : Nat -> Nat -> Nat
    9.  Nat.+      : Nat -> Nat -> Nat
    10. Nat./      : Nat -> Nat -> Nat
    11. Nat.drop   : Nat -> Nat -> Nat
  
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
  6.  List.cons : a -> [a] -> [a]
  7.  List.drop : Nat -> [a] -> [a]
  8.  List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  9.  List.empty : [a]
  10. List.first : [a] -> Optional a
  11. List.init : [a] -> Optional [a]
  12. List.intersperse : a -> [a] -> [a]
  13. List.isEmpty : [a] -> Boolean
  14. List.last : [a] -> Optional a
  15. List.replicate : Nat -> a -> [a]
  16. List.size : [a] -> Nat
  17. List.snoc : [a] -> a -> [a]
  18. List.splitAt : Nat -> [a] -> ([a], [a])
  19. List.tail : [a] -> Optional [a]
  20. List.take : Nat -> [a] -> [a]
  21. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  22. builtin type Nat
  23. Nat.* : Nat -> Nat -> Nat
  24. Nat.+ : Nat -> Nat -> Nat
  25. Nat./ : Nat -> Nat -> Nat
  26. Nat.drop : Nat -> Nat -> Nat
  

```
Thanks, `alias.many!
