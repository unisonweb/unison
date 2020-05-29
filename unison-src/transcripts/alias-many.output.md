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
  66.  Int.and : Int -> Int -> Int
  67.  Int.complement : Int -> Int
  68.  Int.eq : Int -> Int -> Boolean
  69.  Int.fromText : Text -> Optional Int
  70.  Int.gt : Int -> Int -> Boolean
  71.  Int.gteq : Int -> Int -> Boolean
  72.  Int.increment : Int -> Int
  73.  Int.isEven : Int -> Boolean
  74.  Int.isOdd : Int -> Boolean
  75.  Int.leadingZeros : Int -> Nat
  76.  Int.lt : Int -> Int -> Boolean
  77.  Int.lteq : Int -> Int -> Boolean
  78.  Int.mod : Int -> Int -> Int
  79.  Int.negate : Int -> Int
  80.  Int.or : Int -> Int -> Int
  81.  Int.pow : Int -> Nat -> Int
  82.  Int.shiftLeft : Int -> Nat -> Int
  83.  Int.shiftRight : Int -> Nat -> Int
  84.  Int.signum : Int -> Int
  85.  Int.toFloat : Int -> Float
  86.  Int.toText : Int -> Text
  87.  Int.trailingZeros : Int -> Nat
  88.  Int.truncate0 : Int -> Nat
  89.  Int.xor : Int -> Int -> Int
  90.  unique type Link
  91.  builtin type Link.Term
  92.  Link.Term : Term -> Link
  93.  builtin type Link.Type
  94.  Link.Type : Type -> Link
  95.  builtin type List
  96.  List.++ : [a] -> [a] -> [a]
  97.  List.+: : a -> [a] -> [a]
  98.  List.:+ : [a] -> a -> [a]
  99.  List.at : Nat -> [a] -> Optional a
  100. List.cons : a -> [a] -> [a]
  101. List.drop : Nat -> [a] -> [a]
  102. List.empty : [a]
  103. List.size : [a] -> Nat
  104. List.snoc : [a] -> a -> [a]
  105. List.take : Nat -> [a] -> [a]
  106. builtin type Nat
  107. Nat.* : Nat -> Nat -> Nat
  108. Nat.+ : Nat -> Nat -> Nat
  109. Nat./ : Nat -> Nat -> Nat
  110. Nat.and : Nat -> Nat -> Nat
  111. Nat.complement : Nat -> Nat
  112. Nat.drop : Nat -> Nat -> Nat
  113. Nat.eq : Nat -> Nat -> Boolean
  114. Nat.fromText : Text -> Optional Nat
  115. Nat.gt : Nat -> Nat -> Boolean
  116. Nat.gteq : Nat -> Nat -> Boolean
  117. Nat.increment : Nat -> Nat
  118. Nat.isEven : Nat -> Boolean
  119. Nat.isOdd : Nat -> Boolean
  120. Nat.leadingZeros : Nat -> Nat
  121. Nat.lt : Nat -> Nat -> Boolean
  122. Nat.lteq : Nat -> Nat -> Boolean
  123. Nat.mod : Nat -> Nat -> Nat
  124. Nat.or : Nat -> Nat -> Nat
  125. Nat.pow : Nat -> Nat -> Nat
  126. Nat.shiftLeft : Nat -> Nat -> Nat
  127. Nat.shiftRight : Nat -> Nat -> Nat
  128. Nat.sub : Nat -> Nat -> Int
  129. Nat.toFloat : Nat -> Float
  130. Nat.toInt : Nat -> Int
  131. Nat.toText : Nat -> Text
  132. Nat.trailingZeros : Nat -> Nat
  133. Nat.xor : Nat -> Nat -> Nat
  134. type Optional a
  135. Optional.None : Optional a
  136. Optional.Some : a -> Optional a
  137. builtin type Request
  138. unique type Test.Result
  139. Test.Result.Fail : Text -> Result
  140. Test.Result.Ok : Text -> Result
  141. builtin type Text
  142. Text.!= : Text -> Text -> Boolean
  143. Text.++ : Text -> Text -> Text
  144. Text.drop : Nat -> Text -> Text
  145. Text.empty : Text
  146. Text.eq : Text -> Text -> Boolean
  147. Text.fromCharList : [Char] -> Text
  148. Text.gt : Text -> Text -> Boolean
  149. Text.gteq : Text -> Text -> Boolean
  150. Text.lt : Text -> Text -> Boolean
  151. Text.lteq : Text -> Text -> Boolean
  152. Text.size : Text -> Nat
  153. Text.take : Nat -> Text -> Text
  154. Text.toCharList : Text -> [Char]
  155. Text.uncons : Text -> Optional (Char, Text)
  156. Text.unsnoc : Text -> Optional (Text, Char)
  157. type Tuple a b
  158. Tuple.Cons : a -> b -> Tuple a b
  159. type Unit
  160. Unit.Unit : ()
  161. Universal.< : a -> a -> Boolean
  162. Universal.<= : a -> a -> Boolean
  163. Universal.== : a -> a -> Boolean
  164. Universal.> : a -> a -> Boolean
  165. Universal.>= : a -> a -> Boolean
  166. Universal.compare : a -> a -> Int
  167. bug : a -> b
  168. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type List
    2.  Link.Type   : Type -> Link
    3.  List.++     : [a] -> [a] -> [a]
    4.  ┌ List.+:   : a -> [a] -> [a]
    5.  └ List.cons : a -> [a] -> [a]
    6.  ┌ List.:+   : [a] -> a -> [a]
    7.  └ List.snoc : [a] -> a -> [a]
    8.  List.at     : Nat -> [a] -> Optional a
    9.  List.drop   : Nat -> [a] -> [a]
    10. List.empty  : [a]
    11. List.size   : [a] -> Nat
  
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

  1.  Link.Type : Type -> Link
  2.  builtin type List
  3.  List.++ : [a] -> [a] -> [a]
  4.  List.+: : a -> [a] -> [a]
  5.  List.:+ : [a] -> a -> [a]
  6.  List.adjacentPairs : [a] -> [(a, a)]
  7.  List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  8.  List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  9.  List.at : Nat -> [a] -> Optional a
  10. List.chunk : Nat -> [a] -> [[a]]
  11. List.chunksOf : Nat -> [a] -> [[a]]
  12. List.cons : a -> [a] -> [a]
  13. List.drop : Nat -> [a] -> [a]
  14. List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  15. List.empty : [a]
  16. List.first : [a] -> Optional a
  17. List.init : [a] -> Optional [a]
  18. List.intersperse : a -> [a] -> [a]
  19. List.isEmpty : [a] -> Boolean
  20. List.last : [a] -> Optional a
  21. List.replicate : Nat -> a -> [a]
  22. List.size : [a] -> Nat
  23. List.snoc : [a] -> a -> [a]
  24. List.splitAt : Nat -> [a] -> ([a], [a])
  25. List.tail : [a] -> Optional [a]
  26. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  

```
Thanks, `alias.many!
