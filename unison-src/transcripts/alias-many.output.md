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

  1.   AlreadyExists : IOError
  2.   builtin type Boolean
  3.   Boolean.not : Boolean -> Boolean
  4.   type BufferMode
  5.   BufferMode.BlockBuffering : BufferMode
  6.   BufferMode.LineBuffering : BufferMode
  7.   BufferMode.NoBuffering : BufferMode
  8.   BufferMode.SizedBlockBuffering : Nat -> BufferMode
  9.   builtin type Bytes
  10.  Bytes.++ : Bytes -> Bytes -> Bytes
  11.  Bytes.at : Nat -> Bytes -> Optional Nat
  12.  Bytes.drop : Nat -> Bytes -> Bytes
  13.  Bytes.empty : Bytes
  14.  Bytes.flatten : Bytes -> Bytes
  15.  Bytes.fromList : [Nat] -> Bytes
  16.  Bytes.size : Bytes -> Nat
  17.  Bytes.take : Nat -> Bytes -> Bytes
  18.  Bytes.toList : Bytes -> [Nat]
  19.  builtin type Char
  20.  Char.fromNat : Nat -> Char
  21.  Char.toNat : Char -> Nat
  22.  Debug.watch : Text -> a -> a
  23.  unique type Doc
  24.  Doc.Blob : Text -> Doc
  25.  Doc.Evaluate : Term -> Doc
  26.  Doc.Join : [Doc] -> Doc
  27.  Doc.Link : Link -> Doc
  28.  Doc.Signature : Term -> Doc
  29.  Doc.Source : Link -> Doc
  30.  EOF : IOError
  31.  type Either a b
  32.  Either.Left : a -> Either a b
  33.  Either.Right : b -> Either a b
  34.  type FileMode
  35.  FileMode.Append : FileMode
  36.  FileMode.Read : FileMode
  37.  FileMode.ReadWrite : FileMode
  38.  FileMode.Write : FileMode
  39.  builtin type Float
  40.  Float.* : Float -> Float -> Float
  41.  Float.+ : Float -> Float -> Float
  42.  Float.- : Float -> Float -> Float
  43.  Float./ : Float -> Float -> Float
  44.  Float.abs : Float -> Float
  45.  Float.acos : Float -> Float
  46.  Float.acosh : Float -> Float
  47.  Float.asin : Float -> Float
  48.  Float.asinh : Float -> Float
  49.  Float.atan : Float -> Float
  50.  Float.atan2 : Float -> Float -> Float
  51.  Float.atanh : Float -> Float
  52.  Float.ceiling : Float -> Int
  53.  Float.cos : Float -> Float
  54.  Float.cosh : Float -> Float
  55.  Float.eq : Float -> Float -> Boolean
  56.  Float.exp : Float -> Float
  57.  Float.floor : Float -> Int
  58.  Float.fromText : Text -> Optional Float
  59.  Float.gt : Float -> Float -> Boolean
  60.  Float.gteq : Float -> Float -> Boolean
  61.  Float.log : Float -> Float
  62.  Float.logBase : Float -> Float -> Float
  63.  Float.lt : Float -> Float -> Boolean
  64.  Float.lteq : Float -> Float -> Boolean
  65.  Float.max : Float -> Float -> Float
  66.  Float.min : Float -> Float -> Float
  67.  Float.pow : Float -> Float -> Float
  68.  Float.round : Float -> Int
  69.  Float.sin : Float -> Float
  70.  Float.sinh : Float -> Float
  71.  Float.sqrt : Float -> Float
  72.  Float.tan : Float -> Float
  73.  Float.tanh : Float -> Float
  74.  Float.toText : Float -> Text
  75.  Float.truncate : Float -> Int
  76.  IO.clientSocket : Text
                         -> Text
                         ->{##IO} Either IOError ##Socket
  77.  IO.closeFile : ##Handle ->{##IO} Either IOError ()
  78.  IO.closeSocket : ##Socket ->{##IO} Either IOError ()
  79.  IO.createDirectory : Text ->{##IO} Either IOError ()
  80.  IO.fileExists : Text ->{##IO} Either IOError Boolean
  81.  IO.forkComp : '{##IO} Either IOError a
                     ->{##IO} Either IOError ##ThreadId
  82.  IO.getBuffering : ##Handle
                         ->{##IO} Either IOError BufferMode
  83.  IO.getCurrentDirectory : '{##IO} Either IOError Text
  84.  IO.getFileSize : Text ->{##IO} Either IOError Nat
  85.  IO.getFileTimestamp : Text ->{##IO} Either IOError Nat
  86.  IO.getLine : ##Handle ->{##IO} Either IOError Text
  87.  IO.getTempDirectory : '{##IO} Either IOError Text
  88.  IO.getText : ##Handle ->{##IO} Either IOError Text
  89.  IO.handlePosition : ##Handle ->{##IO} Either IOError Int
  90.  IO.isDirectory : Text ->{##IO} Either IOError Boolean
  91.  IO.isFileEOF : ##Handle ->{##IO} Either IOError Boolean
  92.  IO.isFileOpen : ##Handle ->{##IO} Either IOError Boolean
  93.  IO.isSeekable : ##Handle ->{##IO} Either IOError Boolean
  94.  IO.listen : ##Socket ->{##IO} Either IOError ()
  95.  IO.openFile : Text ->{##IO} Either IOError ##Handle
  96.  IO.putText : ##Handle -> Text ->{##IO} Either IOError ()
  97.  IO.removeDirectory : Text ->{##IO} Either IOError ()
  98.  IO.removeFile : Text ->{##IO} Either IOError ()
  99.  IO.renameDirectory : Text
                            -> Text
                            ->{##IO} Either IOError ()
  100. IO.renameFile : Text -> Text ->{##IO} Either IOError ()
  101. IO.seekHandle : ##Handle
                       -> FileMode
                       -> Int
                       ->{##IO} Either IOError ()
  102. IO.serverSocket : Text
                         -> Text
                         ->{##IO} Either IOError ##Socket
  103. IO.setBuffering : ##Handle
                         -> BufferMode
                         ->{##IO} Either IOError ()
  104. IO.setCurrentDirectory : Text ->{##IO} Either IOError ()
  105. IO.socketAccept : ##Socket
                         ->{##IO} Either IOError ##Socket
  106. IO.socketReceive : ##Socket
                          -> Nat
                          ->{##IO} Either IOError Bytes
  107. IO.socketSend : ##Socket
                       -> Bytes
                       ->{##IO} Either IOError ()
  108. IO.stdHandle : Nat -> Optional ##Handle
  109. IO.systemTime : '{##IO} Either IOError Nat
  110. type IOError
  111. IllegalOperation : IOError
  112. builtin type Int
  113. Int.* : Int -> Int -> Int
  114. Int.+ : Int -> Int -> Int
  115. Int.- : Int -> Int -> Int
  116. Int./ : Int -> Int -> Int
  117. Int.and : Int -> Int -> Int
  118. Int.complement : Int -> Int
  119. Int.eq : Int -> Int -> Boolean
  120. Int.fromText : Text -> Optional Int
  121. Int.gt : Int -> Int -> Boolean
  122. Int.gteq : Int -> Int -> Boolean
  123. Int.increment : Int -> Int
  124. Int.isEven : Int -> Boolean
  125. Int.isOdd : Int -> Boolean
  126. Int.leadingZeros : Int -> Nat
  127. Int.lt : Int -> Int -> Boolean
  128. Int.lteq : Int -> Int -> Boolean
  129. Int.mod : Int -> Int -> Int
  130. Int.negate : Int -> Int
  131. Int.or : Int -> Int -> Int
  132. Int.pow : Int -> Nat -> Int
  133. Int.shiftLeft : Int -> Nat -> Int
  134. Int.shiftRight : Int -> Nat -> Int
  135. Int.signum : Int -> Int
  136. Int.toFloat : Int -> Float
  137. Int.toText : Int -> Text
  138. Int.trailingZeros : Int -> Nat
  139. Int.truncate0 : Int -> Nat
  140. Int.xor : Int -> Int -> Int
  141. unique type Link
  142. builtin type Link.Term
  143. Link.Term : Term -> Link
  144. builtin type Link.Type
  145. Link.Type : Type -> Link
  146. builtin type List
  147. List.++ : [a] -> [a] -> [a]
  148. List.+: : a -> [a] -> [a]
  149. List.:+ : [a] -> a -> [a]
  150. List.at : Nat -> [a] -> Optional a
  151. List.cons : a -> [a] -> [a]
  152. List.drop : Nat -> [a] -> [a]
  153. List.empty : [a]
  154. List.size : [a] -> Nat
  155. List.snoc : [a] -> a -> [a]
  156. List.take : Nat -> [a] -> [a]
  157. builtin type Nat
  158. Nat.* : Nat -> Nat -> Nat
  159. Nat.+ : Nat -> Nat -> Nat
  160. Nat./ : Nat -> Nat -> Nat
  161. Nat.and : Nat -> Nat -> Nat
  162. Nat.complement : Nat -> Nat
  163. Nat.drop : Nat -> Nat -> Nat
  164. Nat.eq : Nat -> Nat -> Boolean
  165. Nat.fromText : Text -> Optional Nat
  166. Nat.gt : Nat -> Nat -> Boolean
  167. Nat.gteq : Nat -> Nat -> Boolean
  168. Nat.increment : Nat -> Nat
  169. Nat.isEven : Nat -> Boolean
  170. Nat.isOdd : Nat -> Boolean
  171. Nat.leadingZeros : Nat -> Nat
  172. Nat.lt : Nat -> Nat -> Boolean
  173. Nat.lteq : Nat -> Nat -> Boolean
  174. Nat.mod : Nat -> Nat -> Nat
  175. Nat.or : Nat -> Nat -> Nat
  176. Nat.pow : Nat -> Nat -> Nat
  177. Nat.shiftLeft : Nat -> Nat -> Nat
  178. Nat.shiftRight : Nat -> Nat -> Nat
  179. Nat.sub : Nat -> Nat -> Int
  180. Nat.toFloat : Nat -> Float
  181. Nat.toInt : Nat -> Int
  182. Nat.toText : Nat -> Text
  183. Nat.trailingZeros : Nat -> Nat
  184. Nat.xor : Nat -> Nat -> Nat
  185. NoSuchThing : IOError
  186. type Optional a
  187. Optional.None : Optional a
  188. Optional.Some : a -> Optional a
  189. PermissionDenied : IOError
  190. builtin type Request
  191. ResourceBusy : IOError
  192. ResourceExhausted : IOError
  193. type SeqView a
  194. SeqView.VElem : a -> SeqView a -> SeqView a
  195. SeqView.VEmpty : SeqView a
  196. unique type Test.Result
  197. Test.Result.Fail : Text -> Result
  198. Test.Result.Ok : Text -> Result
  199. builtin type Text
  200. Text.!= : Text -> Text -> Boolean
  201. Text.++ : Text -> Text -> Text
  202. Text.drop : Nat -> Text -> Text
  203. Text.empty : Text
  204. Text.eq : Text -> Text -> Boolean
  205. Text.fromCharList : [Char] -> Text
  206. Text.gt : Text -> Text -> Boolean
  207. Text.gteq : Text -> Text -> Boolean
  208. Text.lt : Text -> Text -> Boolean
  209. Text.lteq : Text -> Text -> Boolean
  210. Text.size : Text -> Nat
  211. Text.take : Nat -> Text -> Text
  212. Text.toCharList : Text -> [Char]
  213. Text.uncons : Text -> Optional (Char, Text)
  214. Text.unsnoc : Text -> Optional (Text, Char)
  215. type Tuple a b
  216. Tuple.Cons : a -> b -> Tuple a b
  217. type Unit
  218. Unit.Unit : ()
  219. Universal.< : a -> a -> Boolean
  220. Universal.<= : a -> a -> Boolean
  221. Universal.== : a -> a -> Boolean
  222. Universal.> : a -> a -> Boolean
  223. Universal.>= : a -> a -> Boolean
  224. Universal.compare : a -> a -> Int
  225. UserError : IOError
  226. bug : a -> b
  227. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  IO.listen              : ##Socket
                               ->{##IO} Either IOError ()
    2.  IO.openFile            : Text
                               ->{##IO} Either IOError ##Handle
    3.  IO.putText             : ##Handle
                               -> Text
                               ->{##IO} Either IOError ()
    4.  IO.removeDirectory     : Text ->{##IO} Either IOError ()
    5.  IO.removeFile          : Text ->{##IO} Either IOError ()
    6.  IO.renameDirectory     : Text
                               -> Text
                               ->{##IO} Either IOError ()
    7.  IO.renameFile          : Text
                               -> Text
                               ->{##IO} Either IOError ()
    8.  IO.seekHandle          : ##Handle
                               -> FileMode
                               -> Int
                               ->{##IO} Either IOError ()
    9.  IO.serverSocket        : Text
                               -> Text
                               ->{##IO} Either IOError ##Socket
    10. IO.setBuffering        : ##Handle
                               -> BufferMode
                               ->{##IO} Either IOError ()
    11. IO.setCurrentDirectory : Text ->{##IO} Either IOError ()
  
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

  1.  IO.listen : ##Socket ->{##IO} Either IOError ()
  2.  IO.openFile : Text ->{##IO} Either IOError ##Handle
  3.  IO.putText : ##Handle -> Text ->{##IO} Either IOError ()
  4.  IO.removeDirectory : Text ->{##IO} Either IOError ()
  5.  IO.removeFile : Text ->{##IO} Either IOError ()
  6.  IO.renameDirectory : Text
                           -> Text
                           ->{##IO} Either IOError ()
  7.  IO.renameFile : Text -> Text ->{##IO} Either IOError ()
  8.  IO.seekHandle : ##Handle
                      -> FileMode
                      -> Int
                      ->{##IO} Either IOError ()
  9.  IO.serverSocket : Text
                        -> Text
                        ->{##IO} Either IOError ##Socket
  10. IO.setBuffering : ##Handle
                        -> BufferMode
                        ->{##IO} Either IOError ()
  11. IO.setCurrentDirectory : Text ->{##IO} Either IOError ()
  12. List.adjacentPairs : [a] -> [(a, a)]
  13. List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  14. List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  15. List.chunk : Nat -> [a] -> [[a]]
  16. List.chunksOf : Nat -> [a] -> [[a]]
  17. List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  18. List.first : [a] -> Optional a
  19. List.init : [a] -> Optional [a]
  20. List.intersperse : a -> [a] -> [a]
  21. List.isEmpty : [a] -> Boolean
  22. List.last : [a] -> Optional a
  23. List.replicate : Nat -> a -> [a]
  24. List.splitAt : Nat -> [a] -> ([a], [a])
  25. List.tail : [a] -> Optional [a]
  26. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  

```
Thanks, `alias.many!
