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
  16.  builtin type Code
  17.  Code.cache_ : [Tuple Term Code] ->{IO} [Term]
  18.  Code.dependencies : Code -> [Term]
  19.  Code.deserialize : Bytes -> Either Text Code
  20.  Code.isMissing : Term ->{IO} Boolean
  21.  Code.lookup : Term ->{IO} Optional Code
  22.  Code.serialize : Code -> Bytes
  23.  Debug.watch : Text -> a -> a
  24.  unique type Doc
  25.  Doc.Blob : Text -> Doc
  26.  Doc.Evaluate : Term -> Doc
  27.  Doc.Join : [Doc] -> Doc
  28.  Doc.Link : Link -> Doc
  29.  Doc.Signature : Term -> Doc
  30.  Doc.Source : Link -> Doc
  31.  type Either a b
  32.  Either.Left : a -> Either a b
  33.  Either.Right : b -> Either a b
  34.  builtin type Float
  35.  Float.* : Float -> Float -> Float
  36.  Float.+ : Float -> Float -> Float
  37.  Float.- : Float -> Float -> Float
  38.  Float./ : Float -> Float -> Float
  39.  Float.abs : Float -> Float
  40.  Float.acos : Float -> Float
  41.  Float.acosh : Float -> Float
  42.  Float.asin : Float -> Float
  43.  Float.asinh : Float -> Float
  44.  Float.atan : Float -> Float
  45.  Float.atan2 : Float -> Float -> Float
  46.  Float.atanh : Float -> Float
  47.  Float.ceiling : Float -> Int
  48.  Float.cos : Float -> Float
  49.  Float.cosh : Float -> Float
  50.  Float.eq : Float -> Float -> Boolean
  51.  Float.exp : Float -> Float
  52.  Float.floor : Float -> Int
  53.  Float.fromText : Text -> Optional Float
  54.  Float.gt : Float -> Float -> Boolean
  55.  Float.gteq : Float -> Float -> Boolean
  56.  Float.log : Float -> Float
  57.  Float.logBase : Float -> Float -> Float
  58.  Float.lt : Float -> Float -> Boolean
  59.  Float.lteq : Float -> Float -> Boolean
  60.  Float.max : Float -> Float -> Float
  61.  Float.min : Float -> Float -> Float
  62.  Float.pow : Float -> Float -> Float
  63.  Float.round : Float -> Int
  64.  Float.sin : Float -> Float
  65.  Float.sinh : Float -> Float
  66.  Float.sqrt : Float -> Float
  67.  Float.tan : Float -> Float
  68.  Float.tanh : Float -> Float
  69.  Float.toText : Float -> Text
  70.  Float.truncate : Float -> Int
  71.  builtin type Int
  72.  Int.* : Int -> Int -> Int
  73.  Int.+ : Int -> Int -> Int
  74.  Int.- : Int -> Int -> Int
  75.  Int./ : Int -> Int -> Int
  76.  Int.and : Int -> Int -> Int
  77.  Int.complement : Int -> Int
  78.  Int.eq : Int -> Int -> Boolean
  79.  Int.fromText : Text -> Optional Int
  80.  Int.gt : Int -> Int -> Boolean
  81.  Int.gteq : Int -> Int -> Boolean
  82.  Int.increment : Int -> Int
  83.  Int.isEven : Int -> Boolean
  84.  Int.isOdd : Int -> Boolean
  85.  Int.leadingZeros : Int -> Nat
  86.  Int.lt : Int -> Int -> Boolean
  87.  Int.lteq : Int -> Int -> Boolean
  88.  Int.mod : Int -> Int -> Int
  89.  Int.negate : Int -> Int
  90.  Int.or : Int -> Int -> Int
  91.  Int.pow : Int -> Nat -> Int
  92.  Int.shiftLeft : Int -> Nat -> Int
  93.  Int.shiftRight : Int -> Nat -> Int
  94.  Int.signum : Int -> Int
  95.  Int.toFloat : Int -> Float
  96.  Int.toText : Int -> Text
  97.  Int.trailingZeros : Int -> Nat
  98.  Int.truncate0 : Int -> Nat
  99.  Int.xor : Int -> Int -> Int
  100. unique type Link
  101. builtin type Link.Term
  102. Link.Term : Term -> Link
  103. builtin type Link.Type
  104. Link.Type : Type -> Link
  105. builtin type List
  106. List.++ : [a] -> [a] -> [a]
  107. List.+: : a -> [a] -> [a]
  108. List.:+ : [a] -> a -> [a]
  109. List.at : Nat -> [a] -> Optional a
  110. List.cons : a -> [a] -> [a]
  111. List.drop : Nat -> [a] -> [a]
  112. List.empty : [a]
  113. List.size : [a] -> Nat
  114. List.snoc : [a] -> a -> [a]
  115. List.take : Nat -> [a] -> [a]
  116. builtin type Nat
  117. Nat.* : Nat -> Nat -> Nat
  118. Nat.+ : Nat -> Nat -> Nat
  119. Nat./ : Nat -> Nat -> Nat
  120. Nat.and : Nat -> Nat -> Nat
  121. Nat.complement : Nat -> Nat
  122. Nat.drop : Nat -> Nat -> Nat
  123. Nat.eq : Nat -> Nat -> Boolean
  124. Nat.fromText : Text -> Optional Nat
  125. Nat.gt : Nat -> Nat -> Boolean
  126. Nat.gteq : Nat -> Nat -> Boolean
  127. Nat.increment : Nat -> Nat
  128. Nat.isEven : Nat -> Boolean
  129. Nat.isOdd : Nat -> Boolean
  130. Nat.leadingZeros : Nat -> Nat
  131. Nat.lt : Nat -> Nat -> Boolean
  132. Nat.lteq : Nat -> Nat -> Boolean
  133. Nat.mod : Nat -> Nat -> Nat
  134. Nat.or : Nat -> Nat -> Nat
  135. Nat.pow : Nat -> Nat -> Nat
  136. Nat.shiftLeft : Nat -> Nat -> Nat
  137. Nat.shiftRight : Nat -> Nat -> Nat
  138. Nat.sub : Nat -> Nat -> Int
  139. Nat.toFloat : Nat -> Float
  140. Nat.toInt : Nat -> Int
  141. Nat.toText : Nat -> Text
  142. Nat.trailingZeros : Nat -> Nat
  143. Nat.xor : Nat -> Nat -> Nat
  144. type Optional a
  145. Optional.None : Optional a
  146. Optional.Some : a -> Optional a
  147. builtin type Request
  148. type SeqView a b
  149. SeqView.VElem : a -> b -> SeqView a b
  150. SeqView.VEmpty : SeqView a b
  151. unique type Test.Result
  152. Test.Result.Fail : Text -> Result
  153. Test.Result.Ok : Text -> Result
  154. builtin type Text
  155. Text.!= : Text -> Text -> Boolean
  156. Text.++ : Text -> Text -> Text
  157. Text.drop : Nat -> Text -> Text
  158. Text.empty : Text
  159. Text.eq : Text -> Text -> Boolean
  160. Text.fromCharList : [Char] -> Text
  161. Text.gt : Text -> Text -> Boolean
  162. Text.gteq : Text -> Text -> Boolean
  163. Text.lt : Text -> Text -> Boolean
  164. Text.lteq : Text -> Text -> Boolean
  165. Text.size : Text -> Nat
  166. Text.take : Nat -> Text -> Text
  167. Text.toCharList : Text -> [Char]
  168. Text.uncons : Text -> Optional (Char, Text)
  169. Text.unsnoc : Text -> Optional (Text, Char)
  170. type Tuple a b
  171. Tuple.Cons : a -> b -> Tuple a b
  172. type Unit
  173. Unit.Unit : ()
  174. Universal.< : a -> a -> Boolean
  175. Universal.<= : a -> a -> Boolean
  176. Universal.== : a -> a -> Boolean
  177. Universal.> : a -> a -> Boolean
  178. Universal.>= : a -> a -> Boolean
  179. Universal.compare : a -> a -> Int
  180. builtin type Value
  181. Value.dependencies : Value -> [Term]
  182. Value.deserialize : Bytes -> Either Text Value
  183. Value.load : Value ->{IO} Either [Term] a
  184. Value.serialize : Value -> Bytes
  185. Value.value : a -> Value
  186. bug : a -> b
  187. unique type io2.BufferMode
  188. io2.BufferMode.BlockBuffering : BufferMode
  189. io2.BufferMode.LineBuffering : BufferMode
  190. io2.BufferMode.NoBuffering : BufferMode
  191. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  192. unique type io2.FileMode
  193. io2.FileMode.Append : FileMode
  194. io2.FileMode.Read : FileMode
  195. io2.FileMode.ReadWrite : FileMode
  196. io2.FileMode.Write : FileMode
  197. builtin type io2.Handle
  198. builtin type io2.IO
  199. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  200. io2.IO.closeFile : Handle ->{IO} Either IOError ()
  201. io2.IO.closeSocket : Socket ->{IO} Either IOError ()
  202. io2.IO.createDirectory : Text ->{IO} Either IOError ()
  203. io2.IO.delay : Nat ->{IO} Either IOError ()
  204. io2.IO.fileExists : Text ->{IO} Either IOError Boolean
  205. io2.IO.forkComp : '{IO} Either IOError a ->{IO} ThreadId
  206. io2.IO.getBuffering : Handle
                             ->{IO} Either IOError BufferMode
  207. io2.IO.getCurrentDirectory : '{IO} Either IOError Text
  208. io2.IO.getFileSize : Text ->{IO} Either IOError Nat
  209. io2.IO.getFileTimestamp : Text ->{IO} Either IOError Nat
  210. io2.IO.getLine : Handle ->{IO} Either IOError Text
  211. io2.IO.getTempDirectory : '{IO} Either IOError Text
  212. io2.IO.getText : Handle ->{IO} Either IOError Text
  213. io2.IO.handlePosition : Handle ->{IO} Either IOError Int
  214. io2.IO.isDirectory : Text ->{IO} Either IOError Boolean
  215. io2.IO.isFileEOF : Handle ->{IO} Either IOError Boolean
  216. io2.IO.isFileOpen : Handle ->{IO} Either IOError Boolean
  217. io2.IO.isSeekable : Handle ->{IO} Either IOError Boolean
  218. io2.IO.kill : ThreadId ->{IO} Either IOError ()
  219. io2.IO.listen : Socket ->{IO} Either IOError ()
  220. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either IOError Handle
  221. io2.IO.putText : Handle -> Text ->{IO} Either IOError ()
  222. io2.IO.removeDirectory : Text ->{IO} Either IOError ()
  223. io2.IO.removeFile : Text ->{IO} Either IOError ()
  224. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either IOError ()
  225. io2.IO.renameFile : Text -> Text ->{IO} Either IOError ()
  226. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either IOError ()
  227. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  228. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either IOError ()
  229. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either IOError ()
  230. io2.IO.socketAccept : Socket ->{IO} Either IOError Socket
  231. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either IOError Bytes
  232. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either IOError ()
  233. io2.IO.stdHandle : StdHandle -> Handle
  234. io2.IO.systemTime : '{IO} Either IOError Nat
  235. unique type io2.IOError
  236. io2.IOError.AlreadyExists : IOError
  237. io2.IOError.EOF : IOError
  238. io2.IOError.IllegalOperation : IOError
  239. io2.IOError.NoSuchThing : IOError
  240. io2.IOError.PermissionDenied : IOError
  241. io2.IOError.ResourceBusy : IOError
  242. io2.IOError.ResourceExhausted : IOError
  243. io2.IOError.UserError : IOError
  244. builtin type io2.MVar
  245. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  246. io2.MVar.new : a ->{IO} MVar a
  247. io2.MVar.newEmpty : {IO} (MVar a)
  248. io2.MVar.put : MVar a -> a ->{IO} Either IOError ()
  249. io2.MVar.read : MVar a ->{IO} Either IOError a
  250. io2.MVar.swap : MVar a -> a ->{IO} Either IOError a
  251. io2.MVar.take : MVar a ->{IO} Either IOError a
  252. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  253. io2.MVar.tryRead : MVar a ->{IO} Optional a
  254. io2.MVar.tryTake : MVar a ->{IO} Optional a
  255. unique type io2.SeekMode
  256. io2.SeekMode.AbsoluteSeek : SeekMode
  257. io2.SeekMode.RelativeSeek : SeekMode
  258. io2.SeekMode.SeekFromEnd : SeekMode
  259. builtin type io2.Socket
  260. unique type io2.StdHandle
  261. io2.StdHandle.StdErr : StdHandle
  262. io2.StdHandle.StdIn : StdHandle
  263. io2.StdHandle.StdOut : StdHandle
  264. builtin type io2.ThreadId
  265. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  unique type Link
    2.  builtin type Link.Term
    3.  builtin type Link.Type
    4.  Link.Term         : Term -> Link
    5.  Link.Type         : Type -> Link
    6.  Int.signum        : Int -> Int
    7.  Int.toFloat       : Int -> Float
    8.  Int.toText        : Int -> Text
    9.  Int.trailingZeros : Int -> Nat
    10. Int.truncate0     : Int -> Nat
    11. Int.xor           : Int -> Int -> Int
  
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

  1.  Int.signum : Int -> Int
  2.  Int.toFloat : Int -> Float
  3.  Int.toText : Int -> Text
  4.  Int.trailingZeros : Int -> Nat
  5.  Int.truncate0 : Int -> Nat
  6.  Int.xor : Int -> Int -> Int
  7.  unique type Link
  8.  builtin type Link.Term
  9.  Link.Term : Term -> Link
  10. builtin type Link.Type
  11. Link.Type : Type -> Link
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
