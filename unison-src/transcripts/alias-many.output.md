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
  9.   Bytes.fromBase16 : Bytes -> Either Text Bytes
  10.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  11.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  12.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  13.  Bytes.fromList : [Nat] -> Bytes
  14.  Bytes.size : Bytes -> Nat
  15.  Bytes.take : Nat -> Bytes -> Bytes
  16.  Bytes.toBase16 : Bytes -> Bytes
  17.  Bytes.toBase32 : Bytes -> Bytes
  18.  Bytes.toBase64 : Bytes -> Bytes
  19.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  20.  Bytes.toList : Bytes -> [Nat]
  21.  builtin type Char
  22.  Char.fromNat : Nat -> Char
  23.  Char.toNat : Char -> Nat
  24.  Debug.watch : Text -> a -> a
  25.  unique type Doc
  26.  Doc.Blob : Text -> Doc
  27.  Doc.Evaluate : Term -> Doc
  28.  Doc.Join : [Doc] -> Doc
  29.  Doc.Link : Link -> Doc
  30.  Doc.Signature : Term -> Doc
  31.  Doc.Source : Link -> Doc
  32.  type Either a b
  33.  Either.Left : a -> Either a b
  34.  Either.Right : b -> Either a b
  35.  builtin type Float
  36.  Float.* : Float -> Float -> Float
  37.  Float.+ : Float -> Float -> Float
  38.  Float.- : Float -> Float -> Float
  39.  Float./ : Float -> Float -> Float
  40.  Float.abs : Float -> Float
  41.  Float.acos : Float -> Float
  42.  Float.acosh : Float -> Float
  43.  Float.asin : Float -> Float
  44.  Float.asinh : Float -> Float
  45.  Float.atan : Float -> Float
  46.  Float.atan2 : Float -> Float -> Float
  47.  Float.atanh : Float -> Float
  48.  Float.ceiling : Float -> Int
  49.  Float.cos : Float -> Float
  50.  Float.cosh : Float -> Float
  51.  Float.eq : Float -> Float -> Boolean
  52.  Float.exp : Float -> Float
  53.  Float.floor : Float -> Int
  54.  Float.fromText : Text -> Optional Float
  55.  Float.gt : Float -> Float -> Boolean
  56.  Float.gteq : Float -> Float -> Boolean
  57.  Float.log : Float -> Float
  58.  Float.logBase : Float -> Float -> Float
  59.  Float.lt : Float -> Float -> Boolean
  60.  Float.lteq : Float -> Float -> Boolean
  61.  Float.max : Float -> Float -> Float
  62.  Float.min : Float -> Float -> Float
  63.  Float.pow : Float -> Float -> Float
  64.  Float.round : Float -> Int
  65.  Float.sin : Float -> Float
  66.  Float.sinh : Float -> Float
  67.  Float.sqrt : Float -> Float
  68.  Float.tan : Float -> Float
  69.  Float.tanh : Float -> Float
  70.  Float.toText : Float -> Text
  71.  Float.truncate : Float -> Int
  72.  builtin type Int
  73.  Int.* : Int -> Int -> Int
  74.  Int.+ : Int -> Int -> Int
  75.  Int.- : Int -> Int -> Int
  76.  Int./ : Int -> Int -> Int
  77.  Int.and : Int -> Int -> Int
  78.  Int.complement : Int -> Int
  79.  Int.eq : Int -> Int -> Boolean
  80.  Int.fromText : Text -> Optional Int
  81.  Int.gt : Int -> Int -> Boolean
  82.  Int.gteq : Int -> Int -> Boolean
  83.  Int.increment : Int -> Int
  84.  Int.isEven : Int -> Boolean
  85.  Int.isOdd : Int -> Boolean
  86.  Int.leadingZeros : Int -> Nat
  87.  Int.lt : Int -> Int -> Boolean
  88.  Int.lteq : Int -> Int -> Boolean
  89.  Int.mod : Int -> Int -> Int
  90.  Int.negate : Int -> Int
  91.  Int.or : Int -> Int -> Int
  92.  Int.pow : Int -> Nat -> Int
  93.  Int.shiftLeft : Int -> Nat -> Int
  94.  Int.shiftRight : Int -> Nat -> Int
  95.  Int.signum : Int -> Int
  96.  Int.toFloat : Int -> Float
  97.  Int.toText : Int -> Text
  98.  Int.trailingZeros : Int -> Nat
  99.  Int.truncate0 : Int -> Nat
  100. Int.xor : Int -> Int -> Int
  101. unique type Link
  102. builtin type Link.Term
  103. Link.Term : Term -> Link
  104. builtin type Link.Type
  105. Link.Type : Type -> Link
  106. builtin type List
  107. List.++ : [a] -> [a] -> [a]
  108. List.+: : a -> [a] -> [a]
  109. List.:+ : [a] -> a -> [a]
  110. List.at : Nat -> [a] -> Optional a
  111. List.cons : a -> [a] -> [a]
  112. List.drop : Nat -> [a] -> [a]
  113. List.empty : [a]
  114. List.size : [a] -> Nat
  115. List.snoc : [a] -> a -> [a]
  116. List.take : Nat -> [a] -> [a]
  117. builtin type Nat
  118. Nat.* : Nat -> Nat -> Nat
  119. Nat.+ : Nat -> Nat -> Nat
  120. Nat./ : Nat -> Nat -> Nat
  121. Nat.and : Nat -> Nat -> Nat
  122. Nat.complement : Nat -> Nat
  123. Nat.drop : Nat -> Nat -> Nat
  124. Nat.eq : Nat -> Nat -> Boolean
  125. Nat.fromText : Text -> Optional Nat
  126. Nat.gt : Nat -> Nat -> Boolean
  127. Nat.gteq : Nat -> Nat -> Boolean
  128. Nat.increment : Nat -> Nat
  129. Nat.isEven : Nat -> Boolean
  130. Nat.isOdd : Nat -> Boolean
  131. Nat.leadingZeros : Nat -> Nat
  132. Nat.lt : Nat -> Nat -> Boolean
  133. Nat.lteq : Nat -> Nat -> Boolean
  134. Nat.mod : Nat -> Nat -> Nat
  135. Nat.or : Nat -> Nat -> Nat
  136. Nat.pow : Nat -> Nat -> Nat
  137. Nat.shiftLeft : Nat -> Nat -> Nat
  138. Nat.shiftRight : Nat -> Nat -> Nat
  139. Nat.sub : Nat -> Nat -> Int
  140. Nat.toFloat : Nat -> Float
  141. Nat.toInt : Nat -> Int
  142. Nat.toText : Nat -> Text
  143. Nat.trailingZeros : Nat -> Nat
  144. Nat.xor : Nat -> Nat -> Nat
  145. type Optional a
  146. Optional.None : Optional a
  147. Optional.Some : a -> Optional a
  148. builtin type Request
  149. type SeqView a b
  150. SeqView.VElem : a -> b -> SeqView a b
  151. SeqView.VEmpty : SeqView a b
  152. unique type Test.Result
  153. Test.Result.Fail : Text -> Result
  154. Test.Result.Ok : Text -> Result
  155. builtin type Text
  156. Text.!= : Text -> Text -> Boolean
  157. Text.++ : Text -> Text -> Text
  158. Text.drop : Nat -> Text -> Text
  159. Text.empty : Text
  160. Text.eq : Text -> Text -> Boolean
  161. Text.fromCharList : [Char] -> Text
  162. Text.gt : Text -> Text -> Boolean
  163. Text.gteq : Text -> Text -> Boolean
  164. Text.lt : Text -> Text -> Boolean
  165. Text.lteq : Text -> Text -> Boolean
  166. Text.size : Text -> Nat
  167. Text.take : Nat -> Text -> Text
  168. Text.toCharList : Text -> [Char]
  169. Text.uncons : Text -> Optional (Char, Text)
  170. Text.unsnoc : Text -> Optional (Text, Char)
  171. type Tuple a b
  172. Tuple.Cons : a -> b -> Tuple a b
  173. type Unit
  174. Unit.Unit : ()
  175. Universal.< : a -> a -> Boolean
  176. Universal.<= : a -> a -> Boolean
  177. Universal.== : a -> a -> Boolean
  178. Universal.> : a -> a -> Boolean
  179. Universal.>= : a -> a -> Boolean
  180. Universal.compare : a -> a -> Int
  181. bug : a -> b
  182. builtin type crypto.HashAlgorithm
  183. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  184. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  185. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  186. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  187. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  188. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  189. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  190. crypto.hash : HashAlgorithm -> a -> Bytes
  191. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  192. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  193. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  194. unique type io2.BufferMode
  195. io2.BufferMode.BlockBuffering : BufferMode
  196. io2.BufferMode.LineBuffering : BufferMode
  197. io2.BufferMode.NoBuffering : BufferMode
  198. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  199. unique type io2.FileMode
  200. io2.FileMode.Append : FileMode
  201. io2.FileMode.Read : FileMode
  202. io2.FileMode.ReadWrite : FileMode
  203. io2.FileMode.Write : FileMode
  204. builtin type io2.Handle
  205. builtin type io2.IO
  206. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  207. io2.IO.closeFile : Handle ->{IO} Either IOError ()
  208. io2.IO.closeSocket : Socket ->{IO} Either IOError ()
  209. io2.IO.createDirectory : Text ->{IO} Either IOError ()
  210. io2.IO.delay : Nat ->{IO} Either IOError ()
  211. io2.IO.fileExists : Text ->{IO} Either IOError Boolean
  212. io2.IO.forkComp : '{IO} Either IOError a ->{IO} ThreadId
  213. io2.IO.getBuffering : Handle
                             ->{IO} Either IOError BufferMode
  214. io2.IO.getCurrentDirectory : '{IO} Either IOError Text
  215. io2.IO.getFileSize : Text ->{IO} Either IOError Nat
  216. io2.IO.getFileTimestamp : Text ->{IO} Either IOError Nat
  217. io2.IO.getLine : Handle ->{IO} Either IOError Text
  218. io2.IO.getTempDirectory : '{IO} Either IOError Text
  219. io2.IO.getText : Handle ->{IO} Either IOError Text
  220. io2.IO.handlePosition : Handle ->{IO} Either IOError Int
  221. io2.IO.isDirectory : Text ->{IO} Either IOError Boolean
  222. io2.IO.isFileEOF : Handle ->{IO} Either IOError Boolean
  223. io2.IO.isFileOpen : Handle ->{IO} Either IOError Boolean
  224. io2.IO.isSeekable : Handle ->{IO} Either IOError Boolean
  225. io2.IO.kill : ThreadId ->{IO} Either IOError ()
  226. io2.IO.listen : Socket ->{IO} Either IOError ()
  227. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either IOError Handle
  228. io2.IO.putText : Handle -> Text ->{IO} Either IOError ()
  229. io2.IO.removeDirectory : Text ->{IO} Either IOError ()
  230. io2.IO.removeFile : Text ->{IO} Either IOError ()
  231. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either IOError ()
  232. io2.IO.renameFile : Text -> Text ->{IO} Either IOError ()
  233. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either IOError ()
  234. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  235. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either IOError ()
  236. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either IOError ()
  237. io2.IO.socketAccept : Socket ->{IO} Either IOError Socket
  238. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either IOError Bytes
  239. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either IOError ()
  240. io2.IO.stdHandle : StdHandle -> Handle
  241. io2.IO.systemTime : '{IO} Either IOError Nat
  242. unique type io2.IOError
  243. io2.IOError.AlreadyExists : IOError
  244. io2.IOError.EOF : IOError
  245. io2.IOError.IllegalOperation : IOError
  246. io2.IOError.NoSuchThing : IOError
  247. io2.IOError.PermissionDenied : IOError
  248. io2.IOError.ResourceBusy : IOError
  249. io2.IOError.ResourceExhausted : IOError
  250. io2.IOError.UserError : IOError
  251. builtin type io2.MVar
  252. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  253. io2.MVar.new : a ->{IO} MVar a
  254. io2.MVar.newEmpty : {IO} (MVar a)
  255. io2.MVar.put : MVar a -> a ->{IO} Either IOError ()
  256. io2.MVar.read : MVar a ->{IO} Either IOError a
  257. io2.MVar.swap : MVar a -> a ->{IO} Either IOError a
  258. io2.MVar.take : MVar a ->{IO} Either IOError a
  259. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  260. io2.MVar.tryRead : MVar a ->{IO} Optional a
  261. io2.MVar.tryTake : MVar a ->{IO} Optional a
  262. unique type io2.SeekMode
  263. io2.SeekMode.AbsoluteSeek : SeekMode
  264. io2.SeekMode.RelativeSeek : SeekMode
  265. io2.SeekMode.SeekFromEnd : SeekMode
  266. builtin type io2.Socket
  267. unique type io2.StdHandle
  268. io2.StdHandle.StdErr : StdHandle
  269. io2.StdHandle.StdIn : StdHandle
  270. io2.StdHandle.StdOut : StdHandle
  271. builtin type io2.ThreadId
  272. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  unique type Link
    2.  builtin type Link.Term
    3.  builtin type Link.Type
    4.  Link.Term         : Term -> Link
    5.  Int.shiftRight    : Int -> Nat -> Int
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

  1.  Int.shiftRight : Int -> Nat -> Int
  2.  Int.signum : Int -> Int
  3.  Int.toFloat : Int -> Float
  4.  Int.toText : Int -> Text
  5.  Int.trailingZeros : Int -> Nat
  6.  Int.truncate0 : Int -> Nat
  7.  Int.xor : Int -> Int -> Int
  8.  unique type Link
  9.  builtin type Link.Term
  10. Link.Term : Term -> Link
  11. builtin type Link.Type
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
