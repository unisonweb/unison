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

  1.   builtin type Any
  2.   Any.Any : a -> Any
  3.   builtin type Boolean
  4.   Boolean.not : Boolean -> Boolean
  5.   builtin type Bytes
  6.   Bytes.++ : Bytes -> Bytes -> Bytes
  7.   Bytes.at : Nat -> Bytes -> Optional Nat
  8.   Bytes.drop : Nat -> Bytes -> Bytes
  9.   Bytes.empty : Bytes
  10.  Bytes.flatten : Bytes -> Bytes
  11.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  12.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  13.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  14.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  15.  Bytes.fromList : [Nat] -> Bytes
  16.  Bytes.size : Bytes -> Nat
  17.  Bytes.take : Nat -> Bytes -> Bytes
  18.  Bytes.toBase16 : Bytes -> Bytes
  19.  Bytes.toBase32 : Bytes -> Bytes
  20.  Bytes.toBase64 : Bytes -> Bytes
  21.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  22.  Bytes.toList : Bytes -> [Nat]
  23.  builtin type Char
  24.  Char.fromNat : Nat -> Char
  25.  Char.toNat : Char -> Nat
  26.  builtin type Code
  27.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  28.  Code.dependencies : Code -> [Term]
  29.  Code.deserialize : Bytes -> Either Text Code
  30.  Code.isMissing : Term ->{IO} Boolean
  31.  Code.lookup : Term ->{IO} Optional Code
  32.  Code.serialize : Code -> Bytes
  33.  Debug.watch : Text -> a -> a
  34.  unique type Doc
  35.  Doc.Blob : Text -> Doc
  36.  Doc.Evaluate : Term -> Doc
  37.  Doc.Join : [Doc] -> Doc
  38.  Doc.Link : Link -> Doc
  39.  Doc.Signature : Term -> Doc
  40.  Doc.Source : Link -> Doc
  41.  type Either a b
  42.  Either.Left : a -> Either a b
  43.  Either.Right : b -> Either a b
  44.  builtin type Float
  45.  Float.* : Float -> Float -> Float
  46.  Float.+ : Float -> Float -> Float
  47.  Float.- : Float -> Float -> Float
  48.  Float./ : Float -> Float -> Float
  49.  Float.abs : Float -> Float
  50.  Float.acos : Float -> Float
  51.  Float.acosh : Float -> Float
  52.  Float.asin : Float -> Float
  53.  Float.asinh : Float -> Float
  54.  Float.atan : Float -> Float
  55.  Float.atan2 : Float -> Float -> Float
  56.  Float.atanh : Float -> Float
  57.  Float.ceiling : Float -> Int
  58.  Float.cos : Float -> Float
  59.  Float.cosh : Float -> Float
  60.  Float.eq : Float -> Float -> Boolean
  61.  Float.exp : Float -> Float
  62.  Float.floor : Float -> Int
  63.  Float.fromText : Text -> Optional Float
  64.  Float.gt : Float -> Float -> Boolean
  65.  Float.gteq : Float -> Float -> Boolean
  66.  Float.log : Float -> Float
  67.  Float.logBase : Float -> Float -> Float
  68.  Float.lt : Float -> Float -> Boolean
  69.  Float.lteq : Float -> Float -> Boolean
  70.  Float.max : Float -> Float -> Float
  71.  Float.min : Float -> Float -> Float
  72.  Float.pow : Float -> Float -> Float
  73.  Float.round : Float -> Int
  74.  Float.sin : Float -> Float
  75.  Float.sinh : Float -> Float
  76.  Float.sqrt : Float -> Float
  77.  Float.tan : Float -> Float
  78.  Float.tanh : Float -> Float
  79.  Float.toText : Float -> Text
  80.  Float.truncate : Float -> Int
  81.  builtin type Int
  82.  Int.* : Int -> Int -> Int
  83.  Int.+ : Int -> Int -> Int
  84.  Int.- : Int -> Int -> Int
  85.  Int./ : Int -> Int -> Int
  86.  Int.and : Int -> Int -> Int
  87.  Int.complement : Int -> Int
  88.  Int.eq : Int -> Int -> Boolean
  89.  Int.fromText : Text -> Optional Int
  90.  Int.gt : Int -> Int -> Boolean
  91.  Int.gteq : Int -> Int -> Boolean
  92.  Int.increment : Int -> Int
  93.  Int.isEven : Int -> Boolean
  94.  Int.isOdd : Int -> Boolean
  95.  Int.leadingZeros : Int -> Nat
  96.  Int.lt : Int -> Int -> Boolean
  97.  Int.lteq : Int -> Int -> Boolean
  98.  Int.mod : Int -> Int -> Int
  99.  Int.negate : Int -> Int
  100. Int.or : Int -> Int -> Int
  101. Int.popCount : Int -> Nat
  102. Int.pow : Int -> Nat -> Int
  103. Int.shiftLeft : Int -> Nat -> Int
  104. Int.shiftRight : Int -> Nat -> Int
  105. Int.signum : Int -> Int
  106. Int.toFloat : Int -> Float
  107. Int.toText : Int -> Text
  108. Int.trailingZeros : Int -> Nat
  109. Int.truncate0 : Int -> Nat
  110. Int.xor : Int -> Int -> Int
  111. unique type Link
  112. builtin type Link.Term
  113. Link.Term : Term -> Link
  114. builtin type Link.Type
  115. Link.Type : Type -> Link
  116. builtin type List
  117. List.++ : [a] -> [a] -> [a]
  118. List.+: : a -> [a] -> [a]
  119. List.:+ : [a] -> a -> [a]
  120. List.at : Nat -> [a] -> Optional a
  121. List.cons : a -> [a] -> [a]
  122. List.drop : Nat -> [a] -> [a]
  123. List.empty : [a]
  124. List.size : [a] -> Nat
  125. List.snoc : [a] -> a -> [a]
  126. List.take : Nat -> [a] -> [a]
  127. builtin type Nat
  128. Nat.* : Nat -> Nat -> Nat
  129. Nat.+ : Nat -> Nat -> Nat
  130. Nat./ : Nat -> Nat -> Nat
  131. Nat.and : Nat -> Nat -> Nat
  132. Nat.complement : Nat -> Nat
  133. Nat.drop : Nat -> Nat -> Nat
  134. Nat.eq : Nat -> Nat -> Boolean
  135. Nat.fromText : Text -> Optional Nat
  136. Nat.gt : Nat -> Nat -> Boolean
  137. Nat.gteq : Nat -> Nat -> Boolean
  138. Nat.increment : Nat -> Nat
  139. Nat.isEven : Nat -> Boolean
  140. Nat.isOdd : Nat -> Boolean
  141. Nat.leadingZeros : Nat -> Nat
  142. Nat.lt : Nat -> Nat -> Boolean
  143. Nat.lteq : Nat -> Nat -> Boolean
  144. Nat.mod : Nat -> Nat -> Nat
  145. Nat.or : Nat -> Nat -> Nat
  146. Nat.popCount : Nat -> Nat
  147. Nat.pow : Nat -> Nat -> Nat
  148. Nat.shiftLeft : Nat -> Nat -> Nat
  149. Nat.shiftRight : Nat -> Nat -> Nat
  150. Nat.sub : Nat -> Nat -> Int
  151. Nat.toFloat : Nat -> Float
  152. Nat.toInt : Nat -> Int
  153. Nat.toText : Nat -> Text
  154. Nat.trailingZeros : Nat -> Nat
  155. Nat.xor : Nat -> Nat -> Nat
  156. type Optional a
  157. Optional.None : Optional a
  158. Optional.Some : a -> Optional a
  159. builtin type Request
  160. type SeqView a b
  161. SeqView.VElem : a -> b -> SeqView a b
  162. SeqView.VEmpty : SeqView a b
  163. unique type Test.Result
  164. Test.Result.Fail : Text -> Result
  165. Test.Result.Ok : Text -> Result
  166. builtin type Text
  167. Text.!= : Text -> Text -> Boolean
  168. Text.++ : Text -> Text -> Text
  169. Text.drop : Nat -> Text -> Text
  170. Text.empty : Text
  171. Text.eq : Text -> Text -> Boolean
  172. Text.fromCharList : [Char] -> Text
  173. Text.fromUtf8 : Bytes -> Either Failure Text
  174. Text.gt : Text -> Text -> Boolean
  175. Text.gteq : Text -> Text -> Boolean
  176. Text.lt : Text -> Text -> Boolean
  177. Text.lteq : Text -> Text -> Boolean
  178. Text.size : Text -> Nat
  179. Text.take : Nat -> Text -> Text
  180. Text.toCharList : Text -> [Char]
  181. Text.toUtf8 : Text -> Bytes
  182. Text.uncons : Text -> Optional (Char, Text)
  183. Text.unsnoc : Text -> Optional (Text, Char)
  184. type Tuple a b
  185. Tuple.Cons : a -> b -> Tuple a b
  186. type Unit
  187. Unit.Unit : ()
  188. Universal.< : a -> a -> Boolean
  189. Universal.<= : a -> a -> Boolean
  190. Universal.== : a -> a -> Boolean
  191. Universal.> : a -> a -> Boolean
  192. Universal.>= : a -> a -> Boolean
  193. Universal.compare : a -> a -> Int
  194. builtin type Value
  195. Value.dependencies : Value -> [Term]
  196. Value.deserialize : Bytes -> Either Text Value
  197. Value.load : Value ->{IO} Either [Term] a
  198. Value.serialize : Value -> Bytes
  199. Value.value : a -> Value
  200. bug : a -> b
  201. builtin type crypto.HashAlgorithm
  202. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  203. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  204. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  205. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  206. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  207. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  208. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  209. crypto.hash : HashAlgorithm -> a -> Bytes
  210. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  211. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  212. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  213. unique type io2.BufferMode
  214. io2.BufferMode.BlockBuffering : BufferMode
  215. io2.BufferMode.LineBuffering : BufferMode
  216. io2.BufferMode.NoBuffering : BufferMode
  217. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  218. unique type io2.Failure
  219. io2.Failure.Failure : Type -> Text -> Failure
  220. unique type io2.FileMode
  221. io2.FileMode.Append : FileMode
  222. io2.FileMode.Read : FileMode
  223. io2.FileMode.ReadWrite : FileMode
  224. io2.FileMode.Write : FileMode
  225. builtin type io2.Handle
  226. builtin type io2.IO
  227. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  228. io2.IO.closeFile : Handle ->{IO} Either Failure ()
  229. io2.IO.closeSocket : Socket ->{IO} Either Failure ()
  230. io2.IO.createDirectory : Text ->{IO} Either Failure ()
  231. io2.IO.createTempDirectory : Text
                                    ->{IO} Either Failure Text
  232. io2.IO.delay : Nat ->{IO} Either Failure ()
  233. io2.IO.fileExists : Text ->{IO} Either Failure Boolean
  234. io2.IO.forkComp : '{IO} Either Failure a ->{IO} ThreadId
  235. io2.IO.getBuffering : Handle
                             ->{IO} Either Failure BufferMode
  236. io2.IO.getBytes : Handle
                         -> Nat
                         ->{IO} Either Failure Bytes
  237. io2.IO.getCurrentDirectory : '{IO} Either Failure Text
  238. io2.IO.getFileSize : Text ->{IO} Either Failure Nat
  239. io2.IO.getFileTimestamp : Text ->{IO} Either Failure Nat
  240. io2.IO.getTempDirectory : '{IO} Either Failure Text
  241. io2.IO.handlePosition : Handle ->{IO} Either Failure Int
  242. io2.IO.isDirectory : Text ->{IO} Either Failure Boolean
  243. io2.IO.isFileEOF : Handle ->{IO} Either Failure Boolean
  244. io2.IO.isFileOpen : Handle ->{IO} Either Failure Boolean
  245. io2.IO.isSeekable : Handle ->{IO} Either Failure Boolean
  246. io2.IO.kill : ThreadId ->{IO} Either Failure ()
  247. io2.IO.listen : Socket ->{IO} Either Failure ()
  248. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either Failure Handle
  249. io2.IO.putBytes : Handle
                         -> Bytes
                         ->{IO} Either Failure ()
  250. io2.IO.removeDirectory : Text ->{IO} Either Failure ()
  251. io2.IO.removeFile : Text ->{IO} Either Failure ()
  252. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either Failure ()
  253. io2.IO.renameFile : Text -> Text ->{IO} Either Failure ()
  254. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either Failure ()
  255. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  256. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either Failure ()
  257. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either Failure ()
  258. io2.IO.socketAccept : Socket ->{IO} Either Failure Socket
  259. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either Failure Bytes
  260. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either Failure ()
  261. io2.IO.stdHandle : StdHandle -> Handle
  262. io2.IO.systemTime : '{IO} Either Failure Nat
  263. unique type io2.IOError
  264. io2.IOError.AlreadyExists : IOError
  265. io2.IOError.EOF : IOError
  266. io2.IOError.IllegalOperation : IOError
  267. io2.IOError.NoSuchThing : IOError
  268. io2.IOError.PermissionDenied : IOError
  269. io2.IOError.ResourceBusy : IOError
  270. io2.IOError.ResourceExhausted : IOError
  271. io2.IOError.UserError : IOError
  272. builtin type io2.MVar
  273. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  274. io2.MVar.new : a ->{IO} MVar a
  275. io2.MVar.newEmpty : '{IO} MVar a
  276. io2.MVar.put : MVar a -> a ->{IO} Either Failure ()
  277. io2.MVar.read : MVar a ->{IO} Either Failure a
  278. io2.MVar.swap : MVar a -> a ->{IO} Either Failure a
  279. io2.MVar.take : MVar a ->{IO} Either Failure a
  280. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  281. io2.MVar.tryRead : MVar a ->{IO} Optional a
  282. io2.MVar.tryTake : MVar a ->{IO} Optional a
  283. unique type io2.SeekMode
  284. io2.SeekMode.AbsoluteSeek : SeekMode
  285. io2.SeekMode.RelativeSeek : SeekMode
  286. io2.SeekMode.SeekFromEnd : SeekMode
  287. builtin type io2.Socket
  288. unique type io2.StdHandle
  289. io2.StdHandle.StdErr : StdHandle
  290. io2.StdHandle.StdIn : StdHandle
  291. io2.StdHandle.StdOut : StdHandle
  292. builtin type io2.ThreadId
  293. builtin type io2.Tls
  294. builtin type io2.Tls.ClientConfig
  295. io2.Tls.Config.defaultClient : Text
                                      -> Bytes
                                      -> ClientConfig
  296. io2.Tls.Config.defaultServer : ServerConfig
  297. builtin type io2.Tls.ServerConfig
  298. io2.Tls.handshake : Tls ->{IO} Either Failure ()
  299. io2.Tls.newClient : ClientConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  300. io2.Tls.newServer : ServerConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  301. io2.Tls.receive : Tls ->{IO} Either Failure Bytes
  302. io2.Tls.send : Tls -> Bytes ->{IO} Either Failure ()
  303. io2.Tls.terminate : Tls ->{IO} Either Failure ()
  304. unique type io2.TlsFailure
  305. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.isOdd        : Int -> Boolean
    2.  Int.leadingZeros : Int -> Nat
    3.  Int.lt           : Int -> Int -> Boolean
    4.  Int.lteq         : Int -> Int -> Boolean
    5.  Int.mod          : Int -> Int -> Int
    6.  Int.negate       : Int -> Int
    7.  Int.or           : Int -> Int -> Int
    8.  Int.popCount     : Int -> Nat
    9.  Int.pow          : Int -> Nat -> Int
    10. Int.shiftLeft    : Int -> Nat -> Int
    11. Int.shiftRight   : Int -> Nat -> Int
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar

.runar> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  3.  List.any : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{g} Boolean) ->{g} [a] ->{g} [a]
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
    2.  List.all           : (a ->{g} Boolean)
                           ->{g} [a]
                           ->{g} Boolean
    3.  List.any           : (a ->{g} Boolean)
                           ->{g} [a]
                           ->{g} Boolean
    4.  List.chunk         : Nat -> [a] -> [[a]]
    5.  List.chunksOf      : Nat -> [a] -> [[a]]
    6.  List.dropWhile     : (a ->{g} Boolean)
                           ->{g} [a]
                           ->{g} [a]
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

  1.  Int.isOdd : Int -> Boolean
  2.  Int.leadingZeros : Int -> Nat
  3.  Int.lt : Int -> Int -> Boolean
  4.  Int.lteq : Int -> Int -> Boolean
  5.  Int.mod : Int -> Int -> Int
  6.  Int.negate : Int -> Int
  7.  Int.or : Int -> Int -> Int
  8.  Int.popCount : Int -> Nat
  9.  Int.pow : Int -> Nat -> Int
  10. Int.shiftLeft : Int -> Nat -> Int
  11. Int.shiftRight : Int -> Nat -> Int
  12. List.adjacentPairs : [a] -> [(a, a)]
  13. List.all : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  14. List.any : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  15. List.chunk : Nat -> [a] -> [[a]]
  16. List.chunksOf : Nat -> [a] -> [[a]]
  17. List.dropWhile : (a ->{g} Boolean) ->{g} [a] ->{g} [a]
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
