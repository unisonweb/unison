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
  92.  Int.popCount : Int -> Nat
  93.  Int.pow : Int -> Nat -> Int
  94.  Int.shiftLeft : Int -> Nat -> Int
  95.  Int.shiftRight : Int -> Nat -> Int
  96.  Int.signum : Int -> Int
  97.  Int.toFloat : Int -> Float
  98.  Int.toText : Int -> Text
  99.  Int.trailingZeros : Int -> Nat
  100. Int.truncate0 : Int -> Nat
  101. Int.xor : Int -> Int -> Int
  102. unique type Link
  103. builtin type Link.Term
  104. Link.Term : Term -> Link
  105. builtin type Link.Type
  106. Link.Type : Type -> Link
  107. builtin type List
  108. List.++ : [a] -> [a] -> [a]
  109. List.+: : a -> [a] -> [a]
  110. List.:+ : [a] -> a -> [a]
  111. List.at : Nat -> [a] -> Optional a
  112. List.cons : a -> [a] -> [a]
  113. List.drop : Nat -> [a] -> [a]
  114. List.empty : [a]
  115. List.size : [a] -> Nat
  116. List.snoc : [a] -> a -> [a]
  117. List.take : Nat -> [a] -> [a]
  118. builtin type Nat
  119. Nat.* : Nat -> Nat -> Nat
  120. Nat.+ : Nat -> Nat -> Nat
  121. Nat./ : Nat -> Nat -> Nat
  122. Nat.and : Nat -> Nat -> Nat
  123. Nat.complement : Nat -> Nat
  124. Nat.drop : Nat -> Nat -> Nat
  125. Nat.eq : Nat -> Nat -> Boolean
  126. Nat.fromText : Text -> Optional Nat
  127. Nat.gt : Nat -> Nat -> Boolean
  128. Nat.gteq : Nat -> Nat -> Boolean
  129. Nat.increment : Nat -> Nat
  130. Nat.isEven : Nat -> Boolean
  131. Nat.isOdd : Nat -> Boolean
  132. Nat.leadingZeros : Nat -> Nat
  133. Nat.lt : Nat -> Nat -> Boolean
  134. Nat.lteq : Nat -> Nat -> Boolean
  135. Nat.mod : Nat -> Nat -> Nat
  136. Nat.or : Nat -> Nat -> Nat
  137. Nat.popCount : Nat -> Nat
  138. Nat.pow : Nat -> Nat -> Nat
  139. Nat.shiftLeft : Nat -> Nat -> Nat
  140. Nat.shiftRight : Nat -> Nat -> Nat
  141. Nat.sub : Nat -> Nat -> Int
  142. Nat.toFloat : Nat -> Float
  143. Nat.toInt : Nat -> Int
  144. Nat.toText : Nat -> Text
  145. Nat.trailingZeros : Nat -> Nat
  146. Nat.xor : Nat -> Nat -> Nat
  147. type Optional a
  148. Optional.None : Optional a
  149. Optional.Some : a -> Optional a
  150. builtin type Request
  151. type SeqView a b
  152. SeqView.VElem : a -> b -> SeqView a b
  153. SeqView.VEmpty : SeqView a b
  154. unique type Test.Result
  155. Test.Result.Fail : Text -> Result
  156. Test.Result.Ok : Text -> Result
  157. builtin type Text
  158. Text.!= : Text -> Text -> Boolean
  159. Text.++ : Text -> Text -> Text
  160. Text.drop : Nat -> Text -> Text
  161. Text.empty : Text
  162. Text.eq : Text -> Text -> Boolean
  163. Text.fromCharList : [Char] -> Text
  164. Text.fromUtf8 : Bytes -> Either Failure Text
  165. Text.gt : Text -> Text -> Boolean
  166. Text.gteq : Text -> Text -> Boolean
  167. Text.lt : Text -> Text -> Boolean
  168. Text.lteq : Text -> Text -> Boolean
  169. Text.size : Text -> Nat
  170. Text.take : Nat -> Text -> Text
  171. Text.toCharList : Text -> [Char]
  172. Text.toUtf8 : Text -> Bytes
  173. Text.uncons : Text -> Optional (Char, Text)
  174. Text.unsnoc : Text -> Optional (Text, Char)
  175. type Tuple a b
  176. Tuple.Cons : a -> b -> Tuple a b
  177. type Unit
  178. Unit.Unit : ()
  179. Universal.< : a -> a -> Boolean
  180. Universal.<= : a -> a -> Boolean
  181. Universal.== : a -> a -> Boolean
  182. Universal.> : a -> a -> Boolean
  183. Universal.>= : a -> a -> Boolean
  184. Universal.compare : a -> a -> Int
  185. bug : a -> b
  186. builtin type crypto.HashAlgorithm
  187. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  188. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  189. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  190. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  191. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  192. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  193. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  194. crypto.hash : HashAlgorithm -> a -> Bytes
  195. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  196. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  197. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  198. unique type io2.BufferMode
  199. io2.BufferMode.BlockBuffering : BufferMode
  200. io2.BufferMode.LineBuffering : BufferMode
  201. io2.BufferMode.NoBuffering : BufferMode
  202. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  203. unique type io2.Failure
  204. io2.Failure.Failure : Type -> Text -> Failure
  205. unique type io2.FileMode
  206. io2.FileMode.Append : FileMode
  207. io2.FileMode.Read : FileMode
  208. io2.FileMode.ReadWrite : FileMode
  209. io2.FileMode.Write : FileMode
  210. builtin type io2.Handle
  211. builtin type io2.IO
  212. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  213. io2.IO.closeFile : Handle ->{IO} Either Failure ()
  214. io2.IO.closeSocket : Socket ->{IO} Either Failure ()
  215. io2.IO.createDirectory : Text ->{IO} Either Failure ()
  216. io2.IO.createTempDirectory : Text
                                    ->{IO} Either Failure Text
  217. io2.IO.delay : Nat ->{IO} Either Failure ()
  218. io2.IO.fileExists : Text ->{IO} Either Failure Boolean
  219. io2.IO.forkComp : '{IO} Either Failure a ->{IO} ThreadId
  220. io2.IO.getBuffering : Handle
                             ->{IO} Either Failure BufferMode
  221. io2.IO.getBytes : Handle
                         -> Nat
                         ->{IO} Either Failure Bytes
  222. io2.IO.getCurrentDirectory : '{IO} Either Failure Text
  223. io2.IO.getFileSize : Text ->{IO} Either Failure Nat
  224. io2.IO.getFileTimestamp : Text ->{IO} Either Failure Nat
  225. io2.IO.getTempDirectory : '{IO} Either Failure Text
  226. io2.IO.handlePosition : Handle ->{IO} Either Failure Int
  227. io2.IO.isDirectory : Text ->{IO} Either Failure Boolean
  228. io2.IO.isFileEOF : Handle ->{IO} Either Failure Boolean
  229. io2.IO.isFileOpen : Handle ->{IO} Either Failure Boolean
  230. io2.IO.isSeekable : Handle ->{IO} Either Failure Boolean
  231. io2.IO.kill : ThreadId ->{IO} Either Failure ()
  232. io2.IO.listen : Socket ->{IO} Either Failure ()
  233. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either Failure Handle
  234. io2.IO.putBytes : Handle
                         -> Bytes
                         ->{IO} Either Failure ()
  235. io2.IO.removeDirectory : Text ->{IO} Either Failure ()
  236. io2.IO.removeFile : Text ->{IO} Either Failure ()
  237. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either Failure ()
  238. io2.IO.renameFile : Text -> Text ->{IO} Either Failure ()
  239. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either Failure ()
  240. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  241. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either Failure ()
  242. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either Failure ()
  243. io2.IO.socketAccept : Socket ->{IO} Either Failure Socket
  244. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either Failure Bytes
  245. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either Failure ()
  246. io2.IO.stdHandle : StdHandle -> Handle
  247. io2.IO.systemTime : '{IO} Either Failure Nat
  248. unique type io2.IOError
  249. io2.IOError.AlreadyExists : IOError
  250. io2.IOError.EOF : IOError
  251. io2.IOError.IllegalOperation : IOError
  252. io2.IOError.NoSuchThing : IOError
  253. io2.IOError.PermissionDenied : IOError
  254. io2.IOError.ResourceBusy : IOError
  255. io2.IOError.ResourceExhausted : IOError
  256. io2.IOError.UserError : IOError
  257. builtin type io2.MVar
  258. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  259. io2.MVar.new : a ->{IO} MVar a
  260. io2.MVar.newEmpty : '{IO} MVar a
  261. io2.MVar.put : MVar a -> a ->{IO} Either Failure ()
  262. io2.MVar.read : MVar a ->{IO} Either Failure a
  263. io2.MVar.swap : MVar a -> a ->{IO} Either Failure a
  264. io2.MVar.take : MVar a ->{IO} Either Failure a
  265. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  266. io2.MVar.tryRead : MVar a ->{IO} Optional a
  267. io2.MVar.tryTake : MVar a ->{IO} Optional a
  268. unique type io2.SeekMode
  269. io2.SeekMode.AbsoluteSeek : SeekMode
  270. io2.SeekMode.RelativeSeek : SeekMode
  271. io2.SeekMode.SeekFromEnd : SeekMode
  272. builtin type io2.Socket
  273. unique type io2.StdHandle
  274. io2.StdHandle.StdErr : StdHandle
  275. io2.StdHandle.StdIn : StdHandle
  276. io2.StdHandle.StdOut : StdHandle
  277. builtin type io2.ThreadId
  278. builtin type io2.Tls
  279. builtin type io2.Tls.ClientConfig
  280. io2.Tls.Config.defaultClient : Text
                                      -> Bytes
                                      -> ClientConfig
  281. io2.Tls.Config.defaultServer : ServerConfig
  282. builtin type io2.Tls.ServerConfig
  283. io2.Tls.handshake : Tls ->{IO} Either Failure ()
  284. io2.Tls.newClient : ClientConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  285. io2.Tls.newServer : ServerConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  286. io2.Tls.receive : Tls ->{IO} Either Failure Bytes
  287. io2.Tls.send : Tls -> Bytes ->{IO} Either Failure ()
  288. io2.Tls.terminate : Tls ->{IO} Either Failure ()
  289. unique type io2.TlsFailure
  290. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  unique type Link
    2.  builtin type Link.Term
    3.  Link.Term         : Term -> Link
    4.  Int.shiftLeft     : Int -> Nat -> Int
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

  1.  Int.shiftLeft : Int -> Nat -> Int
  2.  Int.shiftRight : Int -> Nat -> Int
  3.  Int.signum : Int -> Int
  4.  Int.toFloat : Int -> Float
  5.  Int.toText : Int -> Text
  6.  Int.trailingZeros : Int -> Nat
  7.  Int.truncate0 : Int -> Nat
  8.  Int.xor : Int -> Int -> Int
  9.  unique type Link
  10. builtin type Link.Term
  11. Link.Term : Term -> Link
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
