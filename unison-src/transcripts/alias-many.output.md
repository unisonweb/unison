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
  26.  Char.toText : Char -> Text
  27.  builtin type Code
  28.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  29.  Code.dependencies : Code -> [Term]
  30.  Code.deserialize : Bytes -> Either Text Code
  31.  Code.isMissing : Term ->{IO} Boolean
  32.  Code.lookup : Term ->{IO} Optional Code
  33.  Code.serialize : Code -> Bytes
  34.  Debug.watch : Text -> a -> a
  35.  unique type Doc
  36.  Doc.Blob : Text -> Doc
  37.  Doc.Evaluate : Term -> Doc
  38.  Doc.Join : [Doc] -> Doc
  39.  Doc.Link : Link -> Doc
  40.  Doc.Signature : Term -> Doc
  41.  Doc.Source : Link -> Doc
  42.  type Either a b
  43.  Either.Left : a -> Either a b
  44.  Either.Right : b -> Either a b
  45.  builtin type Float
  46.  Float.* : Float -> Float -> Float
  47.  Float.+ : Float -> Float -> Float
  48.  Float.- : Float -> Float -> Float
  49.  Float./ : Float -> Float -> Float
  50.  Float.abs : Float -> Float
  51.  Float.acos : Float -> Float
  52.  Float.acosh : Float -> Float
  53.  Float.asin : Float -> Float
  54.  Float.asinh : Float -> Float
  55.  Float.atan : Float -> Float
  56.  Float.atan2 : Float -> Float -> Float
  57.  Float.atanh : Float -> Float
  58.  Float.ceiling : Float -> Int
  59.  Float.cos : Float -> Float
  60.  Float.cosh : Float -> Float
  61.  Float.eq : Float -> Float -> Boolean
  62.  Float.exp : Float -> Float
  63.  Float.floor : Float -> Int
  64.  Float.fromText : Text -> Optional Float
  65.  Float.gt : Float -> Float -> Boolean
  66.  Float.gteq : Float -> Float -> Boolean
  67.  Float.log : Float -> Float
  68.  Float.logBase : Float -> Float -> Float
  69.  Float.lt : Float -> Float -> Boolean
  70.  Float.lteq : Float -> Float -> Boolean
  71.  Float.max : Float -> Float -> Float
  72.  Float.min : Float -> Float -> Float
  73.  Float.pow : Float -> Float -> Float
  74.  Float.round : Float -> Int
  75.  Float.sin : Float -> Float
  76.  Float.sinh : Float -> Float
  77.  Float.sqrt : Float -> Float
  78.  Float.tan : Float -> Float
  79.  Float.tanh : Float -> Float
  80.  Float.toText : Float -> Text
  81.  Float.truncate : Float -> Int
  82.  builtin type Int
  83.  Int.* : Int -> Int -> Int
  84.  Int.+ : Int -> Int -> Int
  85.  Int.- : Int -> Int -> Int
  86.  Int./ : Int -> Int -> Int
  87.  Int.and : Int -> Int -> Int
  88.  Int.complement : Int -> Int
  89.  Int.eq : Int -> Int -> Boolean
  90.  Int.fromText : Text -> Optional Int
  91.  Int.gt : Int -> Int -> Boolean
  92.  Int.gteq : Int -> Int -> Boolean
  93.  Int.increment : Int -> Int
  94.  Int.isEven : Int -> Boolean
  95.  Int.isOdd : Int -> Boolean
  96.  Int.leadingZeros : Int -> Nat
  97.  Int.lt : Int -> Int -> Boolean
  98.  Int.lteq : Int -> Int -> Boolean
  99.  Int.mod : Int -> Int -> Int
  100. Int.negate : Int -> Int
  101. Int.or : Int -> Int -> Int
  102. Int.popCount : Int -> Nat
  103. Int.pow : Int -> Nat -> Int
  104. Int.shiftLeft : Int -> Nat -> Int
  105. Int.shiftRight : Int -> Nat -> Int
  106. Int.signum : Int -> Int
  107. Int.toFloat : Int -> Float
  108. Int.toText : Int -> Text
  109. Int.trailingZeros : Int -> Nat
  110. Int.truncate0 : Int -> Nat
  111. Int.xor : Int -> Int -> Int
  112. unique type IsPropagated
  113. IsPropagated.IsPropagated : IsPropagated
  114. unique type IsTest
  115. IsTest.IsTest : IsTest
  116. unique type Link
  117. builtin type Link.Term
  118. Link.Term : Term -> Link
  119. builtin type Link.Type
  120. Link.Type : Type -> Link
  121. builtin type List
  122. List.++ : [a] -> [a] -> [a]
  123. List.+: : a -> [a] -> [a]
  124. List.:+ : [a] -> a -> [a]
  125. List.at : Nat -> [a] -> Optional a
  126. List.cons : a -> [a] -> [a]
  127. List.drop : Nat -> [a] -> [a]
  128. List.empty : [a]
  129. List.size : [a] -> Nat
  130. List.snoc : [a] -> a -> [a]
  131. List.take : Nat -> [a] -> [a]
  132. builtin type Nat
  133. Nat.* : Nat -> Nat -> Nat
  134. Nat.+ : Nat -> Nat -> Nat
  135. Nat./ : Nat -> Nat -> Nat
  136. Nat.and : Nat -> Nat -> Nat
  137. Nat.complement : Nat -> Nat
  138. Nat.drop : Nat -> Nat -> Nat
  139. Nat.eq : Nat -> Nat -> Boolean
  140. Nat.fromText : Text -> Optional Nat
  141. Nat.gt : Nat -> Nat -> Boolean
  142. Nat.gteq : Nat -> Nat -> Boolean
  143. Nat.increment : Nat -> Nat
  144. Nat.isEven : Nat -> Boolean
  145. Nat.isOdd : Nat -> Boolean
  146. Nat.leadingZeros : Nat -> Nat
  147. Nat.lt : Nat -> Nat -> Boolean
  148. Nat.lteq : Nat -> Nat -> Boolean
  149. Nat.mod : Nat -> Nat -> Nat
  150. Nat.or : Nat -> Nat -> Nat
  151. Nat.popCount : Nat -> Nat
  152. Nat.pow : Nat -> Nat -> Nat
  153. Nat.shiftLeft : Nat -> Nat -> Nat
  154. Nat.shiftRight : Nat -> Nat -> Nat
  155. Nat.sub : Nat -> Nat -> Int
  156. Nat.toFloat : Nat -> Float
  157. Nat.toInt : Nat -> Int
  158. Nat.toText : Nat -> Text
  159. Nat.trailingZeros : Nat -> Nat
  160. Nat.xor : Nat -> Nat -> Nat
  161. type Optional a
  162. Optional.None : Optional a
  163. Optional.Some : a -> Optional a
  164. builtin type Request
  165. type SeqView a b
  166. SeqView.VElem : a -> b -> SeqView a b
  167. SeqView.VEmpty : SeqView a b
  168. unique type Test.Result
  169. Test.Result.Fail : Text -> Result
  170. Test.Result.Ok : Text -> Result
  171. builtin type Text
  172. Text.!= : Text -> Text -> Boolean
  173. Text.++ : Text -> Text -> Text
  174. Text.drop : Nat -> Text -> Text
  175. Text.empty : Text
  176. Text.eq : Text -> Text -> Boolean
  177. Text.fromCharList : [Char] -> Text
  178. Text.fromUtf8.impl : Bytes -> Either Failure Text
  179. Text.gt : Text -> Text -> Boolean
  180. Text.gteq : Text -> Text -> Boolean
  181. Text.lt : Text -> Text -> Boolean
  182. Text.lteq : Text -> Text -> Boolean
  183. Text.repeat : Nat -> Text -> Text
  184. Text.size : Text -> Nat
  185. Text.take : Nat -> Text -> Text
  186. Text.toCharList : Text -> [Char]
  187. Text.toUtf8 : Text -> Bytes
  188. Text.uncons : Text -> Optional (Char, Text)
  189. Text.unsnoc : Text -> Optional (Text, Char)
  190. type Tuple a b
  191. Tuple.Cons : a -> b -> Tuple a b
  192. type Unit
  193. Unit.Unit : ()
  194. Universal.< : a -> a -> Boolean
  195. Universal.<= : a -> a -> Boolean
  196. Universal.== : a -> a -> Boolean
  197. Universal.> : a -> a -> Boolean
  198. Universal.>= : a -> a -> Boolean
  199. Universal.compare : a -> a -> Int
  200. builtin type Value
  201. Value.dependencies : Value -> [Term]
  202. Value.deserialize : Bytes -> Either Text Value
  203. Value.load : Value ->{IO} Either [Term] a
  204. Value.serialize : Value -> Bytes
  205. Value.value : a -> Value
  206. bug : a -> b
  207. builtin type crypto.HashAlgorithm
  208. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  209. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  210. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  211. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  212. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  213. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  214. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  215. crypto.hash : HashAlgorithm -> a -> Bytes
  216. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  217. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  218. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  219. unique type io2.BufferMode
  220. io2.BufferMode.BlockBuffering : BufferMode
  221. io2.BufferMode.LineBuffering : BufferMode
  222. io2.BufferMode.NoBuffering : BufferMode
  223. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  224. unique type io2.Failure
  225. io2.Failure.Failure : Type -> Text -> Any -> Failure
  226. unique type io2.FileMode
  227. io2.FileMode.Append : FileMode
  228. io2.FileMode.Read : FileMode
  229. io2.FileMode.ReadWrite : FileMode
  230. io2.FileMode.Write : FileMode
  231. builtin type io2.Handle
  232. builtin type io2.IO
  233. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  234. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  235. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  236. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  237. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  238. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  239. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  240. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  241. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  242. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  243. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  244. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  245. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  246. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  247. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  248. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  249. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  250. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  251. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  252. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  253. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  254. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  255. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  256. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  257. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  258. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  259. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  260. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  261. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  262. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  263. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  264. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  265. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  266. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  267. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  268. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  269. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  270. io2.IO.stdHandle : StdHandle -> Handle
  271. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  272. unique type io2.IOError
  273. io2.IOError.AlreadyExists : IOError
  274. io2.IOError.EOF : IOError
  275. io2.IOError.IllegalOperation : IOError
  276. io2.IOError.NoSuchThing : IOError
  277. io2.IOError.PermissionDenied : IOError
  278. io2.IOError.ResourceBusy : IOError
  279. io2.IOError.ResourceExhausted : IOError
  280. io2.IOError.UserError : IOError
  281. unique type io2.IOFailure
  282. builtin type io2.MVar
  283. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  284. io2.MVar.new : a ->{IO} MVar a
  285. io2.MVar.newEmpty : '{IO} MVar a
  286. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  287. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  288. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  289. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  290. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  291. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  292. io2.MVar.tryTake : MVar a ->{IO} Optional a
  293. builtin type io2.STM
  294. io2.STM.atomically : '{STM} a ->{IO} a
  295. io2.STM.retry : '{STM} a
  296. unique type io2.SeekMode
  297. io2.SeekMode.AbsoluteSeek : SeekMode
  298. io2.SeekMode.RelativeSeek : SeekMode
  299. io2.SeekMode.SeekFromEnd : SeekMode
  300. builtin type io2.Socket
  301. unique type io2.StdHandle
  302. io2.StdHandle.StdErr : StdHandle
  303. io2.StdHandle.StdIn : StdHandle
  304. io2.StdHandle.StdOut : StdHandle
  305. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  306. builtin type io2.TVar
  307. io2.TVar.new : a ->{STM} TVar a
  308. io2.TVar.newIO : a ->{IO} TVar a
  309. io2.TVar.read : TVar a ->{STM} a
  310. io2.TVar.readIO : TVar a ->{IO} a
  311. io2.TVar.swap : TVar a -> a ->{STM} a
  312. io2.TVar.write : TVar a -> a ->{STM} ()
  313. builtin type io2.ThreadId
  314. builtin type io2.Tls
  315. builtin type io2.Tls.Cipher
  316. builtin type io2.Tls.ClientConfig
  317. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  318. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  319. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  320. builtin type io2.Tls.PrivateKey
  321. builtin type io2.Tls.ServerConfig
  322. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  323. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  324. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  325. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  326. builtin type io2.Tls.SignedCert
  327. builtin type io2.Tls.Version
  328. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  329. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  330. io2.Tls.encodeCert : SignedCert -> Bytes
  331. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  332. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  333. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  334. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  335. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  336. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  337. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  338. unique type io2.TlsFailure
  339. metadata.isPropagated : IsPropagated
  340. metadata.isTest : IsTest
  341. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.isEven       : Int -> Boolean
    2.  Int.isOdd        : Int -> Boolean
    3.  Int.leadingZeros : Int -> Nat
    4.  Int.lt           : Int -> Int -> Boolean
    5.  Int.lteq         : Int -> Int -> Boolean
    6.  Int.mod          : Int -> Int -> Int
    7.  Int.negate       : Int -> Int
    8.  Int.or           : Int -> Int -> Int
    9.  Int.popCount     : Int -> Nat
    10. Int.pow          : Int -> Nat -> Int
    11. Int.shiftLeft    : Int -> Nat -> Int
  
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

  1.  Int.isEven : Int -> Boolean
  2.  Int.isOdd : Int -> Boolean
  3.  Int.leadingZeros : Int -> Nat
  4.  Int.lt : Int -> Int -> Boolean
  5.  Int.lteq : Int -> Int -> Boolean
  6.  Int.mod : Int -> Int -> Int
  7.  Int.negate : Int -> Int
  8.  Int.or : Int -> Int -> Int
  9.  Int.popCount : Int -> Nat
  10. Int.pow : Int -> Nat -> Int
  11. Int.shiftLeft : Int -> Nat -> Int
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
