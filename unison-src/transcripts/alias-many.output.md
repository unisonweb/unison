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
  5.   bug : a -> b
  6.   builtin type Bytes
  7.   Bytes.++ : Bytes -> Bytes -> Bytes
  8.   Bytes.at : Nat -> Bytes -> Optional Nat
  9.   Bytes.decodeNat16be : Bytes -> Optional (Nat, Bytes)
  10.  Bytes.decodeNat16le : Bytes -> Optional (Nat, Bytes)
  11.  Bytes.decodeNat32be : Bytes -> Optional (Nat, Bytes)
  12.  Bytes.decodeNat32le : Bytes -> Optional (Nat, Bytes)
  13.  Bytes.decodeNat64be : Bytes -> Optional (Nat, Bytes)
  14.  Bytes.decodeNat64le : Bytes -> Optional (Nat, Bytes)
  15.  Bytes.drop : Nat -> Bytes -> Bytes
  16.  Bytes.empty : Bytes
  17.  Bytes.encodeNat16be : Nat -> Bytes
  18.  Bytes.encodeNat16le : Nat -> Bytes
  19.  Bytes.encodeNat32be : Nat -> Bytes
  20.  Bytes.encodeNat32le : Nat -> Bytes
  21.  Bytes.encodeNat64be : Nat -> Bytes
  22.  Bytes.encodeNat64le : Nat -> Bytes
  23.  Bytes.flatten : Bytes -> Bytes
  24.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  25.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  26.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  27.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  28.  Bytes.fromList : [Nat] -> Bytes
  29.  Bytes.gzip.compress : Bytes -> Bytes
  30.  Bytes.gzip.decompress : Bytes -> Either Text Bytes
  31.  Bytes.size : Bytes -> Nat
  32.  Bytes.take : Nat -> Bytes -> Bytes
  33.  Bytes.toBase16 : Bytes -> Bytes
  34.  Bytes.toBase32 : Bytes -> Bytes
  35.  Bytes.toBase64 : Bytes -> Bytes
  36.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  37.  Bytes.toList : Bytes -> [Nat]
  38.  Bytes.zlib.compress : Bytes -> Bytes
  39.  Bytes.zlib.decompress : Bytes -> Either Text Bytes
  40.  builtin type Char
  41.  Char.fromNat : Nat -> Char
  42.  Char.toNat : Char -> Nat
  43.  Char.toText : Char -> Text
  44.  builtin type Code
  45.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  46.  Code.dependencies : Code -> [Term]
  47.  Code.deserialize : Bytes -> Either Text Code
  48.  Code.isMissing : Term ->{IO} Boolean
  49.  Code.lookup : Term ->{IO} Optional Code
  50.  Code.serialize : Code -> Bytes
  51.  crypto.hash : HashAlgorithm -> a -> Bytes
  52.  builtin type crypto.HashAlgorithm
  53.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  54.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  55.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  56.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  57.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  58.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  59.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  60.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  61.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  62.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  63.  Debug.watch : Text -> a -> a
  64.  unique type Doc
  65.  Doc.Blob : Text -> Doc
  66.  Doc.Evaluate : Term -> Doc
  67.  Doc.Join : [Doc] -> Doc
  68.  Doc.Link : Link -> Doc
  69.  Doc.Signature : Term -> Doc
  70.  Doc.Source : Link -> Doc
  71.  type Either a b
  72.  Either.Left : a -> Either a b
  73.  Either.Right : b -> Either a b
  74.  ability Exception
  75.  Exception.raise : Failure ->{Exception} x
  76.  builtin type Float
  77.  Float.* : Float -> Float -> Float
  78.  Float.+ : Float -> Float -> Float
  79.  Float.- : Float -> Float -> Float
  80.  Float./ : Float -> Float -> Float
  81.  Float.abs : Float -> Float
  82.  Float.acos : Float -> Float
  83.  Float.acosh : Float -> Float
  84.  Float.asin : Float -> Float
  85.  Float.asinh : Float -> Float
  86.  Float.atan : Float -> Float
  87.  Float.atan2 : Float -> Float -> Float
  88.  Float.atanh : Float -> Float
  89.  Float.ceiling : Float -> Int
  90.  Float.cos : Float -> Float
  91.  Float.cosh : Float -> Float
  92.  Float.eq : Float -> Float -> Boolean
  93.  Float.exp : Float -> Float
  94.  Float.floor : Float -> Int
  95.  Float.fromText : Text -> Optional Float
  96.  Float.gt : Float -> Float -> Boolean
  97.  Float.gteq : Float -> Float -> Boolean
  98.  Float.log : Float -> Float
  99.  Float.logBase : Float -> Float -> Float
  100. Float.lt : Float -> Float -> Boolean
  101. Float.lteq : Float -> Float -> Boolean
  102. Float.max : Float -> Float -> Float
  103. Float.min : Float -> Float -> Float
  104. Float.pow : Float -> Float -> Float
  105. Float.round : Float -> Int
  106. Float.sin : Float -> Float
  107. Float.sinh : Float -> Float
  108. Float.sqrt : Float -> Float
  109. Float.tan : Float -> Float
  110. Float.tanh : Float -> Float
  111. Float.toText : Float -> Text
  112. Float.truncate : Float -> Int
  113. builtin type Int
  114. Int.* : Int -> Int -> Int
  115. Int.+ : Int -> Int -> Int
  116. Int.- : Int -> Int -> Int
  117. Int./ : Int -> Int -> Int
  118. Int.and : Int -> Int -> Int
  119. Int.complement : Int -> Int
  120. Int.eq : Int -> Int -> Boolean
  121. Int.fromText : Text -> Optional Int
  122. Int.gt : Int -> Int -> Boolean
  123. Int.gteq : Int -> Int -> Boolean
  124. Int.increment : Int -> Int
  125. Int.isEven : Int -> Boolean
  126. Int.isOdd : Int -> Boolean
  127. Int.leadingZeros : Int -> Nat
  128. Int.lt : Int -> Int -> Boolean
  129. Int.lteq : Int -> Int -> Boolean
  130. Int.mod : Int -> Int -> Int
  131. Int.negate : Int -> Int
  132. Int.or : Int -> Int -> Int
  133. Int.popCount : Int -> Nat
  134. Int.pow : Int -> Nat -> Int
  135. Int.shiftLeft : Int -> Nat -> Int
  136. Int.shiftRight : Int -> Nat -> Int
  137. Int.signum : Int -> Int
  138. Int.toFloat : Int -> Float
  139. Int.toText : Int -> Text
  140. Int.trailingZeros : Int -> Nat
  141. Int.truncate0 : Int -> Nat
  142. Int.xor : Int -> Int -> Int
  143. unique type io2.BufferMode
  144. io2.BufferMode.BlockBuffering : BufferMode
  145. io2.BufferMode.LineBuffering : BufferMode
  146. io2.BufferMode.NoBuffering : BufferMode
  147. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  148. unique type io2.Failure
  149. io2.Failure.Failure : Type -> Text -> Any -> Failure
  150. unique type io2.FileMode
  151. io2.FileMode.Append : FileMode
  152. io2.FileMode.Read : FileMode
  153. io2.FileMode.ReadWrite : FileMode
  154. io2.FileMode.Write : FileMode
  155. builtin type io2.Handle
  156. builtin type io2.IO
  157. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  158. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  159. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  160. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  161. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  162. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  163. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  164. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  165. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  166. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  167. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  168. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  169. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  170. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  171. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  172. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  173. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  174. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  175. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  176. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  177. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  178. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  179. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  180. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  181. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  182. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  183. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  184. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  185. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  186. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  187. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  188. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  189. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  190. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  191. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  192. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  193. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  194. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  195. io2.IO.stdHandle : StdHandle -> Handle
  196. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  197. unique type io2.IOError
  198. io2.IOError.AlreadyExists : IOError
  199. io2.IOError.EOF : IOError
  200. io2.IOError.IllegalOperation : IOError
  201. io2.IOError.NoSuchThing : IOError
  202. io2.IOError.PermissionDenied : IOError
  203. io2.IOError.ResourceBusy : IOError
  204. io2.IOError.ResourceExhausted : IOError
  205. io2.IOError.UserError : IOError
  206. unique type io2.IOFailure
  207. builtin type io2.MVar
  208. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  209. io2.MVar.new : a ->{IO} MVar a
  210. io2.MVar.newEmpty : '{IO} MVar a
  211. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  212. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  213. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  214. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  215. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  216. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  217. io2.MVar.tryTake : MVar a ->{IO} Optional a
  218. unique type io2.SeekMode
  219. io2.SeekMode.AbsoluteSeek : SeekMode
  220. io2.SeekMode.RelativeSeek : SeekMode
  221. io2.SeekMode.SeekFromEnd : SeekMode
  222. builtin type io2.Socket
  223. unique type io2.StdHandle
  224. io2.StdHandle.StdErr : StdHandle
  225. io2.StdHandle.StdIn : StdHandle
  226. io2.StdHandle.StdOut : StdHandle
  227. builtin type io2.STM
  228. io2.STM.atomically : '{STM} a ->{IO} a
  229. io2.STM.retry : '{STM} a
  230. builtin type io2.ThreadId
  231. builtin type io2.Tls
  232. builtin type io2.Tls.Cipher
  233. builtin type io2.Tls.ClientConfig
  234. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  235. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  236. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  237. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  238. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  239. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  240. io2.Tls.encodeCert : SignedCert -> Bytes
  241. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  242. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  243. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  244. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  245. builtin type io2.Tls.PrivateKey
  246. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  247. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  248. builtin type io2.Tls.ServerConfig
  249. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  250. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  251. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  252. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  253. builtin type io2.Tls.SignedCert
  254. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  255. builtin type io2.Tls.Version
  256. unique type io2.TlsFailure
  257. builtin type io2.TVar
  258. io2.TVar.new : a ->{STM} TVar a
  259. io2.TVar.newIO : a ->{IO} TVar a
  260. io2.TVar.read : TVar a ->{STM} a
  261. io2.TVar.readIO : TVar a ->{IO} a
  262. io2.TVar.swap : TVar a -> a ->{STM} a
  263. io2.TVar.write : TVar a -> a ->{STM} ()
  264. unique type IsPropagated
  265. IsPropagated.IsPropagated : IsPropagated
  266. unique type IsTest
  267. IsTest.IsTest : IsTest
  268. unique type Link
  269. builtin type Link.Term
  270. Link.Term : Term -> Link
  271. builtin type Link.Type
  272. Link.Type : Type -> Link
  273. builtin type List
  274. List.++ : [a] -> [a] -> [a]
  275. List.+: : a -> [a] -> [a]
  276. List.:+ : [a] -> a -> [a]
  277. List.at : Nat -> [a] -> Optional a
  278. List.cons : a -> [a] -> [a]
  279. List.drop : Nat -> [a] -> [a]
  280. List.empty : [a]
  281. List.size : [a] -> Nat
  282. List.snoc : [a] -> a -> [a]
  283. List.take : Nat -> [a] -> [a]
  284. metadata.isPropagated : IsPropagated
  285. metadata.isTest : IsTest
  286. builtin type Nat
  287. Nat.* : Nat -> Nat -> Nat
  288. Nat.+ : Nat -> Nat -> Nat
  289. Nat./ : Nat -> Nat -> Nat
  290. Nat.and : Nat -> Nat -> Nat
  291. Nat.complement : Nat -> Nat
  292. Nat.drop : Nat -> Nat -> Nat
  293. Nat.eq : Nat -> Nat -> Boolean
  294. Nat.fromText : Text -> Optional Nat
  295. Nat.gt : Nat -> Nat -> Boolean
  296. Nat.gteq : Nat -> Nat -> Boolean
  297. Nat.increment : Nat -> Nat
  298. Nat.isEven : Nat -> Boolean
  299. Nat.isOdd : Nat -> Boolean
  300. Nat.leadingZeros : Nat -> Nat
  301. Nat.lt : Nat -> Nat -> Boolean
  302. Nat.lteq : Nat -> Nat -> Boolean
  303. Nat.mod : Nat -> Nat -> Nat
  304. Nat.or : Nat -> Nat -> Nat
  305. Nat.popCount : Nat -> Nat
  306. Nat.pow : Nat -> Nat -> Nat
  307. Nat.shiftLeft : Nat -> Nat -> Nat
  308. Nat.shiftRight : Nat -> Nat -> Nat
  309. Nat.sub : Nat -> Nat -> Int
  310. Nat.toFloat : Nat -> Float
  311. Nat.toInt : Nat -> Int
  312. Nat.toText : Nat -> Text
  313. Nat.trailingZeros : Nat -> Nat
  314. Nat.xor : Nat -> Nat -> Nat
  315. type Optional a
  316. Optional.None : Optional a
  317. Optional.Some : a -> Optional a
  318. builtin type Request
  319. type SeqView a b
  320. SeqView.VElem : a -> b -> SeqView a b
  321. SeqView.VEmpty : SeqView a b
  322. unique type Test.Result
  323. Test.Result.Fail : Text -> Result
  324. Test.Result.Ok : Text -> Result
  325. builtin type Text
  326. Text.!= : Text -> Text -> Boolean
  327. Text.++ : Text -> Text -> Text
  328. Text.drop : Nat -> Text -> Text
  329. Text.empty : Text
  330. Text.eq : Text -> Text -> Boolean
  331. Text.fromCharList : [Char] -> Text
  332. Text.fromUtf8.impl : Bytes -> Either Failure Text
  333. Text.gt : Text -> Text -> Boolean
  334. Text.gteq : Text -> Text -> Boolean
  335. Text.lt : Text -> Text -> Boolean
  336. Text.lteq : Text -> Text -> Boolean
  337. Text.repeat : Nat -> Text -> Text
  338. Text.size : Text -> Nat
  339. Text.take : Nat -> Text -> Text
  340. Text.toCharList : Text -> [Char]
  341. Text.toUtf8 : Text -> Bytes
  342. Text.uncons : Text -> Optional (Char, Text)
  343. Text.unsnoc : Text -> Optional (Text, Char)
  344. todo : a -> b
  345. type Tuple a b
  346. Tuple.Cons : a -> b -> Tuple a b
  347. type Unit
  348. Unit.Unit : ()
  349. Universal.< : a -> a -> Boolean
  350. Universal.<= : a -> a -> Boolean
  351. Universal.== : a -> a -> Boolean
  352. Universal.> : a -> a -> Boolean
  353. Universal.>= : a -> a -> Boolean
  354. Universal.compare : a -> a -> Int
  355. builtin type Value
  356. Value.dependencies : Value -> [Term]
  357. Value.deserialize : Bytes -> Either Text Value
  358. Value.load : Value ->{IO} Either [Term] a
  359. Value.serialize : Value -> Bytes
  360. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.floor    : Float -> Int
    2.  Float.fromText : Text -> Optional Float
    3.  Float.gt       : Float -> Float -> Boolean
    4.  Float.gteq     : Float -> Float -> Boolean
    5.  Float.log      : Float -> Float
    6.  Float.logBase  : Float -> Float -> Float
    7.  Float.lt       : Float -> Float -> Boolean
    8.  Float.lteq     : Float -> Float -> Boolean
    9.  Float.max      : Float -> Float -> Float
    10. Float.min      : Float -> Float -> Float
    11. Float.pow      : Float -> Float -> Float
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar

.runar> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{g} Boolean) -> [a] ->{g} Boolean
  3.  List.any : (a ->{g} Boolean) -> [a] ->{g} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{g} Boolean) -> [a] ->{g} [a]
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
                           -> [a]
                           ->{g} Boolean
    3.  List.any           : (a ->{g} Boolean)
                           -> [a]
                           ->{g} Boolean
    4.  List.chunk         : Nat -> [a] -> [[a]]
    5.  List.chunksOf      : Nat -> [a] -> [[a]]
    6.  List.dropWhile     : (a ->{g} Boolean) -> [a] ->{g} [a]
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

  1.  Float.floor : Float -> Int
  2.  Float.fromText : Text -> Optional Float
  3.  Float.gt : Float -> Float -> Boolean
  4.  Float.gteq : Float -> Float -> Boolean
  5.  Float.log : Float -> Float
  6.  Float.logBase : Float -> Float -> Float
  7.  Float.lt : Float -> Float -> Boolean
  8.  Float.lteq : Float -> Float -> Boolean
  9.  Float.max : Float -> Float -> Float
  10. Float.min : Float -> Float -> Float
  11. Float.pow : Float -> Float -> Float
  12. List.adjacentPairs : [a] -> [(a, a)]
  13. List.all : (a ->{g} Boolean) -> [a] ->{g} Boolean
  14. List.any : (a ->{g} Boolean) -> [a] ->{g} Boolean
  15. List.chunk : Nat -> [a] -> [[a]]
  16. List.chunksOf : Nat -> [a] -> [[a]]
  17. List.dropWhile : (a ->{g} Boolean) -> [a] ->{g} [a]
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
