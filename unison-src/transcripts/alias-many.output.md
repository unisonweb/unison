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
  29.  Bytes.size : Bytes -> Nat
  30.  Bytes.take : Nat -> Bytes -> Bytes
  31.  Bytes.toBase16 : Bytes -> Bytes
  32.  Bytes.toBase32 : Bytes -> Bytes
  33.  Bytes.toBase64 : Bytes -> Bytes
  34.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  35.  Bytes.toList : Bytes -> [Nat]
  36.  builtin type Char
  37.  Char.fromNat : Nat -> Char
  38.  Char.toNat : Char -> Nat
  39.  Char.toText : Char -> Text
  40.  builtin type Code
  41.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  42.  Code.dependencies : Code -> [Term]
  43.  Code.deserialize : Bytes -> Either Text Code
  44.  Code.isMissing : Term ->{IO} Boolean
  45.  Code.lookup : Term ->{IO} Optional Code
  46.  Code.serialize : Code -> Bytes
  47.  crypto.hash : HashAlgorithm -> a -> Bytes
  48.  builtin type crypto.HashAlgorithm
  49.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  50.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  51.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  52.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  53.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  54.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  55.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  56.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  57.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  58.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  59.  Debug.watch : Text -> a -> a
  60.  unique type Doc
  61.  Doc.Blob : Text -> Doc
  62.  Doc.Evaluate : Term -> Doc
  63.  Doc.Join : [Doc] -> Doc
  64.  Doc.Link : Link -> Doc
  65.  Doc.Signature : Term -> Doc
  66.  Doc.Source : Link -> Doc
  67.  type Either a b
  68.  Either.Left : a -> Either a b
  69.  Either.Right : b -> Either a b
  70.  ability Exception
  71.  Exception.raise : Failure ->{Exception} x
  72.  builtin type Float
  73.  Float.* : Float -> Float -> Float
  74.  Float.+ : Float -> Float -> Float
  75.  Float.- : Float -> Float -> Float
  76.  Float./ : Float -> Float -> Float
  77.  Float.abs : Float -> Float
  78.  Float.acos : Float -> Float
  79.  Float.acosh : Float -> Float
  80.  Float.asin : Float -> Float
  81.  Float.asinh : Float -> Float
  82.  Float.atan : Float -> Float
  83.  Float.atan2 : Float -> Float -> Float
  84.  Float.atanh : Float -> Float
  85.  Float.ceiling : Float -> Int
  86.  Float.cos : Float -> Float
  87.  Float.cosh : Float -> Float
  88.  Float.eq : Float -> Float -> Boolean
  89.  Float.exp : Float -> Float
  90.  Float.floor : Float -> Int
  91.  Float.fromText : Text -> Optional Float
  92.  Float.gt : Float -> Float -> Boolean
  93.  Float.gteq : Float -> Float -> Boolean
  94.  Float.log : Float -> Float
  95.  Float.logBase : Float -> Float -> Float
  96.  Float.lt : Float -> Float -> Boolean
  97.  Float.lteq : Float -> Float -> Boolean
  98.  Float.max : Float -> Float -> Float
  99.  Float.min : Float -> Float -> Float
  100. Float.pow : Float -> Float -> Float
  101. Float.round : Float -> Int
  102. Float.sin : Float -> Float
  103. Float.sinh : Float -> Float
  104. Float.sqrt : Float -> Float
  105. Float.tan : Float -> Float
  106. Float.tanh : Float -> Float
  107. Float.toText : Float -> Text
  108. Float.truncate : Float -> Int
  109. builtin type Int
  110. Int.* : Int -> Int -> Int
  111. Int.+ : Int -> Int -> Int
  112. Int.- : Int -> Int -> Int
  113. Int./ : Int -> Int -> Int
  114. Int.and : Int -> Int -> Int
  115. Int.complement : Int -> Int
  116. Int.eq : Int -> Int -> Boolean
  117. Int.fromText : Text -> Optional Int
  118. Int.gt : Int -> Int -> Boolean
  119. Int.gteq : Int -> Int -> Boolean
  120. Int.increment : Int -> Int
  121. Int.isEven : Int -> Boolean
  122. Int.isOdd : Int -> Boolean
  123. Int.leadingZeros : Int -> Nat
  124. Int.lt : Int -> Int -> Boolean
  125. Int.lteq : Int -> Int -> Boolean
  126. Int.mod : Int -> Int -> Int
  127. Int.negate : Int -> Int
  128. Int.or : Int -> Int -> Int
  129. Int.popCount : Int -> Nat
  130. Int.pow : Int -> Nat -> Int
  131. Int.shiftLeft : Int -> Nat -> Int
  132. Int.shiftRight : Int -> Nat -> Int
  133. Int.signum : Int -> Int
  134. Int.toFloat : Int -> Float
  135. Int.toText : Int -> Text
  136. Int.trailingZeros : Int -> Nat
  137. Int.truncate0 : Int -> Nat
  138. Int.xor : Int -> Int -> Int
  139. unique type io2.BufferMode
  140. io2.BufferMode.BlockBuffering : BufferMode
  141. io2.BufferMode.LineBuffering : BufferMode
  142. io2.BufferMode.NoBuffering : BufferMode
  143. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  144. unique type io2.Failure
  145. io2.Failure.Failure : Type -> Text -> Any -> Failure
  146. unique type io2.FileMode
  147. io2.FileMode.Append : FileMode
  148. io2.FileMode.Read : FileMode
  149. io2.FileMode.ReadWrite : FileMode
  150. io2.FileMode.Write : FileMode
  151. builtin type io2.Handle
  152. builtin type io2.IO
  153. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  154. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  155. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  156. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  157. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  158. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  159. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  160. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  161. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  162. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  163. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  164. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  165. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  166. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  167. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  168. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  169. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  170. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  171. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  172. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  173. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  174. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  175. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  176. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  177. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  178. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  179. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  180. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  181. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  182. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  183. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  184. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  185. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  186. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  187. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  188. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  189. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  190. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  191. io2.IO.stdHandle : StdHandle -> Handle
  192. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  193. unique type io2.IOError
  194. io2.IOError.AlreadyExists : IOError
  195. io2.IOError.EOF : IOError
  196. io2.IOError.IllegalOperation : IOError
  197. io2.IOError.NoSuchThing : IOError
  198. io2.IOError.PermissionDenied : IOError
  199. io2.IOError.ResourceBusy : IOError
  200. io2.IOError.ResourceExhausted : IOError
  201. io2.IOError.UserError : IOError
  202. unique type io2.IOFailure
  203. builtin type io2.MVar
  204. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  205. io2.MVar.new : a ->{IO} MVar a
  206. io2.MVar.newEmpty : '{IO} MVar a
  207. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  208. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  209. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  210. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  211. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  212. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  213. io2.MVar.tryTake : MVar a ->{IO} Optional a
  214. unique type io2.SeekMode
  215. io2.SeekMode.AbsoluteSeek : SeekMode
  216. io2.SeekMode.RelativeSeek : SeekMode
  217. io2.SeekMode.SeekFromEnd : SeekMode
  218. builtin type io2.Socket
  219. unique type io2.StdHandle
  220. io2.StdHandle.StdErr : StdHandle
  221. io2.StdHandle.StdIn : StdHandle
  222. io2.StdHandle.StdOut : StdHandle
  223. builtin type io2.STM
  224. io2.STM.atomically : '{STM} a ->{IO} a
  225. io2.STM.retry : '{STM} a
  226. builtin type io2.ThreadId
  227. builtin type io2.Tls
  228. builtin type io2.Tls.Cipher
  229. builtin type io2.Tls.ClientConfig
  230. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  231. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  232. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  233. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  234. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  235. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  236. io2.Tls.encodeCert : SignedCert -> Bytes
  237. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  238. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  239. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  240. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  241. builtin type io2.Tls.PrivateKey
  242. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  243. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  244. builtin type io2.Tls.ServerConfig
  245. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  246. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  247. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  248. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  249. builtin type io2.Tls.SignedCert
  250. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  251. builtin type io2.Tls.Version
  252. unique type io2.TlsFailure
  253. builtin type io2.TVar
  254. io2.TVar.new : a ->{STM} TVar a
  255. io2.TVar.newIO : a ->{IO} TVar a
  256. io2.TVar.read : TVar a ->{STM} a
  257. io2.TVar.readIO : TVar a ->{IO} a
  258. io2.TVar.swap : TVar a -> a ->{STM} a
  259. io2.TVar.write : TVar a -> a ->{STM} ()
  260. unique type IsPropagated
  261. IsPropagated.IsPropagated : IsPropagated
  262. unique type IsTest
  263. IsTest.IsTest : IsTest
  264. unique type Link
  265. builtin type Link.Term
  266. Link.Term : Term -> Link
  267. builtin type Link.Type
  268. Link.Type : Type -> Link
  269. builtin type List
  270. List.++ : [a] -> [a] -> [a]
  271. List.+: : a -> [a] -> [a]
  272. List.:+ : [a] -> a -> [a]
  273. List.at : Nat -> [a] -> Optional a
  274. List.cons : a -> [a] -> [a]
  275. List.drop : Nat -> [a] -> [a]
  276. List.empty : [a]
  277. List.size : [a] -> Nat
  278. List.snoc : [a] -> a -> [a]
  279. List.take : Nat -> [a] -> [a]
  280. metadata.isPropagated : IsPropagated
  281. metadata.isTest : IsTest
  282. builtin type Nat
  283. Nat.* : Nat -> Nat -> Nat
  284. Nat.+ : Nat -> Nat -> Nat
  285. Nat./ : Nat -> Nat -> Nat
  286. Nat.and : Nat -> Nat -> Nat
  287. Nat.complement : Nat -> Nat
  288. Nat.drop : Nat -> Nat -> Nat
  289. Nat.eq : Nat -> Nat -> Boolean
  290. Nat.fromText : Text -> Optional Nat
  291. Nat.gt : Nat -> Nat -> Boolean
  292. Nat.gteq : Nat -> Nat -> Boolean
  293. Nat.increment : Nat -> Nat
  294. Nat.isEven : Nat -> Boolean
  295. Nat.isOdd : Nat -> Boolean
  296. Nat.leadingZeros : Nat -> Nat
  297. Nat.lt : Nat -> Nat -> Boolean
  298. Nat.lteq : Nat -> Nat -> Boolean
  299. Nat.mod : Nat -> Nat -> Nat
  300. Nat.or : Nat -> Nat -> Nat
  301. Nat.popCount : Nat -> Nat
  302. Nat.pow : Nat -> Nat -> Nat
  303. Nat.shiftLeft : Nat -> Nat -> Nat
  304. Nat.shiftRight : Nat -> Nat -> Nat
  305. Nat.sub : Nat -> Nat -> Int
  306. Nat.toFloat : Nat -> Float
  307. Nat.toInt : Nat -> Int
  308. Nat.toText : Nat -> Text
  309. Nat.trailingZeros : Nat -> Nat
  310. Nat.xor : Nat -> Nat -> Nat
  311. type Optional a
  312. Optional.None : Optional a
  313. Optional.Some : a -> Optional a
  314. builtin type Request
  315. type SeqView a b
  316. SeqView.VElem : a -> b -> SeqView a b
  317. SeqView.VEmpty : SeqView a b
  318. unique type Test.Result
  319. Test.Result.Fail : Text -> Result
  320. Test.Result.Ok : Text -> Result
  321. builtin type Text
  322. Text.!= : Text -> Text -> Boolean
  323. Text.++ : Text -> Text -> Text
  324. Text.drop : Nat -> Text -> Text
  325. Text.empty : Text
  326. Text.eq : Text -> Text -> Boolean
  327. Text.fromCharList : [Char] -> Text
  328. Text.fromUtf8.impl : Bytes -> Either Failure Text
  329. Text.gt : Text -> Text -> Boolean
  330. Text.gteq : Text -> Text -> Boolean
  331. Text.lt : Text -> Text -> Boolean
  332. Text.lteq : Text -> Text -> Boolean
  333. Text.repeat : Nat -> Text -> Text
  334. Text.size : Text -> Nat
  335. Text.take : Nat -> Text -> Text
  336. Text.toCharList : Text -> [Char]
  337. Text.toUtf8 : Text -> Bytes
  338. Text.uncons : Text -> Optional (Char, Text)
  339. Text.unsnoc : Text -> Optional (Text, Char)
  340. todo : a -> b
  341. type Tuple a b
  342. Tuple.Cons : a -> b -> Tuple a b
  343. type Unit
  344. Unit.Unit : ()
  345. Universal.< : a -> a -> Boolean
  346. Universal.<= : a -> a -> Boolean
  347. Universal.== : a -> a -> Boolean
  348. Universal.> : a -> a -> Boolean
  349. Universal.>= : a -> a -> Boolean
  350. Universal.compare : a -> a -> Int
  351. unsafe.coerceAbilities : (a -> b) -> a -> b
  352. builtin type Value
  353. Value.dependencies : Value -> [Term]
  354. Value.deserialize : Bytes -> Either Text Value
  355. Value.load : Value ->{IO} Either [Term] a
  356. Value.serialize : Value -> Bytes
  357. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.log     : Float -> Float
    2.  Float.logBase : Float -> Float -> Float
    3.  Float.lt      : Float -> Float -> Boolean
    4.  Float.lteq    : Float -> Float -> Boolean
    5.  Float.max     : Float -> Float -> Float
    6.  Float.min     : Float -> Float -> Float
    7.  Float.pow     : Float -> Float -> Float
    8.  Float.round   : Float -> Int
    9.  Float.sin     : Float -> Float
    10. Float.sinh    : Float -> Float
    11. Float.sqrt    : Float -> Float
  
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

  1.  Float.log : Float -> Float
  2.  Float.logBase : Float -> Float -> Float
  3.  Float.lt : Float -> Float -> Boolean
  4.  Float.lteq : Float -> Float -> Boolean
  5.  Float.max : Float -> Float -> Float
  6.  Float.min : Float -> Float -> Float
  7.  Float.pow : Float -> Float -> Float
  8.  Float.round : Float -> Int
  9.  Float.sin : Float -> Float
  10. Float.sinh : Float -> Float
  11. Float.sqrt : Float -> Float
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
