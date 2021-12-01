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
  3.   Any.unsafeExtract : Any -> a
  4.   builtin type Boolean
  5.   Boolean.not : Boolean -> Boolean
  6.   bug : a -> b
  7.   builtin type Bytes
  8.   Bytes.++ : Bytes -> Bytes -> Bytes
  9.   Bytes.at : Nat -> Bytes -> Optional Nat
  10.  Bytes.decodeNat16be : Bytes -> Optional (Nat, Bytes)
  11.  Bytes.decodeNat16le : Bytes -> Optional (Nat, Bytes)
  12.  Bytes.decodeNat32be : Bytes -> Optional (Nat, Bytes)
  13.  Bytes.decodeNat32le : Bytes -> Optional (Nat, Bytes)
  14.  Bytes.decodeNat64be : Bytes -> Optional (Nat, Bytes)
  15.  Bytes.decodeNat64le : Bytes -> Optional (Nat, Bytes)
  16.  Bytes.drop : Nat -> Bytes -> Bytes
  17.  Bytes.empty : Bytes
  18.  Bytes.encodeNat16be : Nat -> Bytes
  19.  Bytes.encodeNat16le : Nat -> Bytes
  20.  Bytes.encodeNat32be : Nat -> Bytes
  21.  Bytes.encodeNat32le : Nat -> Bytes
  22.  Bytes.encodeNat64be : Nat -> Bytes
  23.  Bytes.encodeNat64le : Nat -> Bytes
  24.  Bytes.flatten : Bytes -> Bytes
  25.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  26.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  27.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  28.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  29.  Bytes.fromList : [Nat] -> Bytes
  30.  Bytes.gzip.compress : Bytes -> Bytes
  31.  Bytes.gzip.decompress : Bytes -> Either Text Bytes
  32.  Bytes.size : Bytes -> Nat
  33.  Bytes.take : Nat -> Bytes -> Bytes
  34.  Bytes.toBase16 : Bytes -> Bytes
  35.  Bytes.toBase32 : Bytes -> Bytes
  36.  Bytes.toBase64 : Bytes -> Bytes
  37.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  38.  Bytes.toList : Bytes -> [Nat]
  39.  Bytes.zlib.compress : Bytes -> Bytes
  40.  Bytes.zlib.decompress : Bytes -> Either Text Bytes
  41.  builtin type Char
  42.  Char.fromNat : Nat -> Char
  43.  Char.toNat : Char -> Nat
  44.  Char.toText : Char -> Text
  45.  builtin type Code
  46.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  47.  Code.dependencies : Code -> [Term]
  48.  Code.deserialize : Bytes -> Either Text Code
  49.  Code.display : Text -> Code -> Text
  50.  Code.isMissing : Term ->{IO} Boolean
  51.  Code.lookup : Term ->{IO} Optional Code
  52.  Code.serialize : Code -> Bytes
  53.  Code.validate : [(Term, Code)] ->{IO} Optional Failure
  54.  crypto.hash : HashAlgorithm -> a -> Bytes
  55.  builtin type crypto.HashAlgorithm
  56.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  57.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  58.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  59.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  60.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  61.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  62.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  63.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  64.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  65.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  66.  Debug.watch : Text -> a -> a
  67.  unique type Doc
  68.  Doc.Blob : Text -> Doc
  69.  Doc.Evaluate : Term -> Doc
  70.  Doc.Join : [Doc] -> Doc
  71.  Doc.Link : Link -> Doc
  72.  Doc.Signature : Term -> Doc
  73.  Doc.Source : Link -> Doc
  74.  structural type Either a b
  75.  Either.Left : a -> Either a b
  76.  Either.Right : b -> Either a b
  77.  structural ability Exception
  78.  Exception.raise : Failure ->{Exception} x
  79.  builtin type Float
  80.  Float.* : Float -> Float -> Float
  81.  Float.+ : Float -> Float -> Float
  82.  Float.- : Float -> Float -> Float
  83.  Float./ : Float -> Float -> Float
  84.  Float.abs : Float -> Float
  85.  Float.acos : Float -> Float
  86.  Float.acosh : Float -> Float
  87.  Float.asin : Float -> Float
  88.  Float.asinh : Float -> Float
  89.  Float.atan : Float -> Float
  90.  Float.atan2 : Float -> Float -> Float
  91.  Float.atanh : Float -> Float
  92.  Float.ceiling : Float -> Int
  93.  Float.cos : Float -> Float
  94.  Float.cosh : Float -> Float
  95.  Float.eq : Float -> Float -> Boolean
  96.  Float.exp : Float -> Float
  97.  Float.floor : Float -> Int
  98.  Float.fromRepresentation : Nat -> Float
  99.  Float.fromText : Text -> Optional Float
  100. Float.gt : Float -> Float -> Boolean
  101. Float.gteq : Float -> Float -> Boolean
  102. Float.log : Float -> Float
  103. Float.logBase : Float -> Float -> Float
  104. Float.lt : Float -> Float -> Boolean
  105. Float.lteq : Float -> Float -> Boolean
  106. Float.max : Float -> Float -> Float
  107. Float.min : Float -> Float -> Float
  108. Float.pow : Float -> Float -> Float
  109. Float.round : Float -> Int
  110. Float.sin : Float -> Float
  111. Float.sinh : Float -> Float
  112. Float.sqrt : Float -> Float
  113. Float.tan : Float -> Float
  114. Float.tanh : Float -> Float
  115. Float.toRepresentation : Float -> Nat
  116. Float.toText : Float -> Text
  117. Float.truncate : Float -> Int
  118. Handle.toText : Handle -> Text
  119. builtin type Int
  120. Int.* : Int -> Int -> Int
  121. Int.+ : Int -> Int -> Int
  122. Int.- : Int -> Int -> Int
  123. Int./ : Int -> Int -> Int
  124. Int.and : Int -> Int -> Int
  125. Int.complement : Int -> Int
  126. Int.eq : Int -> Int -> Boolean
  127. Int.fromRepresentation : Nat -> Int
  128. Int.fromText : Text -> Optional Int
  129. Int.gt : Int -> Int -> Boolean
  130. Int.gteq : Int -> Int -> Boolean
  131. Int.increment : Int -> Int
  132. Int.isEven : Int -> Boolean
  133. Int.isOdd : Int -> Boolean
  134. Int.leadingZeros : Int -> Nat
  135. Int.lt : Int -> Int -> Boolean
  136. Int.lteq : Int -> Int -> Boolean
  137. Int.mod : Int -> Int -> Int
  138. Int.negate : Int -> Int
  139. Int.or : Int -> Int -> Int
  140. Int.popCount : Int -> Nat
  141. Int.pow : Int -> Nat -> Int
  142. Int.shiftLeft : Int -> Nat -> Int
  143. Int.shiftRight : Int -> Nat -> Int
  144. Int.signum : Int -> Int
  145. Int.toFloat : Int -> Float
  146. Int.toRepresentation : Int -> Nat
  147. Int.toText : Int -> Text
  148. Int.trailingZeros : Int -> Nat
  149. Int.truncate0 : Int -> Nat
  150. Int.xor : Int -> Int -> Int
  151. unique type io2.BufferMode
  152. io2.BufferMode.BlockBuffering : BufferMode
  153. io2.BufferMode.LineBuffering : BufferMode
  154. io2.BufferMode.NoBuffering : BufferMode
  155. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  156. unique type io2.Failure
  157. io2.Failure.Failure : Type -> Text -> Any -> Failure
  158. unique type io2.FileMode
  159. io2.FileMode.Append : FileMode
  160. io2.FileMode.Read : FileMode
  161. io2.FileMode.ReadWrite : FileMode
  162. io2.FileMode.Write : FileMode
  163. builtin type io2.Handle
  164. builtin type io2.IO
  165. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  166. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  167. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  168. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  169. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  170. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  171. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  172. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  173. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  174. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  175. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  176. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  177. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  178. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  179. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  180. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  181. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  182. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  183. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  184. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  185. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  186. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  187. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  188. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  189. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  190. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  191. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  192. io2.IO.ref : a ->{IO} Ref {IO} a
  193. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  194. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  195. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  196. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  197. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  198. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  199. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  200. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  201. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  202. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  203. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  204. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  205. io2.IO.stdHandle : StdHandle -> Handle
  206. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  207. io2.IO.systemTimeMicroseconds : '{IO} Int
  208. unique type io2.IOError
  209. io2.IOError.AlreadyExists : IOError
  210. io2.IOError.EOF : IOError
  211. io2.IOError.IllegalOperation : IOError
  212. io2.IOError.NoSuchThing : IOError
  213. io2.IOError.PermissionDenied : IOError
  214. io2.IOError.ResourceBusy : IOError
  215. io2.IOError.ResourceExhausted : IOError
  216. io2.IOError.UserError : IOError
  217. unique type io2.IOFailure
  218. builtin type io2.MVar
  219. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  220. io2.MVar.new : a ->{IO} MVar a
  221. io2.MVar.newEmpty : '{IO} MVar a
  222. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  223. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  224. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  225. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  226. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  227. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  228. io2.MVar.tryTake : MVar a ->{IO} Optional a
  229. unique type io2.SeekMode
  230. io2.SeekMode.AbsoluteSeek : SeekMode
  231. io2.SeekMode.RelativeSeek : SeekMode
  232. io2.SeekMode.SeekFromEnd : SeekMode
  233. builtin type io2.Socket
  234. unique type io2.StdHandle
  235. io2.StdHandle.StdErr : StdHandle
  236. io2.StdHandle.StdIn : StdHandle
  237. io2.StdHandle.StdOut : StdHandle
  238. builtin type io2.STM
  239. io2.STM.atomically : '{STM} a ->{IO} a
  240. io2.STM.retry : '{STM} a
  241. builtin type io2.ThreadId
  242. builtin type io2.Tls
  243. builtin type io2.Tls.Cipher
  244. builtin type io2.Tls.ClientConfig
  245. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  246. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  247. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  248. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  249. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  250. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  251. io2.Tls.encodeCert : SignedCert -> Bytes
  252. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  253. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  254. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  255. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  256. builtin type io2.Tls.PrivateKey
  257. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  258. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  259. builtin type io2.Tls.ServerConfig
  260. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  261. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  262. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  263. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  264. builtin type io2.Tls.SignedCert
  265. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  266. builtin type io2.Tls.Version
  267. unique type io2.TlsFailure
  268. builtin type io2.TVar
  269. io2.TVar.new : a ->{STM} TVar a
  270. io2.TVar.newIO : a ->{IO} TVar a
  271. io2.TVar.read : TVar a ->{STM} a
  272. io2.TVar.readIO : TVar a ->{IO} a
  273. io2.TVar.swap : TVar a -> a ->{STM} a
  274. io2.TVar.write : TVar a -> a ->{STM} ()
  275. unique type IsPropagated
  276. IsPropagated.IsPropagated : IsPropagated
  277. unique type IsTest
  278. IsTest.IsTest : IsTest
  279. unique type Link
  280. builtin type Link.Term
  281. Link.Term : Term -> Link
  282. Link.Term.toText : Term -> Text
  283. builtin type Link.Type
  284. Link.Type : Type -> Link
  285. builtin type List
  286. List.++ : [a] -> [a] -> [a]
  287. List.+: : a -> [a] -> [a]
  288. List.:+ : [a] -> a -> [a]
  289. List.at : Nat -> [a] -> Optional a
  290. List.cons : a -> [a] -> [a]
  291. List.drop : Nat -> [a] -> [a]
  292. List.empty : [a]
  293. List.size : [a] -> Nat
  294. List.snoc : [a] -> a -> [a]
  295. List.take : Nat -> [a] -> [a]
  296. metadata.isPropagated : IsPropagated
  297. metadata.isTest : IsTest
  298. builtin type Nat
  299. Nat.* : Nat -> Nat -> Nat
  300. Nat.+ : Nat -> Nat -> Nat
  301. Nat./ : Nat -> Nat -> Nat
  302. Nat.and : Nat -> Nat -> Nat
  303. Nat.complement : Nat -> Nat
  304. Nat.drop : Nat -> Nat -> Nat
  305. Nat.eq : Nat -> Nat -> Boolean
  306. Nat.fromText : Text -> Optional Nat
  307. Nat.gt : Nat -> Nat -> Boolean
  308. Nat.gteq : Nat -> Nat -> Boolean
  309. Nat.increment : Nat -> Nat
  310. Nat.isEven : Nat -> Boolean
  311. Nat.isOdd : Nat -> Boolean
  312. Nat.leadingZeros : Nat -> Nat
  313. Nat.lt : Nat -> Nat -> Boolean
  314. Nat.lteq : Nat -> Nat -> Boolean
  315. Nat.mod : Nat -> Nat -> Nat
  316. Nat.or : Nat -> Nat -> Nat
  317. Nat.popCount : Nat -> Nat
  318. Nat.pow : Nat -> Nat -> Nat
  319. Nat.shiftLeft : Nat -> Nat -> Nat
  320. Nat.shiftRight : Nat -> Nat -> Nat
  321. Nat.sub : Nat -> Nat -> Int
  322. Nat.toFloat : Nat -> Float
  323. Nat.toInt : Nat -> Int
  324. Nat.toText : Nat -> Text
  325. Nat.trailingZeros : Nat -> Nat
  326. Nat.xor : Nat -> Nat -> Nat
  327. structural type Optional a
  328. Optional.None : Optional a
  329. Optional.Some : a -> Optional a
  330. builtin type Ref
  331. Ref.read : Ref g a ->{g} a
  332. Ref.write : Ref g a -> a ->{g} ()
  333. builtin type Request
  334. builtin type Scope
  335. Scope.ref : a ->{Scope s} Ref {Scope s} a
  336. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  337. structural type SeqView a b
  338. SeqView.VElem : a -> b -> SeqView a b
  339. SeqView.VEmpty : SeqView a b
  340. Socket.toText : Socket -> Text
  341. unique type Test.Result
  342. Test.Result.Fail : Text -> Result
  343. Test.Result.Ok : Text -> Result
  344. builtin type Text
  345. Text.!= : Text -> Text -> Boolean
  346. Text.++ : Text -> Text -> Text
  347. Text.drop : Nat -> Text -> Text
  348. Text.empty : Text
  349. Text.eq : Text -> Text -> Boolean
  350. Text.fromCharList : [Char] -> Text
  351. Text.fromUtf8.impl : Bytes -> Either Failure Text
  352. Text.gt : Text -> Text -> Boolean
  353. Text.gteq : Text -> Text -> Boolean
  354. Text.lt : Text -> Text -> Boolean
  355. Text.lteq : Text -> Text -> Boolean
  356. Text.repeat : Nat -> Text -> Text
  357. Text.size : Text -> Nat
  358. Text.take : Nat -> Text -> Text
  359. Text.toCharList : Text -> [Char]
  360. Text.toUtf8 : Text -> Bytes
  361. Text.uncons : Text -> Optional (Char, Text)
  362. Text.unsnoc : Text -> Optional (Text, Char)
  363. ThreadId.toText : ThreadId -> Text
  364. todo : a -> b
  365. structural type Tuple a b
  366. Tuple.Cons : a -> b -> Tuple a b
  367. structural type Unit
  368. Unit.Unit : ()
  369. Universal.< : a -> a -> Boolean
  370. Universal.<= : a -> a -> Boolean
  371. Universal.== : a -> a -> Boolean
  372. Universal.> : a -> a -> Boolean
  373. Universal.>= : a -> a -> Boolean
  374. Universal.compare : a -> a -> Int
  375. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  376. builtin type Value
  377. Value.dependencies : Value -> [Term]
  378. Value.deserialize : Bytes -> Either Text Value
  379. Value.load : Value ->{IO} Either [Term] a
  380. Value.serialize : Value -> Bytes
  381. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.cosh               : Float -> Float
    2.  Float.eq                 : Float -> Float -> Boolean
    3.  Float.exp                : Float -> Float
    4.  Float.floor              : Float -> Int
    5.  Float.fromRepresentation : Nat -> Float
    6.  Float.fromText           : Text -> Optional Float
    7.  Float.gt                 : Float -> Float -> Boolean
    8.  Float.gteq               : Float -> Float -> Boolean
    9.  Float.log                : Float -> Float
    10. Float.logBase            : Float -> Float -> Float
    11. Float.lt                 : Float -> Float -> Boolean
  
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

  1.  Float.cosh : Float -> Float
  2.  Float.eq : Float -> Float -> Boolean
  3.  Float.exp : Float -> Float
  4.  Float.floor : Float -> Int
  5.  Float.fromRepresentation : Nat -> Float
  6.  Float.fromText : Text -> Optional Float
  7.  Float.gt : Float -> Float -> Boolean
  8.  Float.gteq : Float -> Float -> Boolean
  9.  Float.log : Float -> Float
  10. Float.logBase : Float -> Float -> Float
  11. Float.lt : Float -> Float -> Boolean
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
