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
  59.  crypto.HashAlgorithm.Sha1 : HashAlgorithm
  60.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  61.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  62.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  63.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  64.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  65.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  66.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  67.  Debug.trace : Text -> a -> ()
  68.  Debug.watch : Text -> a -> a
  69.  unique type Doc
  70.  Doc.Blob : Text -> Doc
  71.  Doc.Evaluate : Term -> Doc
  72.  Doc.Join : [Doc] -> Doc
  73.  Doc.Link : Link -> Doc
  74.  Doc.Signature : Term -> Doc
  75.  Doc.Source : Link -> Doc
  76.  structural type Either a b
  77.  Either.Left : a -> Either a b
  78.  Either.Right : b -> Either a b
  79.  structural ability Exception
  80.  Exception.raise : Failure ->{Exception} x
  81.  builtin type Float
  82.  Float.* : Float -> Float -> Float
  83.  Float.+ : Float -> Float -> Float
  84.  Float.- : Float -> Float -> Float
  85.  Float./ : Float -> Float -> Float
  86.  Float.abs : Float -> Float
  87.  Float.acos : Float -> Float
  88.  Float.acosh : Float -> Float
  89.  Float.asin : Float -> Float
  90.  Float.asinh : Float -> Float
  91.  Float.atan : Float -> Float
  92.  Float.atan2 : Float -> Float -> Float
  93.  Float.atanh : Float -> Float
  94.  Float.ceiling : Float -> Int
  95.  Float.cos : Float -> Float
  96.  Float.cosh : Float -> Float
  97.  Float.eq : Float -> Float -> Boolean
  98.  Float.exp : Float -> Float
  99.  Float.floor : Float -> Int
  100. Float.fromRepresentation : Nat -> Float
  101. Float.fromText : Text -> Optional Float
  102. Float.gt : Float -> Float -> Boolean
  103. Float.gteq : Float -> Float -> Boolean
  104. Float.log : Float -> Float
  105. Float.logBase : Float -> Float -> Float
  106. Float.lt : Float -> Float -> Boolean
  107. Float.lteq : Float -> Float -> Boolean
  108. Float.max : Float -> Float -> Float
  109. Float.min : Float -> Float -> Float
  110. Float.pow : Float -> Float -> Float
  111. Float.round : Float -> Int
  112. Float.sin : Float -> Float
  113. Float.sinh : Float -> Float
  114. Float.sqrt : Float -> Float
  115. Float.tan : Float -> Float
  116. Float.tanh : Float -> Float
  117. Float.toRepresentation : Float -> Nat
  118. Float.toText : Float -> Text
  119. Float.truncate : Float -> Int
  120. Handle.toText : Handle -> Text
  121. builtin type Int
  122. Int.* : Int -> Int -> Int
  123. Int.+ : Int -> Int -> Int
  124. Int.- : Int -> Int -> Int
  125. Int./ : Int -> Int -> Int
  126. Int.and : Int -> Int -> Int
  127. Int.complement : Int -> Int
  128. Int.eq : Int -> Int -> Boolean
  129. Int.fromRepresentation : Nat -> Int
  130. Int.fromText : Text -> Optional Int
  131. Int.gt : Int -> Int -> Boolean
  132. Int.gteq : Int -> Int -> Boolean
  133. Int.increment : Int -> Int
  134. Int.isEven : Int -> Boolean
  135. Int.isOdd : Int -> Boolean
  136. Int.leadingZeros : Int -> Nat
  137. Int.lt : Int -> Int -> Boolean
  138. Int.lteq : Int -> Int -> Boolean
  139. Int.mod : Int -> Int -> Int
  140. Int.negate : Int -> Int
  141. Int.or : Int -> Int -> Int
  142. Int.popCount : Int -> Nat
  143. Int.pow : Int -> Nat -> Int
  144. Int.shiftLeft : Int -> Nat -> Int
  145. Int.shiftRight : Int -> Nat -> Int
  146. Int.signum : Int -> Int
  147. Int.toFloat : Int -> Float
  148. Int.toRepresentation : Int -> Nat
  149. Int.toText : Int -> Text
  150. Int.trailingZeros : Int -> Nat
  151. Int.truncate0 : Int -> Nat
  152. Int.xor : Int -> Int -> Int
  153. unique type io2.BufferMode
  154. io2.BufferMode.BlockBuffering : BufferMode
  155. io2.BufferMode.LineBuffering : BufferMode
  156. io2.BufferMode.NoBuffering : BufferMode
  157. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  158. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  159. io2.Clock.internals.nsec : TimeSpec -> Nat
  160. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  161. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  162. io2.Clock.internals.sec : TimeSpec -> Int
  163. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  164. builtin type io2.Clock.internals.TimeSpec
  165. unique type io2.Failure
  166. io2.Failure.Failure : Type -> Text -> Any -> Failure
  167. unique type io2.FileMode
  168. io2.FileMode.Append : FileMode
  169. io2.FileMode.Read : FileMode
  170. io2.FileMode.ReadWrite : FileMode
  171. io2.FileMode.Write : FileMode
  172. builtin type io2.Handle
  173. builtin type io2.IO
  174. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  175. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  176. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  177. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  178. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  179. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  180. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  181. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  182. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  183. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  184. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  185. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  186. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  187. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  188. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  189. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  190. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  191. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  192. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  193. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  194. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  195. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  196. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  197. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  198. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  199. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  200. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  201. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  202. io2.IO.ref : a ->{IO} Ref {IO} a
  203. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  204. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  205. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  206. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  207. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  208. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  209. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  210. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  211. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  212. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  213. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  214. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  215. io2.IO.stdHandle : StdHandle -> Handle
  216. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  217. io2.IO.systemTimeMicroseconds : '{IO} Int
  218. unique type io2.IOError
  219. io2.IOError.AlreadyExists : IOError
  220. io2.IOError.EOF : IOError
  221. io2.IOError.IllegalOperation : IOError
  222. io2.IOError.NoSuchThing : IOError
  223. io2.IOError.PermissionDenied : IOError
  224. io2.IOError.ResourceBusy : IOError
  225. io2.IOError.ResourceExhausted : IOError
  226. io2.IOError.UserError : IOError
  227. unique type io2.IOFailure
  228. builtin type io2.MVar
  229. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  230. io2.MVar.new : a ->{IO} MVar a
  231. io2.MVar.newEmpty : '{IO} MVar a
  232. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  233. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  234. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  235. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  236. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  237. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  238. io2.MVar.tryTake : MVar a ->{IO} Optional a
  239. unique type io2.SeekMode
  240. io2.SeekMode.AbsoluteSeek : SeekMode
  241. io2.SeekMode.RelativeSeek : SeekMode
  242. io2.SeekMode.SeekFromEnd : SeekMode
  243. builtin type io2.Socket
  244. unique type io2.StdHandle
  245. io2.StdHandle.StdErr : StdHandle
  246. io2.StdHandle.StdIn : StdHandle
  247. io2.StdHandle.StdOut : StdHandle
  248. builtin type io2.STM
  249. io2.STM.atomically : '{STM} a ->{IO} a
  250. io2.STM.retry : '{STM} a
  251. builtin type io2.ThreadId
  252. builtin type io2.Tls
  253. builtin type io2.Tls.Cipher
  254. builtin type io2.Tls.ClientConfig
  255. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  256. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  257. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  258. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  259. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  260. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  261. io2.Tls.encodeCert : SignedCert -> Bytes
  262. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  263. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  264. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  265. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  266. builtin type io2.Tls.PrivateKey
  267. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  268. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  269. builtin type io2.Tls.ServerConfig
  270. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  271. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  272. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  273. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  274. builtin type io2.Tls.SignedCert
  275. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  276. builtin type io2.Tls.Version
  277. unique type io2.TlsFailure
  278. builtin type io2.TVar
  279. io2.TVar.new : a ->{STM} TVar a
  280. io2.TVar.newIO : a ->{IO} TVar a
  281. io2.TVar.read : TVar a ->{STM} a
  282. io2.TVar.readIO : TVar a ->{IO} a
  283. io2.TVar.swap : TVar a -> a ->{STM} a
  284. io2.TVar.write : TVar a -> a ->{STM} ()
  285. io2.validateSandboxed : [Term] -> a -> Boolean
  286. unique type IsPropagated
  287. IsPropagated.IsPropagated : IsPropagated
  288. unique type IsTest
  289. IsTest.IsTest : IsTest
  290. unique type Link
  291. builtin type Link.Term
  292. Link.Term : Term -> Link
  293. Link.Term.toText : Term -> Text
  294. builtin type Link.Type
  295. Link.Type : Type -> Link
  296. builtin type List
  297. List.++ : [a] -> [a] -> [a]
  298. List.+: : a -> [a] -> [a]
  299. List.:+ : [a] -> a -> [a]
  300. List.at : Nat -> [a] -> Optional a
  301. List.cons : a -> [a] -> [a]
  302. List.drop : Nat -> [a] -> [a]
  303. List.empty : [a]
  304. List.size : [a] -> Nat
  305. List.snoc : [a] -> a -> [a]
  306. List.take : Nat -> [a] -> [a]
  307. metadata.isPropagated : IsPropagated
  308. metadata.isTest : IsTest
  309. builtin type Nat
  310. Nat.* : Nat -> Nat -> Nat
  311. Nat.+ : Nat -> Nat -> Nat
  312. Nat./ : Nat -> Nat -> Nat
  313. Nat.and : Nat -> Nat -> Nat
  314. Nat.complement : Nat -> Nat
  315. Nat.drop : Nat -> Nat -> Nat
  316. Nat.eq : Nat -> Nat -> Boolean
  317. Nat.fromText : Text -> Optional Nat
  318. Nat.gt : Nat -> Nat -> Boolean
  319. Nat.gteq : Nat -> Nat -> Boolean
  320. Nat.increment : Nat -> Nat
  321. Nat.isEven : Nat -> Boolean
  322. Nat.isOdd : Nat -> Boolean
  323. Nat.leadingZeros : Nat -> Nat
  324. Nat.lt : Nat -> Nat -> Boolean
  325. Nat.lteq : Nat -> Nat -> Boolean
  326. Nat.mod : Nat -> Nat -> Nat
  327. Nat.or : Nat -> Nat -> Nat
  328. Nat.popCount : Nat -> Nat
  329. Nat.pow : Nat -> Nat -> Nat
  330. Nat.shiftLeft : Nat -> Nat -> Nat
  331. Nat.shiftRight : Nat -> Nat -> Nat
  332. Nat.sub : Nat -> Nat -> Int
  333. Nat.toFloat : Nat -> Float
  334. Nat.toInt : Nat -> Int
  335. Nat.toText : Nat -> Text
  336. Nat.trailingZeros : Nat -> Nat
  337. Nat.xor : Nat -> Nat -> Nat
  338. structural type Optional a
  339. Optional.None : Optional a
  340. Optional.Some : a -> Optional a
  341. builtin type Ref
  342. Ref.read : Ref g a ->{g} a
  343. Ref.write : Ref g a -> a ->{g} ()
  344. builtin type Request
  345. builtin type Scope
  346. Scope.ref : a ->{Scope s} Ref {Scope s} a
  347. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  348. structural type SeqView a b
  349. SeqView.VElem : a -> b -> SeqView a b
  350. SeqView.VEmpty : SeqView a b
  351. Socket.toText : Socket -> Text
  352. unique type Test.Result
  353. Test.Result.Fail : Text -> Result
  354. Test.Result.Ok : Text -> Result
  355. builtin type Text
  356. Text.!= : Text -> Text -> Boolean
  357. Text.++ : Text -> Text -> Text
  358. Text.drop : Nat -> Text -> Text
  359. Text.empty : Text
  360. Text.eq : Text -> Text -> Boolean
  361. Text.fromCharList : [Char] -> Text
  362. Text.fromUtf8.impl : Bytes -> Either Failure Text
  363. Text.gt : Text -> Text -> Boolean
  364. Text.gteq : Text -> Text -> Boolean
  365. Text.lt : Text -> Text -> Boolean
  366. Text.lteq : Text -> Text -> Boolean
  367. Text.repeat : Nat -> Text -> Text
  368. Text.size : Text -> Nat
  369. Text.take : Nat -> Text -> Text
  370. Text.toCharList : Text -> [Char]
  371. Text.toUtf8 : Text -> Bytes
  372. Text.uncons : Text -> Optional (Char, Text)
  373. Text.unsnoc : Text -> Optional (Text, Char)
  374. ThreadId.toText : ThreadId -> Text
  375. todo : a -> b
  376. structural type Tuple a b
  377. Tuple.Cons : a -> b -> Tuple a b
  378. structural type Unit
  379. Unit.Unit : ()
  380. Universal.< : a -> a -> Boolean
  381. Universal.<= : a -> a -> Boolean
  382. Universal.== : a -> a -> Boolean
  383. Universal.> : a -> a -> Boolean
  384. Universal.>= : a -> a -> Boolean
  385. Universal.compare : a -> a -> Int
  386. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  387. builtin type Value
  388. Value.dependencies : Value -> [Term]
  389. Value.deserialize : Bytes -> Either Text Value
  390. Value.load : Value ->{IO} Either [Term] a
  391. Value.serialize : Value -> Bytes
  392. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.ceiling            : Float -> Int
    2.  Float.cos                : Float -> Float
    3.  Float.cosh               : Float -> Float
    4.  Float.eq                 : Float -> Float -> Boolean
    5.  Float.exp                : Float -> Float
    6.  Float.floor              : Float -> Int
    7.  Float.fromRepresentation : Nat -> Float
    8.  Float.fromText           : Text -> Optional Float
    9.  Float.gt                 : Float -> Float -> Boolean
    10. Float.gteq               : Float -> Float -> Boolean
    11. Float.log                : Float -> Float
  
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

  1.  Float.ceiling : Float -> Int
  2.  Float.cos : Float -> Float
  3.  Float.cosh : Float -> Float
  4.  Float.eq : Float -> Float -> Boolean
  5.  Float.exp : Float -> Float
  6.  Float.floor : Float -> Int
  7.  Float.fromRepresentation : Nat -> Float
  8.  Float.fromText : Text -> Optional Float
  9.  Float.gt : Float -> Float -> Boolean
  10. Float.gteq : Float -> Float -> Boolean
  11. Float.log : Float -> Float
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
