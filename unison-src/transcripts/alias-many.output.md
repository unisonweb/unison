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
  66.  Debug.trace : Text -> a -> ()
  67.  Debug.watch : Text -> a -> a
  68.  unique type Doc
  69.  Doc.Blob : Text -> Doc
  70.  Doc.Evaluate : Term -> Doc
  71.  Doc.Join : [Doc] -> Doc
  72.  Doc.Link : Link -> Doc
  73.  Doc.Signature : Term -> Doc
  74.  Doc.Source : Link -> Doc
  75.  structural type Either a b
  76.  Either.Left : a -> Either a b
  77.  Either.Right : b -> Either a b
  78.  structural ability Exception
  79.  Exception.raise : Failure ->{Exception} x
  80.  builtin type Float
  81.  Float.* : Float -> Float -> Float
  82.  Float.+ : Float -> Float -> Float
  83.  Float.- : Float -> Float -> Float
  84.  Float./ : Float -> Float -> Float
  85.  Float.abs : Float -> Float
  86.  Float.acos : Float -> Float
  87.  Float.acosh : Float -> Float
  88.  Float.asin : Float -> Float
  89.  Float.asinh : Float -> Float
  90.  Float.atan : Float -> Float
  91.  Float.atan2 : Float -> Float -> Float
  92.  Float.atanh : Float -> Float
  93.  Float.ceiling : Float -> Int
  94.  Float.cos : Float -> Float
  95.  Float.cosh : Float -> Float
  96.  Float.eq : Float -> Float -> Boolean
  97.  Float.exp : Float -> Float
  98.  Float.floor : Float -> Int
  99.  Float.fromRepresentation : Nat -> Float
  100. Float.fromText : Text -> Optional Float
  101. Float.gt : Float -> Float -> Boolean
  102. Float.gteq : Float -> Float -> Boolean
  103. Float.log : Float -> Float
  104. Float.logBase : Float -> Float -> Float
  105. Float.lt : Float -> Float -> Boolean
  106. Float.lteq : Float -> Float -> Boolean
  107. Float.max : Float -> Float -> Float
  108. Float.min : Float -> Float -> Float
  109. Float.pow : Float -> Float -> Float
  110. Float.round : Float -> Int
  111. Float.sin : Float -> Float
  112. Float.sinh : Float -> Float
  113. Float.sqrt : Float -> Float
  114. Float.tan : Float -> Float
  115. Float.tanh : Float -> Float
  116. Float.toRepresentation : Float -> Nat
  117. Float.toText : Float -> Text
  118. Float.truncate : Float -> Int
  119. Handle.toText : Handle -> Text
  120. builtin type Int
  121. Int.* : Int -> Int -> Int
  122. Int.+ : Int -> Int -> Int
  123. Int.- : Int -> Int -> Int
  124. Int./ : Int -> Int -> Int
  125. Int.and : Int -> Int -> Int
  126. Int.complement : Int -> Int
  127. Int.eq : Int -> Int -> Boolean
  128. Int.fromRepresentation : Nat -> Int
  129. Int.fromText : Text -> Optional Int
  130. Int.gt : Int -> Int -> Boolean
  131. Int.gteq : Int -> Int -> Boolean
  132. Int.increment : Int -> Int
  133. Int.isEven : Int -> Boolean
  134. Int.isOdd : Int -> Boolean
  135. Int.leadingZeros : Int -> Nat
  136. Int.lt : Int -> Int -> Boolean
  137. Int.lteq : Int -> Int -> Boolean
  138. Int.mod : Int -> Int -> Int
  139. Int.negate : Int -> Int
  140. Int.or : Int -> Int -> Int
  141. Int.popCount : Int -> Nat
  142. Int.pow : Int -> Nat -> Int
  143. Int.shiftLeft : Int -> Nat -> Int
  144. Int.shiftRight : Int -> Nat -> Int
  145. Int.signum : Int -> Int
  146. Int.toFloat : Int -> Float
  147. Int.toRepresentation : Int -> Nat
  148. Int.toText : Int -> Text
  149. Int.trailingZeros : Int -> Nat
  150. Int.truncate0 : Int -> Nat
  151. Int.xor : Int -> Int -> Int
  152. unique type io2.BufferMode
  153. io2.BufferMode.BlockBuffering : BufferMode
  154. io2.BufferMode.LineBuffering : BufferMode
  155. io2.BufferMode.NoBuffering : BufferMode
  156. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  157. unique type io2.Failure
  158. io2.Failure.Failure : Type -> Text -> Any -> Failure
  159. unique type io2.FileMode
  160. io2.FileMode.Append : FileMode
  161. io2.FileMode.Read : FileMode
  162. io2.FileMode.ReadWrite : FileMode
  163. io2.FileMode.Write : FileMode
  164. builtin type io2.Handle
  165. builtin type io2.IO
  166. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  167. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  168. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  169. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  170. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  171. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  172. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  173. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  174. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  175. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  176. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  177. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  178. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  179. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  180. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  181. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  182. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  183. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  184. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  185. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  186. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  187. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  188. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  189. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  190. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  191. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  192. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  193. io2.IO.ref : a ->{IO} Ref {IO} a
  194. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  195. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  196. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  197. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  198. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  199. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  200. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  201. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  202. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  203. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  204. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  205. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  206. io2.IO.stdHandle : StdHandle -> Handle
  207. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  208. io2.IO.systemTimeMicroseconds : '{IO} Int
  209. unique type io2.IOError
  210. io2.IOError.AlreadyExists : IOError
  211. io2.IOError.EOF : IOError
  212. io2.IOError.IllegalOperation : IOError
  213. io2.IOError.NoSuchThing : IOError
  214. io2.IOError.PermissionDenied : IOError
  215. io2.IOError.ResourceBusy : IOError
  216. io2.IOError.ResourceExhausted : IOError
  217. io2.IOError.UserError : IOError
  218. unique type io2.IOFailure
  219. builtin type io2.MVar
  220. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  221. io2.MVar.new : a ->{IO} MVar a
  222. io2.MVar.newEmpty : '{IO} MVar a
  223. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  224. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  225. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  226. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  227. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  228. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  229. io2.MVar.tryTake : MVar a ->{IO} Optional a
  230. unique type io2.SeekMode
  231. io2.SeekMode.AbsoluteSeek : SeekMode
  232. io2.SeekMode.RelativeSeek : SeekMode
  233. io2.SeekMode.SeekFromEnd : SeekMode
  234. builtin type io2.Socket
  235. unique type io2.StdHandle
  236. io2.StdHandle.StdErr : StdHandle
  237. io2.StdHandle.StdIn : StdHandle
  238. io2.StdHandle.StdOut : StdHandle
  239. builtin type io2.STM
  240. io2.STM.atomically : '{STM} a ->{IO} a
  241. io2.STM.retry : '{STM} a
  242. builtin type io2.ThreadId
  243. builtin type io2.Tls
  244. builtin type io2.Tls.Cipher
  245. builtin type io2.Tls.ClientConfig
  246. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  247. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  248. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  249. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  250. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  251. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  252. io2.Tls.encodeCert : SignedCert -> Bytes
  253. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  254. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  255. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  256. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  257. builtin type io2.Tls.PrivateKey
  258. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  259. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  260. builtin type io2.Tls.ServerConfig
  261. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  262. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  263. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  264. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  265. builtin type io2.Tls.SignedCert
  266. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  267. builtin type io2.Tls.Version
  268. unique type io2.TlsFailure
  269. builtin type io2.TVar
  270. io2.TVar.new : a ->{STM} TVar a
  271. io2.TVar.newIO : a ->{IO} TVar a
  272. io2.TVar.read : TVar a ->{STM} a
  273. io2.TVar.readIO : TVar a ->{IO} a
  274. io2.TVar.swap : TVar a -> a ->{STM} a
  275. io2.TVar.write : TVar a -> a ->{STM} ()
  276. unique type IsPropagated
  277. IsPropagated.IsPropagated : IsPropagated
  278. unique type IsTest
  279. IsTest.IsTest : IsTest
  280. unique type Link
  281. builtin type Link.Term
  282. Link.Term : Term -> Link
  283. Link.Term.toText : Term -> Text
  284. builtin type Link.Type
  285. Link.Type : Type -> Link
  286. builtin type List
  287. List.++ : [a] -> [a] -> [a]
  288. List.+: : a -> [a] -> [a]
  289. List.:+ : [a] -> a -> [a]
  290. List.at : Nat -> [a] -> Optional a
  291. List.cons : a -> [a] -> [a]
  292. List.drop : Nat -> [a] -> [a]
  293. List.empty : [a]
  294. List.size : [a] -> Nat
  295. List.snoc : [a] -> a -> [a]
  296. List.take : Nat -> [a] -> [a]
  297. metadata.isPropagated : IsPropagated
  298. metadata.isTest : IsTest
  299. builtin type Nat
  300. Nat.* : Nat -> Nat -> Nat
  301. Nat.+ : Nat -> Nat -> Nat
  302. Nat./ : Nat -> Nat -> Nat
  303. Nat.and : Nat -> Nat -> Nat
  304. Nat.complement : Nat -> Nat
  305. Nat.drop : Nat -> Nat -> Nat
  306. Nat.eq : Nat -> Nat -> Boolean
  307. Nat.fromText : Text -> Optional Nat
  308. Nat.gt : Nat -> Nat -> Boolean
  309. Nat.gteq : Nat -> Nat -> Boolean
  310. Nat.increment : Nat -> Nat
  311. Nat.isEven : Nat -> Boolean
  312. Nat.isOdd : Nat -> Boolean
  313. Nat.leadingZeros : Nat -> Nat
  314. Nat.lt : Nat -> Nat -> Boolean
  315. Nat.lteq : Nat -> Nat -> Boolean
  316. Nat.mod : Nat -> Nat -> Nat
  317. Nat.or : Nat -> Nat -> Nat
  318. Nat.popCount : Nat -> Nat
  319. Nat.pow : Nat -> Nat -> Nat
  320. Nat.shiftLeft : Nat -> Nat -> Nat
  321. Nat.shiftRight : Nat -> Nat -> Nat
  322. Nat.sub : Nat -> Nat -> Int
  323. Nat.toFloat : Nat -> Float
  324. Nat.toInt : Nat -> Int
  325. Nat.toText : Nat -> Text
  326. Nat.trailingZeros : Nat -> Nat
  327. Nat.xor : Nat -> Nat -> Nat
  328. structural type Optional a
  329. Optional.None : Optional a
  330. Optional.Some : a -> Optional a
  331. builtin type Ref
  332. Ref.read : Ref g a ->{g} a
  333. Ref.write : Ref g a -> a ->{g} ()
  334. builtin type Request
  335. builtin type Scope
  336. Scope.ref : a ->{Scope s} Ref {Scope s} a
  337. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  338. structural type SeqView a b
  339. SeqView.VElem : a -> b -> SeqView a b
  340. SeqView.VEmpty : SeqView a b
  341. Socket.toText : Socket -> Text
  342. unique type Test.Result
  343. Test.Result.Fail : Text -> Result
  344. Test.Result.Ok : Text -> Result
  345. builtin type Text
  346. Text.!= : Text -> Text -> Boolean
  347. Text.++ : Text -> Text -> Text
  348. Text.drop : Nat -> Text -> Text
  349. Text.empty : Text
  350. Text.eq : Text -> Text -> Boolean
  351. Text.fromCharList : [Char] -> Text
  352. Text.fromUtf8.impl : Bytes -> Either Failure Text
  353. Text.gt : Text -> Text -> Boolean
  354. Text.gteq : Text -> Text -> Boolean
  355. Text.lt : Text -> Text -> Boolean
  356. Text.lteq : Text -> Text -> Boolean
  357. Text.repeat : Nat -> Text -> Text
  358. Text.size : Text -> Nat
  359. Text.take : Nat -> Text -> Text
  360. Text.toCharList : Text -> [Char]
  361. Text.toUtf8 : Text -> Bytes
  362. Text.uncons : Text -> Optional (Char, Text)
  363. Text.unsnoc : Text -> Optional (Text, Char)
  364. ThreadId.toText : ThreadId -> Text
  365. todo : a -> b
  366. structural type Tuple a b
  367. Tuple.Cons : a -> b -> Tuple a b
  368. structural type Unit
  369. Unit.Unit : ()
  370. Universal.< : a -> a -> Boolean
  371. Universal.<= : a -> a -> Boolean
  372. Universal.== : a -> a -> Boolean
  373. Universal.> : a -> a -> Boolean
  374. Universal.>= : a -> a -> Boolean
  375. Universal.compare : a -> a -> Int
  376. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  377. builtin type Value
  378. Value.dependencies : Value -> [Term]
  379. Value.deserialize : Bytes -> Either Text Value
  380. Value.load : Value ->{IO} Either [Term] a
  381. Value.serialize : Value -> Bytes
  382. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.cos                : Float -> Float
    2.  Float.cosh               : Float -> Float
    3.  Float.eq                 : Float -> Float -> Boolean
    4.  Float.exp                : Float -> Float
    5.  Float.floor              : Float -> Int
    6.  Float.fromRepresentation : Nat -> Float
    7.  Float.fromText           : Text -> Optional Float
    8.  Float.gt                 : Float -> Float -> Boolean
    9.  Float.gteq               : Float -> Float -> Boolean
    10. Float.log                : Float -> Float
    11. Float.logBase            : Float -> Float -> Float
  
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

  1.  Float.cos : Float -> Float
  2.  Float.cosh : Float -> Float
  3.  Float.eq : Float -> Float -> Boolean
  4.  Float.exp : Float -> Float
  5.  Float.floor : Float -> Int
  6.  Float.fromRepresentation : Nat -> Float
  7.  Float.fromText : Text -> Optional Float
  8.  Float.gt : Float -> Float -> Boolean
  9.  Float.gteq : Float -> Float -> Boolean
  10. Float.log : Float -> Float
  11. Float.logBase : Float -> Float -> Float
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
