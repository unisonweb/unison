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
  118. builtin type Int
  119. Int.* : Int -> Int -> Int
  120. Int.+ : Int -> Int -> Int
  121. Int.- : Int -> Int -> Int
  122. Int./ : Int -> Int -> Int
  123. Int.and : Int -> Int -> Int
  124. Int.complement : Int -> Int
  125. Int.eq : Int -> Int -> Boolean
  126. Int.fromRepresentation : Nat -> Int
  127. Int.fromText : Text -> Optional Int
  128. Int.gt : Int -> Int -> Boolean
  129. Int.gteq : Int -> Int -> Boolean
  130. Int.increment : Int -> Int
  131. Int.isEven : Int -> Boolean
  132. Int.isOdd : Int -> Boolean
  133. Int.leadingZeros : Int -> Nat
  134. Int.lt : Int -> Int -> Boolean
  135. Int.lteq : Int -> Int -> Boolean
  136. Int.mod : Int -> Int -> Int
  137. Int.negate : Int -> Int
  138. Int.or : Int -> Int -> Int
  139. Int.popCount : Int -> Nat
  140. Int.pow : Int -> Nat -> Int
  141. Int.shiftLeft : Int -> Nat -> Int
  142. Int.shiftRight : Int -> Nat -> Int
  143. Int.signum : Int -> Int
  144. Int.toFloat : Int -> Float
  145. Int.toRepresentation : Int -> Nat
  146. Int.toText : Int -> Text
  147. Int.trailingZeros : Int -> Nat
  148. Int.truncate0 : Int -> Nat
  149. Int.xor : Int -> Int -> Int
  150. unique type io2.BufferMode
  151. io2.BufferMode.BlockBuffering : BufferMode
  152. io2.BufferMode.LineBuffering : BufferMode
  153. io2.BufferMode.NoBuffering : BufferMode
  154. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  155. unique type io2.Failure
  156. io2.Failure.Failure : Type -> Text -> Any -> Failure
  157. unique type io2.FileMode
  158. io2.FileMode.Append : FileMode
  159. io2.FileMode.Read : FileMode
  160. io2.FileMode.ReadWrite : FileMode
  161. io2.FileMode.Write : FileMode
  162. builtin type io2.Handle
  163. builtin type io2.IO
  164. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  165. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  166. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  167. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  168. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  169. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  170. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  171. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  172. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  173. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  174. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  175. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  176. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  177. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  178. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  179. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  180. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  181. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  182. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  183. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  184. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  185. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  186. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  187. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  188. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  189. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  190. io2.IO.ref : a ->{IO} Ref {IO} a
  191. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  192. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  193. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  194. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  195. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  196. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  197. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  198. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  199. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  200. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  201. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  202. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  203. io2.IO.stdHandle : StdHandle -> Handle
  204. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  205. io2.IO.systemTimeMicroseconds : '{IO} Int
  206. unique type io2.IOError
  207. io2.IOError.AlreadyExists : IOError
  208. io2.IOError.EOF : IOError
  209. io2.IOError.IllegalOperation : IOError
  210. io2.IOError.NoSuchThing : IOError
  211. io2.IOError.PermissionDenied : IOError
  212. io2.IOError.ResourceBusy : IOError
  213. io2.IOError.ResourceExhausted : IOError
  214. io2.IOError.UserError : IOError
  215. unique type io2.IOFailure
  216. builtin type io2.MVar
  217. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  218. io2.MVar.new : a ->{IO} MVar a
  219. io2.MVar.newEmpty : '{IO} MVar a
  220. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  221. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  222. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  223. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  224. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  225. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  226. io2.MVar.tryTake : MVar a ->{IO} Optional a
  227. unique type io2.SeekMode
  228. io2.SeekMode.AbsoluteSeek : SeekMode
  229. io2.SeekMode.RelativeSeek : SeekMode
  230. io2.SeekMode.SeekFromEnd : SeekMode
  231. builtin type io2.Socket
  232. unique type io2.StdHandle
  233. io2.StdHandle.StdErr : StdHandle
  234. io2.StdHandle.StdIn : StdHandle
  235. io2.StdHandle.StdOut : StdHandle
  236. builtin type io2.STM
  237. io2.STM.atomically : '{STM} a ->{IO} a
  238. io2.STM.retry : '{STM} a
  239. builtin type io2.ThreadId
  240. builtin type io2.Tls
  241. builtin type io2.Tls.Cipher
  242. builtin type io2.Tls.ClientConfig
  243. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  244. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  245. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  246. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  247. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  248. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  249. io2.Tls.encodeCert : SignedCert -> Bytes
  250. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  251. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  252. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  253. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  254. builtin type io2.Tls.PrivateKey
  255. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  256. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  257. builtin type io2.Tls.ServerConfig
  258. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  259. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  260. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  261. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  262. builtin type io2.Tls.SignedCert
  263. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  264. builtin type io2.Tls.Version
  265. unique type io2.TlsFailure
  266. builtin type io2.TVar
  267. io2.TVar.new : a ->{STM} TVar a
  268. io2.TVar.newIO : a ->{IO} TVar a
  269. io2.TVar.read : TVar a ->{STM} a
  270. io2.TVar.readIO : TVar a ->{IO} a
  271. io2.TVar.swap : TVar a -> a ->{STM} a
  272. io2.TVar.write : TVar a -> a ->{STM} ()
  273. unique type IsPropagated
  274. IsPropagated.IsPropagated : IsPropagated
  275. unique type IsTest
  276. IsTest.IsTest : IsTest
  277. unique type Link
  278. builtin type Link.Term
  279. Link.Term : Term -> Link
  280. Link.Term.toText : Term -> Text
  281. builtin type Link.Type
  282. Link.Type : Type -> Link
  283. builtin type List
  284. List.++ : [a] -> [a] -> [a]
  285. List.+: : a -> [a] -> [a]
  286. List.:+ : [a] -> a -> [a]
  287. List.at : Nat -> [a] -> Optional a
  288. List.cons : a -> [a] -> [a]
  289. List.drop : Nat -> [a] -> [a]
  290. List.empty : [a]
  291. List.size : [a] -> Nat
  292. List.snoc : [a] -> a -> [a]
  293. List.take : Nat -> [a] -> [a]
  294. metadata.isPropagated : IsPropagated
  295. metadata.isTest : IsTest
  296. builtin type Nat
  297. Nat.* : Nat -> Nat -> Nat
  298. Nat.+ : Nat -> Nat -> Nat
  299. Nat./ : Nat -> Nat -> Nat
  300. Nat.and : Nat -> Nat -> Nat
  301. Nat.complement : Nat -> Nat
  302. Nat.drop : Nat -> Nat -> Nat
  303. Nat.eq : Nat -> Nat -> Boolean
  304. Nat.fromText : Text -> Optional Nat
  305. Nat.gt : Nat -> Nat -> Boolean
  306. Nat.gteq : Nat -> Nat -> Boolean
  307. Nat.increment : Nat -> Nat
  308. Nat.isEven : Nat -> Boolean
  309. Nat.isOdd : Nat -> Boolean
  310. Nat.leadingZeros : Nat -> Nat
  311. Nat.lt : Nat -> Nat -> Boolean
  312. Nat.lteq : Nat -> Nat -> Boolean
  313. Nat.mod : Nat -> Nat -> Nat
  314. Nat.or : Nat -> Nat -> Nat
  315. Nat.popCount : Nat -> Nat
  316. Nat.pow : Nat -> Nat -> Nat
  317. Nat.shiftLeft : Nat -> Nat -> Nat
  318. Nat.shiftRight : Nat -> Nat -> Nat
  319. Nat.sub : Nat -> Nat -> Int
  320. Nat.toFloat : Nat -> Float
  321. Nat.toInt : Nat -> Int
  322. Nat.toText : Nat -> Text
  323. Nat.trailingZeros : Nat -> Nat
  324. Nat.xor : Nat -> Nat -> Nat
  325. structural type Optional a
  326. Optional.None : Optional a
  327. Optional.Some : a -> Optional a
  328. builtin type Ref
  329. Ref.read : Ref g a ->{g} a
  330. Ref.write : Ref g a -> a ->{g} ()
  331. builtin type Request
  332. builtin type Scope
  333. Scope.ref : a ->{Scope s} Ref {Scope s} a
  334. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  335. structural type SeqView a b
  336. SeqView.VElem : a -> b -> SeqView a b
  337. SeqView.VEmpty : SeqView a b
  338. unique type Test.Result
  339. Test.Result.Fail : Text -> Result
  340. Test.Result.Ok : Text -> Result
  341. builtin type Text
  342. Text.!= : Text -> Text -> Boolean
  343. Text.++ : Text -> Text -> Text
  344. Text.drop : Nat -> Text -> Text
  345. Text.empty : Text
  346. Text.eq : Text -> Text -> Boolean
  347. Text.fromCharList : [Char] -> Text
  348. Text.fromUtf8.impl : Bytes -> Either Failure Text
  349. Text.gt : Text -> Text -> Boolean
  350. Text.gteq : Text -> Text -> Boolean
  351. Text.lt : Text -> Text -> Boolean
  352. Text.lteq : Text -> Text -> Boolean
  353. Text.repeat : Nat -> Text -> Text
  354. Text.size : Text -> Nat
  355. Text.take : Nat -> Text -> Text
  356. Text.toCharList : Text -> [Char]
  357. Text.toUtf8 : Text -> Bytes
  358. Text.uncons : Text -> Optional (Char, Text)
  359. Text.unsnoc : Text -> Optional (Text, Char)
  360. todo : a -> b
  361. structural type Tuple a b
  362. Tuple.Cons : a -> b -> Tuple a b
  363. structural type Unit
  364. Unit.Unit : ()
  365. Universal.< : a -> a -> Boolean
  366. Universal.<= : a -> a -> Boolean
  367. Universal.== : a -> a -> Boolean
  368. Universal.> : a -> a -> Boolean
  369. Universal.>= : a -> a -> Boolean
  370. Universal.compare : a -> a -> Int
  371. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  372. builtin type Value
  373. Value.dependencies : Value -> [Term]
  374. Value.deserialize : Bytes -> Either Text Value
  375. Value.load : Value ->{IO} Either [Term] a
  376. Value.serialize : Value -> Bytes
  377. Value.value : a -> Value
  

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
