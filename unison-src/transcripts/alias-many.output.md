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
  30.  Bytes.size : Bytes -> Nat
  31.  Bytes.take : Nat -> Bytes -> Bytes
  32.  Bytes.toBase16 : Bytes -> Bytes
  33.  Bytes.toBase32 : Bytes -> Bytes
  34.  Bytes.toBase64 : Bytes -> Bytes
  35.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  36.  Bytes.toList : Bytes -> [Nat]
  37.  builtin type Char
  38.  Char.fromNat : Nat -> Char
  39.  Char.toNat : Char -> Nat
  40.  Char.toText : Char -> Text
  41.  builtin type Code
  42.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  43.  Code.dependencies : Code -> [Term]
  44.  Code.deserialize : Bytes -> Either Text Code
  45.  Code.isMissing : Term ->{IO} Boolean
  46.  Code.lookup : Term ->{IO} Optional Code
  47.  Code.serialize : Code -> Bytes
  48.  crypto.hash : HashAlgorithm -> a -> Bytes
  49.  builtin type crypto.HashAlgorithm
  50.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  51.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  52.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  53.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  54.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  55.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  56.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  57.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  58.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  59.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  60.  Debug.watch : Text -> a -> a
  61.  unique type Doc
  62.  Doc.Blob : Text -> Doc
  63.  Doc.Evaluate : Term -> Doc
  64.  Doc.Join : [Doc] -> Doc
  65.  Doc.Link : Link -> Doc
  66.  Doc.Signature : Term -> Doc
  67.  Doc.Source : Link -> Doc
  68.  structural type Either a b
  69.  Either.Left : a -> Either a b
  70.  Either.Right : b -> Either a b
  71.  structural ability Exception
  72.  Exception.raise : Failure ->{Exception} x
  73.  builtin type Float
  74.  Float.* : Float -> Float -> Float
  75.  Float.+ : Float -> Float -> Float
  76.  Float.- : Float -> Float -> Float
  77.  Float./ : Float -> Float -> Float
  78.  Float.abs : Float -> Float
  79.  Float.acos : Float -> Float
  80.  Float.acosh : Float -> Float
  81.  Float.asin : Float -> Float
  82.  Float.asinh : Float -> Float
  83.  Float.atan : Float -> Float
  84.  Float.atan2 : Float -> Float -> Float
  85.  Float.atanh : Float -> Float
  86.  Float.ceiling : Float -> Int
  87.  Float.cos : Float -> Float
  88.  Float.cosh : Float -> Float
  89.  Float.eq : Float -> Float -> Boolean
  90.  Float.exp : Float -> Float
  91.  Float.floor : Float -> Int
  92.  Float.fromRepresentation : Nat -> Float
  93.  Float.fromText : Text -> Optional Float
  94.  Float.gt : Float -> Float -> Boolean
  95.  Float.gteq : Float -> Float -> Boolean
  96.  Float.log : Float -> Float
  97.  Float.logBase : Float -> Float -> Float
  98.  Float.lt : Float -> Float -> Boolean
  99.  Float.lteq : Float -> Float -> Boolean
  100. Float.max : Float -> Float -> Float
  101. Float.min : Float -> Float -> Float
  102. Float.pow : Float -> Float -> Float
  103. Float.round : Float -> Int
  104. Float.sin : Float -> Float
  105. Float.sinh : Float -> Float
  106. Float.sqrt : Float -> Float
  107. Float.tan : Float -> Float
  108. Float.tanh : Float -> Float
  109. Float.toRepresentation : Float -> Nat
  110. Float.toText : Float -> Text
  111. Float.truncate : Float -> Int
  112. builtin type Int
  113. Int.* : Int -> Int -> Int
  114. Int.+ : Int -> Int -> Int
  115. Int.- : Int -> Int -> Int
  116. Int./ : Int -> Int -> Int
  117. Int.and : Int -> Int -> Int
  118. Int.complement : Int -> Int
  119. Int.eq : Int -> Int -> Boolean
  120. Int.fromRepresentation : Nat -> Int
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
  139. Int.toRepresentation : Int -> Nat
  140. Int.toText : Int -> Text
  141. Int.trailingZeros : Int -> Nat
  142. Int.truncate0 : Int -> Nat
  143. Int.xor : Int -> Int -> Int
  144. unique type io2.BufferMode
  145. io2.BufferMode.BlockBuffering : BufferMode
  146. io2.BufferMode.LineBuffering : BufferMode
  147. io2.BufferMode.NoBuffering : BufferMode
  148. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  149. unique type io2.Failure
  150. io2.Failure.Failure : Type -> Text -> Any -> Failure
  151. unique type io2.FileMode
  152. io2.FileMode.Append : FileMode
  153. io2.FileMode.Read : FileMode
  154. io2.FileMode.ReadWrite : FileMode
  155. io2.FileMode.Write : FileMode
  156. builtin type io2.Handle
  157. builtin type io2.IO
  158. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  159. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  160. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  161. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  162. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  163. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  164. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  165. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  166. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  167. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  168. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  169. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  170. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  171. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  172. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  173. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  174. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  175. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  176. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  177. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  178. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  179. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  180. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  181. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  182. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  183. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  184. io2.IO.ref : a ->{IO} Ref {IO} a
  185. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  186. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  187. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  188. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  189. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  190. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  191. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  192. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  193. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  194. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  195. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  196. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  197. io2.IO.stdHandle : StdHandle -> Handle
  198. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  199. unique type io2.IOError
  200. io2.IOError.AlreadyExists : IOError
  201. io2.IOError.EOF : IOError
  202. io2.IOError.IllegalOperation : IOError
  203. io2.IOError.NoSuchThing : IOError
  204. io2.IOError.PermissionDenied : IOError
  205. io2.IOError.ResourceBusy : IOError
  206. io2.IOError.ResourceExhausted : IOError
  207. io2.IOError.UserError : IOError
  208. unique type io2.IOFailure
  209. builtin type io2.MVar
  210. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  211. io2.MVar.new : a ->{IO} MVar a
  212. io2.MVar.newEmpty : '{IO} MVar a
  213. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  214. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  215. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  216. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  217. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  218. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  219. io2.MVar.tryTake : MVar a ->{IO} Optional a
  220. unique type io2.SeekMode
  221. io2.SeekMode.AbsoluteSeek : SeekMode
  222. io2.SeekMode.RelativeSeek : SeekMode
  223. io2.SeekMode.SeekFromEnd : SeekMode
  224. builtin type io2.Socket
  225. unique type io2.StdHandle
  226. io2.StdHandle.StdErr : StdHandle
  227. io2.StdHandle.StdIn : StdHandle
  228. io2.StdHandle.StdOut : StdHandle
  229. builtin type io2.STM
  230. io2.STM.atomically : '{STM} a ->{IO} a
  231. io2.STM.retry : '{STM} a
  232. builtin type io2.ThreadId
  233. builtin type io2.Tls
  234. builtin type io2.Tls.Cipher
  235. builtin type io2.Tls.ClientConfig
  236. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  237. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  238. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  239. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  240. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  241. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  242. io2.Tls.encodeCert : SignedCert -> Bytes
  243. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  244. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  245. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  246. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  247. builtin type io2.Tls.PrivateKey
  248. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  249. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  250. builtin type io2.Tls.ServerConfig
  251. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  252. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  253. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  254. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  255. builtin type io2.Tls.SignedCert
  256. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  257. builtin type io2.Tls.Version
  258. unique type io2.TlsFailure
  259. builtin type io2.TVar
  260. io2.TVar.new : a ->{STM} TVar a
  261. io2.TVar.newIO : a ->{IO} TVar a
  262. io2.TVar.read : TVar a ->{STM} a
  263. io2.TVar.readIO : TVar a ->{IO} a
  264. io2.TVar.swap : TVar a -> a ->{STM} a
  265. io2.TVar.write : TVar a -> a ->{STM} ()
  266. unique type IsPropagated
  267. IsPropagated.IsPropagated : IsPropagated
  268. unique type IsTest
  269. IsTest.IsTest : IsTest
  270. unique type Link
  271. builtin type Link.Term
  272. Link.Term : Term -> Link
  273. builtin type Link.Type
  274. Link.Type : Type -> Link
  275. builtin type List
  276. List.++ : [a] -> [a] -> [a]
  277. List.+: : a -> [a] -> [a]
  278. List.:+ : [a] -> a -> [a]
  279. List.at : Nat -> [a] -> Optional a
  280. List.cons : a -> [a] -> [a]
  281. List.drop : Nat -> [a] -> [a]
  282. List.empty : [a]
  283. List.size : [a] -> Nat
  284. List.snoc : [a] -> a -> [a]
  285. List.take : Nat -> [a] -> [a]
  286. metadata.isPropagated : IsPropagated
  287. metadata.isTest : IsTest
  288. builtin type Nat
  289. Nat.* : Nat -> Nat -> Nat
  290. Nat.+ : Nat -> Nat -> Nat
  291. Nat./ : Nat -> Nat -> Nat
  292. Nat.and : Nat -> Nat -> Nat
  293. Nat.complement : Nat -> Nat
  294. Nat.drop : Nat -> Nat -> Nat
  295. Nat.eq : Nat -> Nat -> Boolean
  296. Nat.fromText : Text -> Optional Nat
  297. Nat.gt : Nat -> Nat -> Boolean
  298. Nat.gteq : Nat -> Nat -> Boolean
  299. Nat.increment : Nat -> Nat
  300. Nat.isEven : Nat -> Boolean
  301. Nat.isOdd : Nat -> Boolean
  302. Nat.leadingZeros : Nat -> Nat
  303. Nat.lt : Nat -> Nat -> Boolean
  304. Nat.lteq : Nat -> Nat -> Boolean
  305. Nat.mod : Nat -> Nat -> Nat
  306. Nat.or : Nat -> Nat -> Nat
  307. Nat.popCount : Nat -> Nat
  308. Nat.pow : Nat -> Nat -> Nat
  309. Nat.shiftLeft : Nat -> Nat -> Nat
  310. Nat.shiftRight : Nat -> Nat -> Nat
  311. Nat.sub : Nat -> Nat -> Int
  312. Nat.toFloat : Nat -> Float
  313. Nat.toInt : Nat -> Int
  314. Nat.toText : Nat -> Text
  315. Nat.trailingZeros : Nat -> Nat
  316. Nat.xor : Nat -> Nat -> Nat
  317. structural type Optional a
  318. Optional.None : Optional a
  319. Optional.Some : a -> Optional a
  320. builtin type Ref
  321. Ref.read : Ref g a ->{g} a
  322. Ref.write : Ref g a -> a ->{g} ()
  323. builtin type Request
  324. builtin type Scope
  325. Scope.ref : a ->{Scope s} Ref {Scope s} a
  326. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  327. structural type SeqView a b
  328. SeqView.VElem : a -> b -> SeqView a b
  329. SeqView.VEmpty : SeqView a b
  330. unique type Test.Result
  331. Test.Result.Fail : Text -> Result
  332. Test.Result.Ok : Text -> Result
  333. builtin type Text
  334. Text.!= : Text -> Text -> Boolean
  335. Text.++ : Text -> Text -> Text
  336. Text.drop : Nat -> Text -> Text
  337. Text.empty : Text
  338. Text.eq : Text -> Text -> Boolean
  339. Text.fromCharList : [Char] -> Text
  340. Text.fromUtf8.impl : Bytes -> Either Failure Text
  341. Text.gt : Text -> Text -> Boolean
  342. Text.gteq : Text -> Text -> Boolean
  343. Text.lt : Text -> Text -> Boolean
  344. Text.lteq : Text -> Text -> Boolean
  345. Text.repeat : Nat -> Text -> Text
  346. Text.size : Text -> Nat
  347. Text.take : Nat -> Text -> Text
  348. Text.toCharList : Text -> [Char]
  349. Text.toUtf8 : Text -> Bytes
  350. Text.uncons : Text -> Optional (Char, Text)
  351. Text.unsnoc : Text -> Optional (Text, Char)
  352. todo : a -> b
  353. structural type Tuple a b
  354. Tuple.Cons : a -> b -> Tuple a b
  355. structural type Unit
  356. Unit.Unit : ()
  357. Universal.< : a -> a -> Boolean
  358. Universal.<= : a -> a -> Boolean
  359. Universal.== : a -> a -> Boolean
  360. Universal.> : a -> a -> Boolean
  361. Universal.>= : a -> a -> Boolean
  362. Universal.compare : a -> a -> Int
  363. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  364. builtin type Value
  365. Value.dependencies : Value -> [Term]
  366. Value.deserialize : Bytes -> Either Text Value
  367. Value.load : Value ->{IO} Either [Term] a
  368. Value.serialize : Value -> Bytes
  369. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.gt      : Float -> Float -> Boolean
    2.  Float.gteq    : Float -> Float -> Boolean
    3.  Float.log     : Float -> Float
    4.  Float.logBase : Float -> Float -> Float
    5.  Float.lt      : Float -> Float -> Boolean
    6.  Float.lteq    : Float -> Float -> Boolean
    7.  Float.max     : Float -> Float -> Float
    8.  Float.min     : Float -> Float -> Float
    9.  Float.pow     : Float -> Float -> Float
    10. Float.round   : Float -> Int
    11. Float.sin     : Float -> Float
  
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

  1.  Float.gt : Float -> Float -> Boolean
  2.  Float.gteq : Float -> Float -> Boolean
  3.  Float.log : Float -> Float
  4.  Float.logBase : Float -> Float -> Float
  5.  Float.lt : Float -> Float -> Boolean
  6.  Float.lteq : Float -> Float -> Boolean
  7.  Float.max : Float -> Float -> Float
  8.  Float.min : Float -> Float -> Float
  9.  Float.pow : Float -> Float -> Float
  10. Float.round : Float -> Int
  11. Float.sin : Float -> Float
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
