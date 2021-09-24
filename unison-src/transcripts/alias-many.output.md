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
  45.  Code.display : Text -> Code -> Text
  46.  Code.isMissing : Term ->{IO} Boolean
  47.  Code.lookup : Term ->{IO} Optional Code
  48.  Code.serialize : Code -> Bytes
  49.  Code.validate : [(Term, Code)] ->{IO} Optional Failure
  50.  crypto.hash : HashAlgorithm -> a -> Bytes
  51.  builtin type crypto.HashAlgorithm
  52.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  53.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  54.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  55.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  56.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  57.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  58.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  59.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  60.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  61.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  62.  Debug.watch : Text -> a -> a
  63.  unique type Doc
  64.  Doc.Blob : Text -> Doc
  65.  Doc.Evaluate : Term -> Doc
  66.  Doc.Join : [Doc] -> Doc
  67.  Doc.Link : Link -> Doc
  68.  Doc.Signature : Term -> Doc
  69.  Doc.Source : Link -> Doc
  70.  structural type Either a b
  71.  Either.Left : a -> Either a b
  72.  Either.Right : b -> Either a b
  73.  structural ability Exception
  74.  Exception.raise : Failure ->{Exception} x
  75.  builtin type Float
  76.  Float.* : Float -> Float -> Float
  77.  Float.+ : Float -> Float -> Float
  78.  Float.- : Float -> Float -> Float
  79.  Float./ : Float -> Float -> Float
  80.  Float.abs : Float -> Float
  81.  Float.acos : Float -> Float
  82.  Float.acosh : Float -> Float
  83.  Float.asin : Float -> Float
  84.  Float.asinh : Float -> Float
  85.  Float.atan : Float -> Float
  86.  Float.atan2 : Float -> Float -> Float
  87.  Float.atanh : Float -> Float
  88.  Float.ceiling : Float -> Int
  89.  Float.cos : Float -> Float
  90.  Float.cosh : Float -> Float
  91.  Float.eq : Float -> Float -> Boolean
  92.  Float.exp : Float -> Float
  93.  Float.floor : Float -> Int
  94.  Float.fromRepresentation : Nat -> Float
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
  111. Float.toRepresentation : Float -> Nat
  112. Float.toText : Float -> Text
  113. Float.truncate : Float -> Int
  114. builtin type Int
  115. Int.* : Int -> Int -> Int
  116. Int.+ : Int -> Int -> Int
  117. Int.- : Int -> Int -> Int
  118. Int./ : Int -> Int -> Int
  119. Int.and : Int -> Int -> Int
  120. Int.complement : Int -> Int
  121. Int.eq : Int -> Int -> Boolean
  122. Int.fromRepresentation : Nat -> Int
  123. Int.fromText : Text -> Optional Int
  124. Int.gt : Int -> Int -> Boolean
  125. Int.gteq : Int -> Int -> Boolean
  126. Int.increment : Int -> Int
  127. Int.isEven : Int -> Boolean
  128. Int.isOdd : Int -> Boolean
  129. Int.leadingZeros : Int -> Nat
  130. Int.lt : Int -> Int -> Boolean
  131. Int.lteq : Int -> Int -> Boolean
  132. Int.mod : Int -> Int -> Int
  133. Int.negate : Int -> Int
  134. Int.or : Int -> Int -> Int
  135. Int.popCount : Int -> Nat
  136. Int.pow : Int -> Nat -> Int
  137. Int.shiftLeft : Int -> Nat -> Int
  138. Int.shiftRight : Int -> Nat -> Int
  139. Int.signum : Int -> Int
  140. Int.toFloat : Int -> Float
  141. Int.toRepresentation : Int -> Nat
  142. Int.toText : Int -> Text
  143. Int.trailingZeros : Int -> Nat
  144. Int.truncate0 : Int -> Nat
  145. Int.xor : Int -> Int -> Int
  146. unique type io2.BufferMode
  147. io2.BufferMode.BlockBuffering : BufferMode
  148. io2.BufferMode.LineBuffering : BufferMode
  149. io2.BufferMode.NoBuffering : BufferMode
  150. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  151. unique type io2.Failure
  152. io2.Failure.Failure : Type -> Text -> Any -> Failure
  153. unique type io2.FileMode
  154. io2.FileMode.Append : FileMode
  155. io2.FileMode.Read : FileMode
  156. io2.FileMode.ReadWrite : FileMode
  157. io2.FileMode.Write : FileMode
  158. builtin type io2.Handle
  159. builtin type io2.IO
  160. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  161. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  162. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  163. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  164. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  165. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  166. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  167. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  168. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  169. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  170. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  171. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  172. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  173. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  174. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  175. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  176. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  177. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  178. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  179. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  180. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  181. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  182. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  183. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  184. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  185. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  186. io2.IO.ref : a ->{IO} Ref {IO} a
  187. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  188. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  189. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  190. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  191. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  192. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  193. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  194. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  195. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  196. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  197. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  198. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  199. io2.IO.stdHandle : StdHandle -> Handle
  200. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  201. unique type io2.IOError
  202. io2.IOError.AlreadyExists : IOError
  203. io2.IOError.EOF : IOError
  204. io2.IOError.IllegalOperation : IOError
  205. io2.IOError.NoSuchThing : IOError
  206. io2.IOError.PermissionDenied : IOError
  207. io2.IOError.ResourceBusy : IOError
  208. io2.IOError.ResourceExhausted : IOError
  209. io2.IOError.UserError : IOError
  210. unique type io2.IOFailure
  211. builtin type io2.MVar
  212. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  213. io2.MVar.new : a ->{IO} MVar a
  214. io2.MVar.newEmpty : '{IO} MVar a
  215. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  216. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  217. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  218. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  219. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  220. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  221. io2.MVar.tryTake : MVar a ->{IO} Optional a
  222. unique type io2.SeekMode
  223. io2.SeekMode.AbsoluteSeek : SeekMode
  224. io2.SeekMode.RelativeSeek : SeekMode
  225. io2.SeekMode.SeekFromEnd : SeekMode
  226. builtin type io2.Socket
  227. unique type io2.StdHandle
  228. io2.StdHandle.StdErr : StdHandle
  229. io2.StdHandle.StdIn : StdHandle
  230. io2.StdHandle.StdOut : StdHandle
  231. builtin type io2.STM
  232. io2.STM.atomically : '{STM} a ->{IO} a
  233. io2.STM.retry : '{STM} a
  234. builtin type io2.ThreadId
  235. builtin type io2.Tls
  236. builtin type io2.Tls.Cipher
  237. builtin type io2.Tls.ClientConfig
  238. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  239. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  240. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  241. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  242. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  243. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  244. io2.Tls.encodeCert : SignedCert -> Bytes
  245. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  246. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  247. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  248. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  249. builtin type io2.Tls.PrivateKey
  250. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  251. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  252. builtin type io2.Tls.ServerConfig
  253. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  254. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  255. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  256. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  257. builtin type io2.Tls.SignedCert
  258. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  259. builtin type io2.Tls.Version
  260. unique type io2.TlsFailure
  261. builtin type io2.TVar
  262. io2.TVar.new : a ->{STM} TVar a
  263. io2.TVar.newIO : a ->{IO} TVar a
  264. io2.TVar.read : TVar a ->{STM} a
  265. io2.TVar.readIO : TVar a ->{IO} a
  266. io2.TVar.swap : TVar a -> a ->{STM} a
  267. io2.TVar.write : TVar a -> a ->{STM} ()
  268. unique type IsPropagated
  269. IsPropagated.IsPropagated : IsPropagated
  270. unique type IsTest
  271. IsTest.IsTest : IsTest
  272. unique type Link
  273. builtin type Link.Term
  274. Link.Term : Term -> Link
  275. Link.Term.toText : Term -> Text
  276. builtin type Link.Type
  277. Link.Type : Type -> Link
  278. builtin type List
  279. List.++ : [a] -> [a] -> [a]
  280. List.+: : a -> [a] -> [a]
  281. List.:+ : [a] -> a -> [a]
  282. List.at : Nat -> [a] -> Optional a
  283. List.cons : a -> [a] -> [a]
  284. List.drop : Nat -> [a] -> [a]
  285. List.empty : [a]
  286. List.size : [a] -> Nat
  287. List.snoc : [a] -> a -> [a]
  288. List.take : Nat -> [a] -> [a]
  289. metadata.isPropagated : IsPropagated
  290. metadata.isTest : IsTest
  291. builtin type Nat
  292. Nat.* : Nat -> Nat -> Nat
  293. Nat.+ : Nat -> Nat -> Nat
  294. Nat./ : Nat -> Nat -> Nat
  295. Nat.and : Nat -> Nat -> Nat
  296. Nat.complement : Nat -> Nat
  297. Nat.drop : Nat -> Nat -> Nat
  298. Nat.eq : Nat -> Nat -> Boolean
  299. Nat.fromText : Text -> Optional Nat
  300. Nat.gt : Nat -> Nat -> Boolean
  301. Nat.gteq : Nat -> Nat -> Boolean
  302. Nat.increment : Nat -> Nat
  303. Nat.isEven : Nat -> Boolean
  304. Nat.isOdd : Nat -> Boolean
  305. Nat.leadingZeros : Nat -> Nat
  306. Nat.lt : Nat -> Nat -> Boolean
  307. Nat.lteq : Nat -> Nat -> Boolean
  308. Nat.mod : Nat -> Nat -> Nat
  309. Nat.or : Nat -> Nat -> Nat
  310. Nat.popCount : Nat -> Nat
  311. Nat.pow : Nat -> Nat -> Nat
  312. Nat.shiftLeft : Nat -> Nat -> Nat
  313. Nat.shiftRight : Nat -> Nat -> Nat
  314. Nat.sub : Nat -> Nat -> Int
  315. Nat.toFloat : Nat -> Float
  316. Nat.toInt : Nat -> Int
  317. Nat.toText : Nat -> Text
  318. Nat.trailingZeros : Nat -> Nat
  319. Nat.xor : Nat -> Nat -> Nat
  320. structural type Optional a
  321. Optional.None : Optional a
  322. Optional.Some : a -> Optional a
  323. builtin type Ref
  324. Ref.read : Ref g a ->{g} a
  325. Ref.write : Ref g a -> a ->{g} ()
  326. builtin type Request
  327. builtin type Scope
  328. Scope.ref : a ->{Scope s} Ref {Scope s} a
  329. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  330. structural type SeqView a b
  331. SeqView.VElem : a -> b -> SeqView a b
  332. SeqView.VEmpty : SeqView a b
  333. unique type Test.Result
  334. Test.Result.Fail : Text -> Result
  335. Test.Result.Ok : Text -> Result
  336. builtin type Text
  337. Text.!= : Text -> Text -> Boolean
  338. Text.++ : Text -> Text -> Text
  339. Text.drop : Nat -> Text -> Text
  340. Text.empty : Text
  341. Text.eq : Text -> Text -> Boolean
  342. Text.fromCharList : [Char] -> Text
  343. Text.fromUtf8.impl : Bytes -> Either Failure Text
  344. Text.gt : Text -> Text -> Boolean
  345. Text.gteq : Text -> Text -> Boolean
  346. Text.lt : Text -> Text -> Boolean
  347. Text.lteq : Text -> Text -> Boolean
  348. Text.repeat : Nat -> Text -> Text
  349. Text.size : Text -> Nat
  350. Text.take : Nat -> Text -> Text
  351. Text.toCharList : Text -> [Char]
  352. Text.toUtf8 : Text -> Bytes
  353. Text.uncons : Text -> Optional (Char, Text)
  354. Text.unsnoc : Text -> Optional (Text, Char)
  355. todo : a -> b
  356. structural type Tuple a b
  357. Tuple.Cons : a -> b -> Tuple a b
  358. structural type Unit
  359. Unit.Unit : ()
  360. Universal.< : a -> a -> Boolean
  361. Universal.<= : a -> a -> Boolean
  362. Universal.== : a -> a -> Boolean
  363. Universal.> : a -> a -> Boolean
  364. Universal.>= : a -> a -> Boolean
  365. Universal.compare : a -> a -> Int
  366. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  367. builtin type Value
  368. Value.dependencies : Value -> [Term]
  369. Value.deserialize : Bytes -> Either Text Value
  370. Value.load : Value ->{IO} Either [Term] a
  371. Value.serialize : Value -> Bytes
  372. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.fromRepresentation : Nat -> Float
    2.  Float.fromText           : Text -> Optional Float
    3.  Float.gt                 : Float -> Float -> Boolean
    4.  Float.gteq               : Float -> Float -> Boolean
    5.  Float.log                : Float -> Float
    6.  Float.logBase            : Float -> Float -> Float
    7.  Float.lt                 : Float -> Float -> Boolean
    8.  Float.lteq               : Float -> Float -> Boolean
    9.  Float.max                : Float -> Float -> Float
    10. Float.min                : Float -> Float -> Float
    11. Float.pow                : Float -> Float -> Float
  
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

  1.  Float.fromRepresentation : Nat -> Float
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
