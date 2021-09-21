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
  44.  Code.display : Text -> Code -> Text
  45.  Code.isMissing : Term ->{IO} Boolean
  46.  Code.lookup : Term ->{IO} Optional Code
  47.  Code.serialize : Code -> Bytes
  48.  Code.validate : [(Term, Code)] ->{IO} Optional Failure
  49.  crypto.hash : HashAlgorithm -> a -> Bytes
  50.  builtin type crypto.HashAlgorithm
  51.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  52.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  53.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  54.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  55.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  56.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  57.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  58.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  59.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  60.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  61.  Debug.watch : Text -> a -> a
  62.  unique type Doc
  63.  Doc.Blob : Text -> Doc
  64.  Doc.Evaluate : Term -> Doc
  65.  Doc.Join : [Doc] -> Doc
  66.  Doc.Link : Link -> Doc
  67.  Doc.Signature : Term -> Doc
  68.  Doc.Source : Link -> Doc
  69.  structural type Either a b
  70.  Either.Left : a -> Either a b
  71.  Either.Right : b -> Either a b
  72.  structural ability Exception
  73.  Exception.raise : Failure ->{Exception} x
  74.  builtin type Float
  75.  Float.* : Float -> Float -> Float
  76.  Float.+ : Float -> Float -> Float
  77.  Float.- : Float -> Float -> Float
  78.  Float./ : Float -> Float -> Float
  79.  Float.abs : Float -> Float
  80.  Float.acos : Float -> Float
  81.  Float.acosh : Float -> Float
  82.  Float.asin : Float -> Float
  83.  Float.asinh : Float -> Float
  84.  Float.atan : Float -> Float
  85.  Float.atan2 : Float -> Float -> Float
  86.  Float.atanh : Float -> Float
  87.  Float.ceiling : Float -> Int
  88.  Float.cos : Float -> Float
  89.  Float.cosh : Float -> Float
  90.  Float.eq : Float -> Float -> Boolean
  91.  Float.exp : Float -> Float
  92.  Float.floor : Float -> Int
  93.  Float.fromRepresentation : Nat -> Float
  94.  Float.fromText : Text -> Optional Float
  95.  Float.gt : Float -> Float -> Boolean
  96.  Float.gteq : Float -> Float -> Boolean
  97.  Float.log : Float -> Float
  98.  Float.logBase : Float -> Float -> Float
  99.  Float.lt : Float -> Float -> Boolean
  100. Float.lteq : Float -> Float -> Boolean
  101. Float.max : Float -> Float -> Float
  102. Float.min : Float -> Float -> Float
  103. Float.pow : Float -> Float -> Float
  104. Float.round : Float -> Int
  105. Float.sin : Float -> Float
  106. Float.sinh : Float -> Float
  107. Float.sqrt : Float -> Float
  108. Float.tan : Float -> Float
  109. Float.tanh : Float -> Float
  110. Float.toRepresentation : Float -> Nat
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
  121. Int.fromRepresentation : Nat -> Int
  122. Int.fromText : Text -> Optional Int
  123. Int.gt : Int -> Int -> Boolean
  124. Int.gteq : Int -> Int -> Boolean
  125. Int.increment : Int -> Int
  126. Int.isEven : Int -> Boolean
  127. Int.isOdd : Int -> Boolean
  128. Int.leadingZeros : Int -> Nat
  129. Int.lt : Int -> Int -> Boolean
  130. Int.lteq : Int -> Int -> Boolean
  131. Int.mod : Int -> Int -> Int
  132. Int.negate : Int -> Int
  133. Int.or : Int -> Int -> Int
  134. Int.popCount : Int -> Nat
  135. Int.pow : Int -> Nat -> Int
  136. Int.shiftLeft : Int -> Nat -> Int
  137. Int.shiftRight : Int -> Nat -> Int
  138. Int.signum : Int -> Int
  139. Int.toFloat : Int -> Float
  140. Int.toRepresentation : Int -> Nat
  141. Int.toText : Int -> Text
  142. Int.trailingZeros : Int -> Nat
  143. Int.truncate0 : Int -> Nat
  144. Int.xor : Int -> Int -> Int
  145. unique type io2.BufferMode
  146. io2.BufferMode.BlockBuffering : BufferMode
  147. io2.BufferMode.LineBuffering : BufferMode
  148. io2.BufferMode.NoBuffering : BufferMode
  149. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  150. unique type io2.Failure
  151. io2.Failure.Failure : Type -> Text -> Any -> Failure
  152. unique type io2.FileMode
  153. io2.FileMode.Append : FileMode
  154. io2.FileMode.Read : FileMode
  155. io2.FileMode.ReadWrite : FileMode
  156. io2.FileMode.Write : FileMode
  157. builtin type io2.Handle
  158. builtin type io2.IO
  159. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  160. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  161. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  162. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  163. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  164. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  165. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  166. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  167. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  168. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  169. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  170. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  171. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  172. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  173. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  174. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  175. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  176. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  177. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  178. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  179. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  180. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  181. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  182. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  183. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  184. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  185. io2.IO.ref : a ->{IO} Ref {IO} a
  186. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  187. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  188. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  189. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  190. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  191. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  192. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  193. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  194. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  195. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  196. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  197. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  198. io2.IO.stdHandle : StdHandle -> Handle
  199. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  200. unique type io2.IOError
  201. io2.IOError.AlreadyExists : IOError
  202. io2.IOError.EOF : IOError
  203. io2.IOError.IllegalOperation : IOError
  204. io2.IOError.NoSuchThing : IOError
  205. io2.IOError.PermissionDenied : IOError
  206. io2.IOError.ResourceBusy : IOError
  207. io2.IOError.ResourceExhausted : IOError
  208. io2.IOError.UserError : IOError
  209. unique type io2.IOFailure
  210. builtin type io2.MVar
  211. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  212. io2.MVar.new : a ->{IO} MVar a
  213. io2.MVar.newEmpty : '{IO} MVar a
  214. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  215. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  216. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  217. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  218. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  219. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  220. io2.MVar.tryTake : MVar a ->{IO} Optional a
  221. unique type io2.SeekMode
  222. io2.SeekMode.AbsoluteSeek : SeekMode
  223. io2.SeekMode.RelativeSeek : SeekMode
  224. io2.SeekMode.SeekFromEnd : SeekMode
  225. builtin type io2.Socket
  226. unique type io2.StdHandle
  227. io2.StdHandle.StdErr : StdHandle
  228. io2.StdHandle.StdIn : StdHandle
  229. io2.StdHandle.StdOut : StdHandle
  230. builtin type io2.STM
  231. io2.STM.atomically : '{STM} a ->{IO} a
  232. io2.STM.retry : '{STM} a
  233. builtin type io2.ThreadId
  234. builtin type io2.Tls
  235. builtin type io2.Tls.Cipher
  236. builtin type io2.Tls.ClientConfig
  237. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  238. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  239. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  240. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  241. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  242. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  243. io2.Tls.encodeCert : SignedCert -> Bytes
  244. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  245. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  246. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  247. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  248. builtin type io2.Tls.PrivateKey
  249. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  250. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  251. builtin type io2.Tls.ServerConfig
  252. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  253. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  254. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  255. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  256. builtin type io2.Tls.SignedCert
  257. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  258. builtin type io2.Tls.Version
  259. unique type io2.TlsFailure
  260. builtin type io2.TVar
  261. io2.TVar.new : a ->{STM} TVar a
  262. io2.TVar.newIO : a ->{IO} TVar a
  263. io2.TVar.read : TVar a ->{STM} a
  264. io2.TVar.readIO : TVar a ->{IO} a
  265. io2.TVar.swap : TVar a -> a ->{STM} a
  266. io2.TVar.write : TVar a -> a ->{STM} ()
  267. unique type IsPropagated
  268. IsPropagated.IsPropagated : IsPropagated
  269. unique type IsTest
  270. IsTest.IsTest : IsTest
  271. unique type Link
  272. builtin type Link.Term
  273. Link.Term : Term -> Link
  274. Link.Term.toText : Term -> Text
  275. builtin type Link.Type
  276. Link.Type : Type -> Link
  277. builtin type List
  278. List.++ : [a] -> [a] -> [a]
  279. List.+: : a -> [a] -> [a]
  280. List.:+ : [a] -> a -> [a]
  281. List.at : Nat -> [a] -> Optional a
  282. List.cons : a -> [a] -> [a]
  283. List.drop : Nat -> [a] -> [a]
  284. List.empty : [a]
  285. List.size : [a] -> Nat
  286. List.snoc : [a] -> a -> [a]
  287. List.take : Nat -> [a] -> [a]
  288. metadata.isPropagated : IsPropagated
  289. metadata.isTest : IsTest
  290. builtin type Nat
  291. Nat.* : Nat -> Nat -> Nat
  292. Nat.+ : Nat -> Nat -> Nat
  293. Nat./ : Nat -> Nat -> Nat
  294. Nat.and : Nat -> Nat -> Nat
  295. Nat.complement : Nat -> Nat
  296. Nat.drop : Nat -> Nat -> Nat
  297. Nat.eq : Nat -> Nat -> Boolean
  298. Nat.fromText : Text -> Optional Nat
  299. Nat.gt : Nat -> Nat -> Boolean
  300. Nat.gteq : Nat -> Nat -> Boolean
  301. Nat.increment : Nat -> Nat
  302. Nat.isEven : Nat -> Boolean
  303. Nat.isOdd : Nat -> Boolean
  304. Nat.leadingZeros : Nat -> Nat
  305. Nat.lt : Nat -> Nat -> Boolean
  306. Nat.lteq : Nat -> Nat -> Boolean
  307. Nat.mod : Nat -> Nat -> Nat
  308. Nat.or : Nat -> Nat -> Nat
  309. Nat.popCount : Nat -> Nat
  310. Nat.pow : Nat -> Nat -> Nat
  311. Nat.shiftLeft : Nat -> Nat -> Nat
  312. Nat.shiftRight : Nat -> Nat -> Nat
  313. Nat.sub : Nat -> Nat -> Int
  314. Nat.toFloat : Nat -> Float
  315. Nat.toInt : Nat -> Int
  316. Nat.toText : Nat -> Text
  317. Nat.trailingZeros : Nat -> Nat
  318. Nat.xor : Nat -> Nat -> Nat
  319. structural type Optional a
  320. Optional.None : Optional a
  321. Optional.Some : a -> Optional a
  322. builtin type Ref
  323. Ref.read : Ref g a ->{g} a
  324. Ref.write : Ref g a -> a ->{g} ()
  325. builtin type Request
  326. builtin type Scope
  327. Scope.ref : a ->{Scope s} Ref {Scope s} a
  328. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  329. structural type SeqView a b
  330. SeqView.VElem : a -> b -> SeqView a b
  331. SeqView.VEmpty : SeqView a b
  332. unique type Test.Result
  333. Test.Result.Fail : Text -> Result
  334. Test.Result.Ok : Text -> Result
  335. builtin type Text
  336. Text.!= : Text -> Text -> Boolean
  337. Text.++ : Text -> Text -> Text
  338. Text.drop : Nat -> Text -> Text
  339. Text.empty : Text
  340. Text.eq : Text -> Text -> Boolean
  341. Text.fromCharList : [Char] -> Text
  342. Text.fromUtf8.impl : Bytes -> Either Failure Text
  343. Text.gt : Text -> Text -> Boolean
  344. Text.gteq : Text -> Text -> Boolean
  345. Text.lt : Text -> Text -> Boolean
  346. Text.lteq : Text -> Text -> Boolean
  347. Text.repeat : Nat -> Text -> Text
  348. Text.size : Text -> Nat
  349. Text.take : Nat -> Text -> Text
  350. Text.toCharList : Text -> [Char]
  351. Text.toUtf8 : Text -> Bytes
  352. Text.uncons : Text -> Optional (Char, Text)
  353. Text.unsnoc : Text -> Optional (Text, Char)
  354. todo : a -> b
  355. structural type Tuple a b
  356. Tuple.Cons : a -> b -> Tuple a b
  357. structural type Unit
  358. Unit.Unit : ()
  359. Universal.< : a -> a -> Boolean
  360. Universal.<= : a -> a -> Boolean
  361. Universal.== : a -> a -> Boolean
  362. Universal.> : a -> a -> Boolean
  363. Universal.>= : a -> a -> Boolean
  364. Universal.compare : a -> a -> Int
  365. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  366. builtin type Value
  367. Value.dependencies : Value -> [Term]
  368. Value.deserialize : Bytes -> Either Text Value
  369. Value.load : Value ->{IO} Either [Term] a
  370. Value.serialize : Value -> Bytes
  371. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.fromText : Text -> Optional Float
    2.  Float.gt       : Float -> Float -> Boolean
    3.  Float.gteq     : Float -> Float -> Boolean
    4.  Float.log      : Float -> Float
    5.  Float.logBase  : Float -> Float -> Float
    6.  Float.lt       : Float -> Float -> Boolean
    7.  Float.lteq     : Float -> Float -> Boolean
    8.  Float.max      : Float -> Float -> Float
    9.  Float.min      : Float -> Float -> Float
    10. Float.pow      : Float -> Float -> Float
    11. Float.round    : Float -> Int
  
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

  1.  Float.fromText : Text -> Optional Float
  2.  Float.gt : Float -> Float -> Boolean
  3.  Float.gteq : Float -> Float -> Boolean
  4.  Float.log : Float -> Float
  5.  Float.logBase : Float -> Float -> Float
  6.  Float.lt : Float -> Float -> Boolean
  7.  Float.lteq : Float -> Float -> Boolean
  8.  Float.max : Float -> Float -> Float
  9.  Float.min : Float -> Float -> Float
  10. Float.pow : Float -> Float -> Float
  11. Float.round : Float -> Int
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
