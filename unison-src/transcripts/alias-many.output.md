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
  8.   Bytes.decodeNat16be : Bytes -> Optional (Nat, Bytes)
  9.   Bytes.decodeNat16le : Bytes -> Optional (Nat, Bytes)
  10.  Bytes.decodeNat32be : Bytes -> Optional (Nat, Bytes)
  11.  Bytes.decodeNat32le : Bytes -> Optional (Nat, Bytes)
  12.  Bytes.decodeNat64be : Bytes -> Optional (Nat, Bytes)
  13.  Bytes.decodeNat64le : Bytes -> Optional (Nat, Bytes)
  14.  Bytes.drop : Nat -> Bytes -> Bytes
  15.  Bytes.empty : Bytes
  16.  Bytes.encodeNat16be : Nat -> Bytes
  17.  Bytes.encodeNat16le : Nat -> Bytes
  18.  Bytes.encodeNat32be : Nat -> Bytes
  19.  Bytes.encodeNat32le : Nat -> Bytes
  20.  Bytes.encodeNat64be : Nat -> Bytes
  21.  Bytes.encodeNat64le : Nat -> Bytes
  22.  Bytes.flatten : Bytes -> Bytes
  23.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  24.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  25.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  26.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  27.  Bytes.fromList : [Nat] -> Bytes
  28.  Bytes.size : Bytes -> Nat
  29.  Bytes.take : Nat -> Bytes -> Bytes
  30.  Bytes.toBase16 : Bytes -> Bytes
  31.  Bytes.toBase32 : Bytes -> Bytes
  32.  Bytes.toBase64 : Bytes -> Bytes
  33.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  34.  Bytes.toList : Bytes -> [Nat]
  35.  builtin type Char
  36.  Char.fromNat : Nat -> Char
  37.  Char.toNat : Char -> Nat
  38.  Char.toText : Char -> Text
  39.  builtin type Code
  40.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  41.  Code.dependencies : Code -> [Term]
  42.  Code.deserialize : Bytes -> Either Text Code
  43.  Code.isMissing : Term ->{IO} Boolean
  44.  Code.lookup : Term ->{IO} Optional Code
  45.  Code.serialize : Code -> Bytes
  46.  Debug.watch : Text -> a -> a
  47.  unique type Doc
  48.  Doc.Blob : Text -> Doc
  49.  Doc.Evaluate : Term -> Doc
  50.  Doc.Join : [Doc] -> Doc
  51.  Doc.Link : Link -> Doc
  52.  Doc.Signature : Term -> Doc
  53.  Doc.Source : Link -> Doc
  54.  type Either a b
  55.  Either.Left : a -> Either a b
  56.  Either.Right : b -> Either a b
  57.  ability Exception
  58.  Exception.raise : Failure ->{Exception} x
  59.  builtin type Float
  60.  Float.* : Float -> Float -> Float
  61.  Float.+ : Float -> Float -> Float
  62.  Float.- : Float -> Float -> Float
  63.  Float./ : Float -> Float -> Float
  64.  Float.abs : Float -> Float
  65.  Float.acos : Float -> Float
  66.  Float.acosh : Float -> Float
  67.  Float.asin : Float -> Float
  68.  Float.asinh : Float -> Float
  69.  Float.atan : Float -> Float
  70.  Float.atan2 : Float -> Float -> Float
  71.  Float.atanh : Float -> Float
  72.  Float.ceiling : Float -> Int
  73.  Float.cos : Float -> Float
  74.  Float.cosh : Float -> Float
  75.  Float.eq : Float -> Float -> Boolean
  76.  Float.exp : Float -> Float
  77.  Float.floor : Float -> Int
  78.  Float.fromText : Text -> Optional Float
  79.  Float.gt : Float -> Float -> Boolean
  80.  Float.gteq : Float -> Float -> Boolean
  81.  Float.log : Float -> Float
  82.  Float.logBase : Float -> Float -> Float
  83.  Float.lt : Float -> Float -> Boolean
  84.  Float.lteq : Float -> Float -> Boolean
  85.  Float.max : Float -> Float -> Float
  86.  Float.min : Float -> Float -> Float
  87.  Float.pow : Float -> Float -> Float
  88.  Float.round : Float -> Int
  89.  Float.sin : Float -> Float
  90.  Float.sinh : Float -> Float
  91.  Float.sqrt : Float -> Float
  92.  Float.tan : Float -> Float
  93.  Float.tanh : Float -> Float
  94.  Float.toText : Float -> Text
  95.  Float.truncate : Float -> Int
  96.  builtin type Int
  97.  Int.* : Int -> Int -> Int
  98.  Int.+ : Int -> Int -> Int
  99.  Int.- : Int -> Int -> Int
  100. Int./ : Int -> Int -> Int
  101. Int.and : Int -> Int -> Int
  102. Int.complement : Int -> Int
  103. Int.eq : Int -> Int -> Boolean
  104. Int.fromText : Text -> Optional Int
  105. Int.gt : Int -> Int -> Boolean
  106. Int.gteq : Int -> Int -> Boolean
  107. Int.increment : Int -> Int
  108. Int.isEven : Int -> Boolean
  109. Int.isOdd : Int -> Boolean
  110. Int.leadingZeros : Int -> Nat
  111. Int.lt : Int -> Int -> Boolean
  112. Int.lteq : Int -> Int -> Boolean
  113. Int.mod : Int -> Int -> Int
  114. Int.negate : Int -> Int
  115. Int.or : Int -> Int -> Int
  116. Int.popCount : Int -> Nat
  117. Int.pow : Int -> Nat -> Int
  118. Int.shiftLeft : Int -> Nat -> Int
  119. Int.shiftRight : Int -> Nat -> Int
  120. Int.signum : Int -> Int
  121. Int.toFloat : Int -> Float
  122. Int.toText : Int -> Text
  123. Int.trailingZeros : Int -> Nat
  124. Int.truncate0 : Int -> Nat
  125. Int.xor : Int -> Int -> Int
  126. unique type IsPropagated
  127. IsPropagated.IsPropagated : IsPropagated
  128. unique type IsTest
  129. IsTest.IsTest : IsTest
  130. unique type Link
  131. builtin type Link.Term
  132. Link.Term : Term -> Link
  133. builtin type Link.Type
  134. Link.Type : Type -> Link
  135. builtin type List
  136. List.++ : [a] -> [a] -> [a]
  137. List.+: : a -> [a] -> [a]
  138. List.:+ : [a] -> a -> [a]
  139. List.at : Nat -> [a] -> Optional a
  140. List.cons : a -> [a] -> [a]
  141. List.drop : Nat -> [a] -> [a]
  142. List.empty : [a]
  143. List.size : [a] -> Nat
  144. List.snoc : [a] -> a -> [a]
  145. List.take : Nat -> [a] -> [a]
  146. builtin type Nat
  147. Nat.* : Nat -> Nat -> Nat
  148. Nat.+ : Nat -> Nat -> Nat
  149. Nat./ : Nat -> Nat -> Nat
  150. Nat.and : Nat -> Nat -> Nat
  151. Nat.complement : Nat -> Nat
  152. Nat.drop : Nat -> Nat -> Nat
  153. Nat.eq : Nat -> Nat -> Boolean
  154. Nat.fromText : Text -> Optional Nat
  155. Nat.gt : Nat -> Nat -> Boolean
  156. Nat.gteq : Nat -> Nat -> Boolean
  157. Nat.increment : Nat -> Nat
  158. Nat.isEven : Nat -> Boolean
  159. Nat.isOdd : Nat -> Boolean
  160. Nat.leadingZeros : Nat -> Nat
  161. Nat.lt : Nat -> Nat -> Boolean
  162. Nat.lteq : Nat -> Nat -> Boolean
  163. Nat.mod : Nat -> Nat -> Nat
  164. Nat.or : Nat -> Nat -> Nat
  165. Nat.popCount : Nat -> Nat
  166. Nat.pow : Nat -> Nat -> Nat
  167. Nat.shiftLeft : Nat -> Nat -> Nat
  168. Nat.shiftRight : Nat -> Nat -> Nat
  169. Nat.sub : Nat -> Nat -> Int
  170. Nat.toFloat : Nat -> Float
  171. Nat.toInt : Nat -> Int
  172. Nat.toText : Nat -> Text
  173. Nat.trailingZeros : Nat -> Nat
  174. Nat.xor : Nat -> Nat -> Nat
  175. type Optional a
  176. Optional.None : Optional a
  177. Optional.Some : a -> Optional a
  178. builtin type Request
  179. type SeqView a b
  180. SeqView.VElem : a -> b -> SeqView a b
  181. SeqView.VEmpty : SeqView a b
  182. unique type Test.Result
  183. Test.Result.Fail : Text -> Result
  184. Test.Result.Ok : Text -> Result
  185. builtin type Text
  186. Text.!= : Text -> Text -> Boolean
  187. Text.++ : Text -> Text -> Text
  188. Text.drop : Nat -> Text -> Text
  189. Text.empty : Text
  190. Text.eq : Text -> Text -> Boolean
  191. Text.fromCharList : [Char] -> Text
  192. Text.fromUtf8.impl : Bytes -> Either Failure Text
  193. Text.gt : Text -> Text -> Boolean
  194. Text.gteq : Text -> Text -> Boolean
  195. Text.lt : Text -> Text -> Boolean
  196. Text.lteq : Text -> Text -> Boolean
  197. Text.repeat : Nat -> Text -> Text
  198. Text.size : Text -> Nat
  199. Text.take : Nat -> Text -> Text
  200. Text.toCharList : Text -> [Char]
  201. Text.toUtf8 : Text -> Bytes
  202. Text.uncons : Text -> Optional (Char, Text)
  203. Text.unsnoc : Text -> Optional (Text, Char)
  204. type Tuple a b
  205. Tuple.Cons : a -> b -> Tuple a b
  206. type Unit
  207. Unit.Unit : ()
  208. Universal.< : a -> a -> Boolean
  209. Universal.<= : a -> a -> Boolean
  210. Universal.== : a -> a -> Boolean
  211. Universal.> : a -> a -> Boolean
  212. Universal.>= : a -> a -> Boolean
  213. Universal.compare : a -> a -> Int
  214. builtin type Value
  215. Value.dependencies : Value -> [Term]
  216. Value.deserialize : Bytes -> Either Text Value
  217. Value.load : Value ->{IO} Either [Term] a
  218. Value.serialize : Value -> Bytes
  219. Value.value : a -> Value
  220. bug : a -> b
  221. builtin type crypto.HashAlgorithm
  222. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  223. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  224. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  225. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  226. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  227. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  228. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  229. crypto.hash : HashAlgorithm -> a -> Bytes
  230. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  231. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  232. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  233. unique type io2.BufferMode
  234. io2.BufferMode.BlockBuffering : BufferMode
  235. io2.BufferMode.LineBuffering : BufferMode
  236. io2.BufferMode.NoBuffering : BufferMode
  237. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  238. unique type io2.Failure
  239. io2.Failure.Failure : Type -> Text -> Any -> Failure
  240. unique type io2.FileMode
  241. io2.FileMode.Append : FileMode
  242. io2.FileMode.Read : FileMode
  243. io2.FileMode.ReadWrite : FileMode
  244. io2.FileMode.Write : FileMode
  245. builtin type io2.Handle
  246. builtin type io2.IO
  247. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  248. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  249. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  250. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  251. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  252. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  253. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  254. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  255. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  256. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  257. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  258. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  259. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  260. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  261. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  262. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  263. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  264. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  265. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  266. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  267. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  268. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  269. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  270. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  271. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  272. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  273. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  274. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  275. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  276. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  277. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  278. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  279. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  280. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  281. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  282. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  283. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  284. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  285. io2.IO.stdHandle : StdHandle -> Handle
  286. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  287. unique type io2.IOError
  288. io2.IOError.AlreadyExists : IOError
  289. io2.IOError.EOF : IOError
  290. io2.IOError.IllegalOperation : IOError
  291. io2.IOError.NoSuchThing : IOError
  292. io2.IOError.PermissionDenied : IOError
  293. io2.IOError.ResourceBusy : IOError
  294. io2.IOError.ResourceExhausted : IOError
  295. io2.IOError.UserError : IOError
  296. unique type io2.IOFailure
  297. builtin type io2.MVar
  298. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  299. io2.MVar.new : a ->{IO} MVar a
  300. io2.MVar.newEmpty : '{IO} MVar a
  301. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  302. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  303. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  304. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  305. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  306. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  307. io2.MVar.tryTake : MVar a ->{IO} Optional a
  308. builtin type io2.STM
  309. io2.STM.atomically : '{STM} a ->{IO} a
  310. io2.STM.retry : '{STM} a
  311. unique type io2.SeekMode
  312. io2.SeekMode.AbsoluteSeek : SeekMode
  313. io2.SeekMode.RelativeSeek : SeekMode
  314. io2.SeekMode.SeekFromEnd : SeekMode
  315. builtin type io2.Socket
  316. unique type io2.StdHandle
  317. io2.StdHandle.StdErr : StdHandle
  318. io2.StdHandle.StdIn : StdHandle
  319. io2.StdHandle.StdOut : StdHandle
  320. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  321. builtin type io2.TVar
  322. io2.TVar.new : a ->{STM} TVar a
  323. io2.TVar.newIO : a ->{IO} TVar a
  324. io2.TVar.read : TVar a ->{STM} a
  325. io2.TVar.readIO : TVar a ->{IO} a
  326. io2.TVar.swap : TVar a -> a ->{STM} a
  327. io2.TVar.write : TVar a -> a ->{STM} ()
  328. builtin type io2.ThreadId
  329. builtin type io2.Tls
  330. builtin type io2.Tls.Cipher
  331. builtin type io2.Tls.ClientConfig
  332. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  333. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  334. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  335. builtin type io2.Tls.PrivateKey
  336. builtin type io2.Tls.ServerConfig
  337. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  338. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  339. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  340. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  341. builtin type io2.Tls.SignedCert
  342. builtin type io2.Tls.Version
  343. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  344. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  345. io2.Tls.encodeCert : SignedCert -> Bytes
  346. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  347. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  348. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  349. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  350. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  351. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  352. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  353. unique type io2.TlsFailure
  354. metadata.isPropagated : IsPropagated
  355. metadata.isTest : IsTest
  356. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type Int
    2.  Float.toText   : Float -> Text
    3.  Float.truncate : Float -> Int
    4.  Int.*          : Int -> Int -> Int
    5.  Int.+          : Int -> Int -> Int
    6.  Int.-          : Int -> Int -> Int
    7.  Int./          : Int -> Int -> Int
    8.  Int.and        : Int -> Int -> Int
    9.  Int.complement : Int -> Int
    10. Int.eq         : Int -> Int -> Boolean
    11. Int.fromText   : Text -> Optional Int
  
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

  1.  Float.toText : Float -> Text
  2.  Float.truncate : Float -> Int
  3.  builtin type Int
  4.  Int.* : Int -> Int -> Int
  5.  Int.+ : Int -> Int -> Int
  6.  Int.- : Int -> Int -> Int
  7.  Int./ : Int -> Int -> Int
  8.  Int.and : Int -> Int -> Int
  9.  Int.complement : Int -> Int
  10. Int.eq : Int -> Int -> Boolean
  11. Int.fromText : Text -> Optional Int
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
