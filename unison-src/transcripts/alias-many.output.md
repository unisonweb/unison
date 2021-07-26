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
  8.   Bytes.drop : Nat -> Bytes -> Bytes
  9.   Bytes.empty : Bytes
  10.  Bytes.flatten : Bytes -> Bytes
  11.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  12.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  13.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  14.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  15.  Bytes.fromList : [Nat] -> Bytes
  16.  Bytes.size : Bytes -> Nat
  17.  Bytes.take : Nat -> Bytes -> Bytes
  18.  Bytes.toBase16 : Bytes -> Bytes
  19.  Bytes.toBase32 : Bytes -> Bytes
  20.  Bytes.toBase64 : Bytes -> Bytes
  21.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  22.  Bytes.toList : Bytes -> [Nat]
  23.  builtin type Char
  24.  Char.fromNat : Nat -> Char
  25.  Char.toNat : Char -> Nat
  26.  Char.toText : Char -> Text
  27.  builtin type Code
  28.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  29.  Code.dependencies : Code -> [Term]
  30.  Code.deserialize : Bytes -> Either Text Code
  31.  Code.isMissing : Term ->{IO} Boolean
  32.  Code.lookup : Term ->{IO} Optional Code
  33.  Code.serialize : Code -> Bytes
  34.  Debug.watch : Text -> a -> a
  35.  unique type Doc
  36.  Doc.Blob : Text -> Doc
  37.  Doc.Evaluate : Term -> Doc
  38.  Doc.Join : [Doc] -> Doc
  39.  Doc.Link : Link -> Doc
  40.  Doc.Signature : Term -> Doc
  41.  Doc.Source : Link -> Doc
  42.  type Either a b
  43.  Either.Left : a -> Either a b
  44.  Either.Right : b -> Either a b
  45.  ability Exception
  46.  Exception.raise : Failure ->{Exception} x
  47.  builtin type Float
  48.  Float.* : Float -> Float -> Float
  49.  Float.+ : Float -> Float -> Float
  50.  Float.- : Float -> Float -> Float
  51.  Float./ : Float -> Float -> Float
  52.  Float.abs : Float -> Float
  53.  Float.acos : Float -> Float
  54.  Float.acosh : Float -> Float
  55.  Float.asin : Float -> Float
  56.  Float.asinh : Float -> Float
  57.  Float.atan : Float -> Float
  58.  Float.atan2 : Float -> Float -> Float
  59.  Float.atanh : Float -> Float
  60.  Float.ceiling : Float -> Int
  61.  Float.cos : Float -> Float
  62.  Float.cosh : Float -> Float
  63.  Float.eq : Float -> Float -> Boolean
  64.  Float.exp : Float -> Float
  65.  Float.floor : Float -> Int
  66.  Float.fromText : Text -> Optional Float
  67.  Float.gt : Float -> Float -> Boolean
  68.  Float.gteq : Float -> Float -> Boolean
  69.  Float.log : Float -> Float
  70.  Float.logBase : Float -> Float -> Float
  71.  Float.lt : Float -> Float -> Boolean
  72.  Float.lteq : Float -> Float -> Boolean
  73.  Float.max : Float -> Float -> Float
  74.  Float.min : Float -> Float -> Float
  75.  Float.pow : Float -> Float -> Float
  76.  Float.round : Float -> Int
  77.  Float.sin : Float -> Float
  78.  Float.sinh : Float -> Float
  79.  Float.sqrt : Float -> Float
  80.  Float.tan : Float -> Float
  81.  Float.tanh : Float -> Float
  82.  Float.toText : Float -> Text
  83.  Float.truncate : Float -> Int
  84.  builtin type Int
  85.  Int.* : Int -> Int -> Int
  86.  Int.+ : Int -> Int -> Int
  87.  Int.- : Int -> Int -> Int
  88.  Int./ : Int -> Int -> Int
  89.  Int.and : Int -> Int -> Int
  90.  Int.complement : Int -> Int
  91.  Int.eq : Int -> Int -> Boolean
  92.  Int.fromText : Text -> Optional Int
  93.  Int.gt : Int -> Int -> Boolean
  94.  Int.gteq : Int -> Int -> Boolean
  95.  Int.increment : Int -> Int
  96.  Int.isEven : Int -> Boolean
  97.  Int.isOdd : Int -> Boolean
  98.  Int.leadingZeros : Int -> Nat
  99.  Int.lt : Int -> Int -> Boolean
  100. Int.lteq : Int -> Int -> Boolean
  101. Int.mod : Int -> Int -> Int
  102. Int.negate : Int -> Int
  103. Int.or : Int -> Int -> Int
  104. Int.popCount : Int -> Nat
  105. Int.pow : Int -> Nat -> Int
  106. Int.shiftLeft : Int -> Nat -> Int
  107. Int.shiftRight : Int -> Nat -> Int
  108. Int.signum : Int -> Int
  109. Int.toFloat : Int -> Float
  110. Int.toText : Int -> Text
  111. Int.trailingZeros : Int -> Nat
  112. Int.truncate0 : Int -> Nat
  113. Int.xor : Int -> Int -> Int
  114. unique type IsPropagated
  115. IsPropagated.IsPropagated : IsPropagated
  116. unique type IsTest
  117. IsTest.IsTest : IsTest
  118. unique type Link
  119. builtin type Link.Term
  120. Link.Term : Term -> Link
  121. builtin type Link.Type
  122. Link.Type : Type -> Link
  123. builtin type List
  124. List.++ : [a] -> [a] -> [a]
  125. List.+: : a -> [a] -> [a]
  126. List.:+ : [a] -> a -> [a]
  127. List.at : Nat -> [a] -> Optional a
  128. List.cons : a -> [a] -> [a]
  129. List.drop : Nat -> [a] -> [a]
  130. List.empty : [a]
  131. List.size : [a] -> Nat
  132. List.snoc : [a] -> a -> [a]
  133. List.take : Nat -> [a] -> [a]
  134. builtin type Nat
  135. Nat.* : Nat -> Nat -> Nat
  136. Nat.+ : Nat -> Nat -> Nat
  137. Nat./ : Nat -> Nat -> Nat
  138. Nat.and : Nat -> Nat -> Nat
  139. Nat.complement : Nat -> Nat
  140. Nat.drop : Nat -> Nat -> Nat
  141. Nat.eq : Nat -> Nat -> Boolean
  142. Nat.fromText : Text -> Optional Nat
  143. Nat.gt : Nat -> Nat -> Boolean
  144. Nat.gteq : Nat -> Nat -> Boolean
  145. Nat.increment : Nat -> Nat
  146. Nat.isEven : Nat -> Boolean
  147. Nat.isOdd : Nat -> Boolean
  148. Nat.leadingZeros : Nat -> Nat
  149. Nat.lt : Nat -> Nat -> Boolean
  150. Nat.lteq : Nat -> Nat -> Boolean
  151. Nat.mod : Nat -> Nat -> Nat
  152. Nat.or : Nat -> Nat -> Nat
  153. Nat.popCount : Nat -> Nat
  154. Nat.pow : Nat -> Nat -> Nat
  155. Nat.shiftLeft : Nat -> Nat -> Nat
  156. Nat.shiftRight : Nat -> Nat -> Nat
  157. Nat.sub : Nat -> Nat -> Int
  158. Nat.toFloat : Nat -> Float
  159. Nat.toInt : Nat -> Int
  160. Nat.toText : Nat -> Text
  161. Nat.trailingZeros : Nat -> Nat
  162. Nat.xor : Nat -> Nat -> Nat
  163. type Optional a
  164. Optional.None : Optional a
  165. Optional.Some : a -> Optional a
  166. builtin type Request
  167. type SeqView a b
  168. SeqView.VElem : a -> b -> SeqView a b
  169. SeqView.VEmpty : SeqView a b
  170. unique type Test.Result
  171. Test.Result.Fail : Text -> Result
  172. Test.Result.Ok : Text -> Result
  173. builtin type Text
  174. Text.!= : Text -> Text -> Boolean
  175. Text.++ : Text -> Text -> Text
  176. Text.drop : Nat -> Text -> Text
  177. Text.empty : Text
  178. Text.eq : Text -> Text -> Boolean
  179. Text.fromCharList : [Char] -> Text
  180. Text.fromUtf8.impl : Bytes -> Either Failure Text
  181. Text.gt : Text -> Text -> Boolean
  182. Text.gteq : Text -> Text -> Boolean
  183. Text.lt : Text -> Text -> Boolean
  184. Text.lteq : Text -> Text -> Boolean
  185. Text.repeat : Nat -> Text -> Text
  186. Text.size : Text -> Nat
  187. Text.take : Nat -> Text -> Text
  188. Text.toCharList : Text -> [Char]
  189. Text.toUtf8 : Text -> Bytes
  190. Text.uncons : Text -> Optional (Char, Text)
  191. Text.unsnoc : Text -> Optional (Text, Char)
  192. type Tuple a b
  193. Tuple.Cons : a -> b -> Tuple a b
  194. type Unit
  195. Unit.Unit : ()
  196. Universal.< : a -> a -> Boolean
  197. Universal.<= : a -> a -> Boolean
  198. Universal.== : a -> a -> Boolean
  199. Universal.> : a -> a -> Boolean
  200. Universal.>= : a -> a -> Boolean
  201. Universal.compare : a -> a -> Int
  202. builtin type Value
  203. Value.dependencies : Value -> [Term]
  204. Value.deserialize : Bytes -> Either Text Value
  205. Value.load : Value ->{IO} Either [Term] a
  206. Value.serialize : Value -> Bytes
  207. Value.value : a -> Value
  208. bug : a -> b
  209. builtin type crypto.HashAlgorithm
  210. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  211. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  212. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  213. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  214. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  215. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  216. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  217. crypto.hash : HashAlgorithm -> a -> Bytes
  218. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  219. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  220. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  221. unique type io2.BufferMode
  222. io2.BufferMode.BlockBuffering : BufferMode
  223. io2.BufferMode.LineBuffering : BufferMode
  224. io2.BufferMode.NoBuffering : BufferMode
  225. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  226. unique type io2.Failure
  227. io2.Failure.Failure : Type -> Text -> Any -> Failure
  228. unique type io2.FileMode
  229. io2.FileMode.Append : FileMode
  230. io2.FileMode.Read : FileMode
  231. io2.FileMode.ReadWrite : FileMode
  232. io2.FileMode.Write : FileMode
  233. builtin type io2.Handle
  234. builtin type io2.IO
  235. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  236. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  237. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  238. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  239. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  240. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  241. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  242. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  243. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  244. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  245. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  246. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  247. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  248. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  249. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  250. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  251. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  252. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  253. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  254. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  255. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  256. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  257. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  258. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  259. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  260. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  261. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  262. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  263. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  264. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  265. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  266. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  267. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  268. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  269. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  270. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  271. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  272. io2.IO.stdHandle : StdHandle -> Handle
  273. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  274. unique type io2.IOError
  275. io2.IOError.AlreadyExists : IOError
  276. io2.IOError.EOF : IOError
  277. io2.IOError.IllegalOperation : IOError
  278. io2.IOError.NoSuchThing : IOError
  279. io2.IOError.PermissionDenied : IOError
  280. io2.IOError.ResourceBusy : IOError
  281. io2.IOError.ResourceExhausted : IOError
  282. io2.IOError.UserError : IOError
  283. unique type io2.IOFailure
  284. builtin type io2.MVar
  285. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  286. io2.MVar.new : a ->{IO} MVar a
  287. io2.MVar.newEmpty : '{IO} MVar a
  288. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  289. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  290. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  291. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  292. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  293. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  294. io2.MVar.tryTake : MVar a ->{IO} Optional a
  295. builtin type io2.STM
  296. io2.STM.atomically : '{STM} a ->{IO} a
  297. io2.STM.retry : '{STM} a
  298. unique type io2.SeekMode
  299. io2.SeekMode.AbsoluteSeek : SeekMode
  300. io2.SeekMode.RelativeSeek : SeekMode
  301. io2.SeekMode.SeekFromEnd : SeekMode
  302. builtin type io2.Socket
  303. unique type io2.StdHandle
  304. io2.StdHandle.StdErr : StdHandle
  305. io2.StdHandle.StdIn : StdHandle
  306. io2.StdHandle.StdOut : StdHandle
  307. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  308. builtin type io2.TVar
  309. io2.TVar.new : a ->{STM} TVar a
  310. io2.TVar.newIO : a ->{IO} TVar a
  311. io2.TVar.read : TVar a ->{STM} a
  312. io2.TVar.readIO : TVar a ->{IO} a
  313. io2.TVar.swap : TVar a -> a ->{STM} a
  314. io2.TVar.write : TVar a -> a ->{STM} ()
  315. builtin type io2.ThreadId
  316. builtin type io2.Tls
  317. builtin type io2.Tls.Cipher
  318. builtin type io2.Tls.ClientConfig
  319. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  320. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  321. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  322. builtin type io2.Tls.PrivateKey
  323. builtin type io2.Tls.ServerConfig
  324. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  325. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  326. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  327. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  328. builtin type io2.Tls.SignedCert
  329. builtin type io2.Tls.Version
  330. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  331. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  332. io2.Tls.encodeCert : SignedCert -> Bytes
  333. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  334. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  335. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  336. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  337. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  338. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  339. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  340. unique type io2.TlsFailure
  341. metadata.isPropagated : IsPropagated
  342. metadata.isTest : IsTest
  343. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.gteq         : Int -> Int -> Boolean
    2.  Int.increment    : Int -> Int
    3.  Int.isEven       : Int -> Boolean
    4.  Int.isOdd        : Int -> Boolean
    5.  Int.leadingZeros : Int -> Nat
    6.  Int.lt           : Int -> Int -> Boolean
    7.  Int.lteq         : Int -> Int -> Boolean
    8.  Int.mod          : Int -> Int -> Int
    9.  Int.negate       : Int -> Int
    10. Int.or           : Int -> Int -> Int
    11. Int.popCount     : Int -> Nat
  
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

  1.  Int.gteq : Int -> Int -> Boolean
  2.  Int.increment : Int -> Int
  3.  Int.isEven : Int -> Boolean
  4.  Int.isOdd : Int -> Boolean
  5.  Int.leadingZeros : Int -> Nat
  6.  Int.lt : Int -> Int -> Boolean
  7.  Int.lteq : Int -> Int -> Boolean
  8.  Int.mod : Int -> Int -> Int
  9.  Int.negate : Int -> Int
  10. Int.or : Int -> Int -> Int
  11. Int.popCount : Int -> Nat
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
