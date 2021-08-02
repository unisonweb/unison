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
  9.   Bytes.drop : Nat -> Bytes -> Bytes
  10.  Bytes.empty : Bytes
  11.  Bytes.flatten : Bytes -> Bytes
  12.  Bytes.fromBase16 : Bytes -> Either Text Bytes
  13.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  14.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  15.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  16.  Bytes.fromList : [Nat] -> Bytes
  17.  Bytes.size : Bytes -> Nat
  18.  Bytes.take : Nat -> Bytes -> Bytes
  19.  Bytes.toBase16 : Bytes -> Bytes
  20.  Bytes.toBase32 : Bytes -> Bytes
  21.  Bytes.toBase64 : Bytes -> Bytes
  22.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  23.  Bytes.toList : Bytes -> [Nat]
  24.  builtin type Char
  25.  Char.fromNat : Nat -> Char
  26.  Char.toNat : Char -> Nat
  27.  Char.toText : Char -> Text
  28.  builtin type Code
  29.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  30.  Code.dependencies : Code -> [Term]
  31.  Code.deserialize : Bytes -> Either Text Code
  32.  Code.isMissing : Term ->{IO} Boolean
  33.  Code.lookup : Term ->{IO} Optional Code
  34.  Code.serialize : Code -> Bytes
  35.  crypto.hash : HashAlgorithm -> a -> Bytes
  36.  builtin type crypto.HashAlgorithm
  37.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  38.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  39.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  40.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  41.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  42.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  43.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  44.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  45.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  46.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  47.  Debug.watch : Text -> a -> a
  48.  unique type Doc
  49.  Doc.Blob : Text -> Doc
  50.  Doc.Evaluate : Term -> Doc
  51.  Doc.Join : [Doc] -> Doc
  52.  Doc.Link : Link -> Doc
  53.  Doc.Signature : Term -> Doc
  54.  Doc.Source : Link -> Doc
  55.  type Either a b
  56.  Either.Left : a -> Either a b
  57.  Either.Right : b -> Either a b
  58.  ability Exception
  59.  Exception.raise : Failure ->{Exception} x
  60.  builtin type Float
  61.  Float.* : Float -> Float -> Float
  62.  Float.+ : Float -> Float -> Float
  63.  Float.- : Float -> Float -> Float
  64.  Float./ : Float -> Float -> Float
  65.  Float.abs : Float -> Float
  66.  Float.acos : Float -> Float
  67.  Float.acosh : Float -> Float
  68.  Float.asin : Float -> Float
  69.  Float.asinh : Float -> Float
  70.  Float.atan : Float -> Float
  71.  Float.atan2 : Float -> Float -> Float
  72.  Float.atanh : Float -> Float
  73.  Float.ceiling : Float -> Int
  74.  Float.cos : Float -> Float
  75.  Float.cosh : Float -> Float
  76.  Float.eq : Float -> Float -> Boolean
  77.  Float.exp : Float -> Float
  78.  Float.floor : Float -> Int
  79.  Float.fromText : Text -> Optional Float
  80.  Float.gt : Float -> Float -> Boolean
  81.  Float.gteq : Float -> Float -> Boolean
  82.  Float.log : Float -> Float
  83.  Float.logBase : Float -> Float -> Float
  84.  Float.lt : Float -> Float -> Boolean
  85.  Float.lteq : Float -> Float -> Boolean
  86.  Float.max : Float -> Float -> Float
  87.  Float.min : Float -> Float -> Float
  88.  Float.pow : Float -> Float -> Float
  89.  Float.round : Float -> Int
  90.  Float.sin : Float -> Float
  91.  Float.sinh : Float -> Float
  92.  Float.sqrt : Float -> Float
  93.  Float.tan : Float -> Float
  94.  Float.tanh : Float -> Float
  95.  Float.toText : Float -> Text
  96.  Float.truncate : Float -> Int
  97.  builtin type Int
  98.  Int.* : Int -> Int -> Int
  99.  Int.+ : Int -> Int -> Int
  100. Int.- : Int -> Int -> Int
  101. Int./ : Int -> Int -> Int
  102. Int.and : Int -> Int -> Int
  103. Int.complement : Int -> Int
  104. Int.eq : Int -> Int -> Boolean
  105. Int.fromText : Text -> Optional Int
  106. Int.gt : Int -> Int -> Boolean
  107. Int.gteq : Int -> Int -> Boolean
  108. Int.increment : Int -> Int
  109. Int.isEven : Int -> Boolean
  110. Int.isOdd : Int -> Boolean
  111. Int.leadingZeros : Int -> Nat
  112. Int.lt : Int -> Int -> Boolean
  113. Int.lteq : Int -> Int -> Boolean
  114. Int.mod : Int -> Int -> Int
  115. Int.negate : Int -> Int
  116. Int.or : Int -> Int -> Int
  117. Int.popCount : Int -> Nat
  118. Int.pow : Int -> Nat -> Int
  119. Int.shiftLeft : Int -> Nat -> Int
  120. Int.shiftRight : Int -> Nat -> Int
  121. Int.signum : Int -> Int
  122. Int.toFloat : Int -> Float
  123. Int.toText : Int -> Text
  124. Int.trailingZeros : Int -> Nat
  125. Int.truncate0 : Int -> Nat
  126. Int.xor : Int -> Int -> Int
  127. unique type io2.BufferMode
  128. io2.BufferMode.BlockBuffering : BufferMode
  129. io2.BufferMode.LineBuffering : BufferMode
  130. io2.BufferMode.NoBuffering : BufferMode
  131. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  132. unique type io2.Failure
  133. io2.Failure.Failure : Type -> Text -> Any -> Failure
  134. unique type io2.FileMode
  135. io2.FileMode.Append : FileMode
  136. io2.FileMode.Read : FileMode
  137. io2.FileMode.ReadWrite : FileMode
  138. io2.FileMode.Write : FileMode
  139. builtin type io2.Handle
  140. builtin type io2.IO
  141. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  142. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  143. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  144. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  145. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  146. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  147. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  148. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  149. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  150. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  151. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  152. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  153. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  154. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  155. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  156. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  157. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  158. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  159. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  160. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  161. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  162. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  163. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  164. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  165. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  166. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  167. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  168. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  169. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  170. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  171. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  172. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  173. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  174. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  175. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  176. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  177. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  178. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  179. io2.IO.stdHandle : StdHandle -> Handle
  180. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  181. unique type io2.IOError
  182. io2.IOError.AlreadyExists : IOError
  183. io2.IOError.EOF : IOError
  184. io2.IOError.IllegalOperation : IOError
  185. io2.IOError.NoSuchThing : IOError
  186. io2.IOError.PermissionDenied : IOError
  187. io2.IOError.ResourceBusy : IOError
  188. io2.IOError.ResourceExhausted : IOError
  189. io2.IOError.UserError : IOError
  190. unique type io2.IOFailure
  191. builtin type io2.MVar
  192. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  193. io2.MVar.new : a ->{IO} MVar a
  194. io2.MVar.newEmpty : '{IO} MVar a
  195. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  196. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  197. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  198. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  199. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  200. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  201. io2.MVar.tryTake : MVar a ->{IO} Optional a
  202. unique type io2.SeekMode
  203. io2.SeekMode.AbsoluteSeek : SeekMode
  204. io2.SeekMode.RelativeSeek : SeekMode
  205. io2.SeekMode.SeekFromEnd : SeekMode
  206. builtin type io2.Socket
  207. unique type io2.StdHandle
  208. io2.StdHandle.StdErr : StdHandle
  209. io2.StdHandle.StdIn : StdHandle
  210. io2.StdHandle.StdOut : StdHandle
  211. builtin type io2.STM
  212. io2.STM.atomically : '{STM} a ->{IO} a
  213. io2.STM.retry : '{STM} a
  214. builtin type io2.ThreadId
  215. builtin type io2.Tls
  216. builtin type io2.Tls.Cipher
  217. builtin type io2.Tls.ClientConfig
  218. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  219. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  220. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  221. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  222. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  223. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  224. io2.Tls.encodeCert : SignedCert -> Bytes
  225. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  226. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  227. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  228. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  229. builtin type io2.Tls.PrivateKey
  230. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  231. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  232. builtin type io2.Tls.ServerConfig
  233. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  234. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  235. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  236. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  237. builtin type io2.Tls.SignedCert
  238. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  239. builtin type io2.Tls.Version
  240. unique type io2.TlsFailure
  241. builtin type io2.TVar
  242. io2.TVar.new : a ->{STM} TVar a
  243. io2.TVar.newIO : a ->{IO} TVar a
  244. io2.TVar.read : TVar a ->{STM} a
  245. io2.TVar.readIO : TVar a ->{IO} a
  246. io2.TVar.swap : TVar a -> a ->{STM} a
  247. io2.TVar.write : TVar a -> a ->{STM} ()
  248. unique type IsPropagated
  249. IsPropagated.IsPropagated : IsPropagated
  250. unique type IsTest
  251. IsTest.IsTest : IsTest
  252. unique type Link
  253. builtin type Link.Term
  254. Link.Term : Term -> Link
  255. builtin type Link.Type
  256. Link.Type : Type -> Link
  257. builtin type List
  258. List.++ : [a] -> [a] -> [a]
  259. List.+: : a -> [a] -> [a]
  260. List.:+ : [a] -> a -> [a]
  261. List.at : Nat -> [a] -> Optional a
  262. List.cons : a -> [a] -> [a]
  263. List.drop : Nat -> [a] -> [a]
  264. List.empty : [a]
  265. List.size : [a] -> Nat
  266. List.snoc : [a] -> a -> [a]
  267. List.take : Nat -> [a] -> [a]
  268. metadata.isPropagated : IsPropagated
  269. metadata.isTest : IsTest
  270. builtin type Nat
  271. Nat.* : Nat -> Nat -> Nat
  272. Nat.+ : Nat -> Nat -> Nat
  273. Nat./ : Nat -> Nat -> Nat
  274. Nat.and : Nat -> Nat -> Nat
  275. Nat.complement : Nat -> Nat
  276. Nat.drop : Nat -> Nat -> Nat
  277. Nat.eq : Nat -> Nat -> Boolean
  278. Nat.fromText : Text -> Optional Nat
  279. Nat.gt : Nat -> Nat -> Boolean
  280. Nat.gteq : Nat -> Nat -> Boolean
  281. Nat.increment : Nat -> Nat
  282. Nat.isEven : Nat -> Boolean
  283. Nat.isOdd : Nat -> Boolean
  284. Nat.leadingZeros : Nat -> Nat
  285. Nat.lt : Nat -> Nat -> Boolean
  286. Nat.lteq : Nat -> Nat -> Boolean
  287. Nat.mod : Nat -> Nat -> Nat
  288. Nat.or : Nat -> Nat -> Nat
  289. Nat.popCount : Nat -> Nat
  290. Nat.pow : Nat -> Nat -> Nat
  291. Nat.shiftLeft : Nat -> Nat -> Nat
  292. Nat.shiftRight : Nat -> Nat -> Nat
  293. Nat.sub : Nat -> Nat -> Int
  294. Nat.toFloat : Nat -> Float
  295. Nat.toInt : Nat -> Int
  296. Nat.toText : Nat -> Text
  297. Nat.trailingZeros : Nat -> Nat
  298. Nat.xor : Nat -> Nat -> Nat
  299. type Optional a
  300. Optional.None : Optional a
  301. Optional.Some : a -> Optional a
  302. builtin type Request
  303. type SeqView a b
  304. SeqView.VElem : a -> b -> SeqView a b
  305. SeqView.VEmpty : SeqView a b
  306. unique type Test.Result
  307. Test.Result.Fail : Text -> Result
  308. Test.Result.Ok : Text -> Result
  309. builtin type Text
  310. Text.!= : Text -> Text -> Boolean
  311. Text.++ : Text -> Text -> Text
  312. Text.drop : Nat -> Text -> Text
  313. Text.empty : Text
  314. Text.eq : Text -> Text -> Boolean
  315. Text.fromCharList : [Char] -> Text
  316. Text.fromUtf8.impl : Bytes -> Either Failure Text
  317. Text.gt : Text -> Text -> Boolean
  318. Text.gteq : Text -> Text -> Boolean
  319. Text.lt : Text -> Text -> Boolean
  320. Text.lteq : Text -> Text -> Boolean
  321. Text.repeat : Nat -> Text -> Text
  322. Text.size : Text -> Nat
  323. Text.take : Nat -> Text -> Text
  324. Text.toCharList : Text -> [Char]
  325. Text.toUtf8 : Text -> Bytes
  326. Text.uncons : Text -> Optional (Char, Text)
  327. Text.unsnoc : Text -> Optional (Text, Char)
  328. todo : a -> b
  329. type Tuple a b
  330. Tuple.Cons : a -> b -> Tuple a b
  331. type Unit
  332. Unit.Unit : ()
  333. Universal.< : a -> a -> Boolean
  334. Universal.<= : a -> a -> Boolean
  335. Universal.== : a -> a -> Boolean
  336. Universal.> : a -> a -> Boolean
  337. Universal.>= : a -> a -> Boolean
  338. Universal.compare : a -> a -> Int
  339. builtin type Value
  340. Value.dependencies : Value -> [Term]
  341. Value.deserialize : Bytes -> Either Text Value
  342. Value.load : Value ->{IO} Either [Term] a
  343. Value.serialize : Value -> Bytes
  344. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type Int
    2.  Int.*          : Int -> Int -> Int
    3.  Int.+          : Int -> Int -> Int
    4.  Int.-          : Int -> Int -> Int
    5.  Int./          : Int -> Int -> Int
    6.  Int.and        : Int -> Int -> Int
    7.  Int.complement : Int -> Int
    8.  Int.eq         : Int -> Int -> Boolean
    9.  Float.tanh     : Float -> Float
    10. Float.toText   : Float -> Text
    11. Float.truncate : Float -> Int
  
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

  1.  Float.tanh : Float -> Float
  2.  Float.toText : Float -> Text
  3.  Float.truncate : Float -> Int
  4.  builtin type Int
  5.  Int.* : Int -> Int -> Int
  6.  Int.+ : Int -> Int -> Int
  7.  Int.- : Int -> Int -> Int
  8.  Int./ : Int -> Int -> Int
  9.  Int.and : Int -> Int -> Int
  10. Int.complement : Int -> Int
  11. Int.eq : Int -> Int -> Boolean
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
