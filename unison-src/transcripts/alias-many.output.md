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
  26.  builtin type Code
  27.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  28.  Code.dependencies : Code -> [Term]
  29.  Code.deserialize : Bytes -> Either Text Code
  30.  Code.isMissing : Term ->{IO} Boolean
  31.  Code.lookup : Term ->{IO} Optional Code
  32.  Code.serialize : Code -> Bytes
  33.  Debug.watch : Text -> a -> a
  34.  unique type Doc
  35.  Doc.Blob : Text -> Doc
  36.  Doc.Evaluate : Term -> Doc
  37.  Doc.Join : [Doc] -> Doc
  38.  Doc.Link : Link -> Doc
  39.  Doc.Signature : Term -> Doc
  40.  Doc.Source : Link -> Doc
  41.  type Either a b
  42.  Either.Left : a -> Either a b
  43.  Either.Right : b -> Either a b
  44.  builtin type Float
  45.  Float.* : Float -> Float -> Float
  46.  Float.+ : Float -> Float -> Float
  47.  Float.- : Float -> Float -> Float
  48.  Float./ : Float -> Float -> Float
  49.  Float.abs : Float -> Float
  50.  Float.acos : Float -> Float
  51.  Float.acosh : Float -> Float
  52.  Float.asin : Float -> Float
  53.  Float.asinh : Float -> Float
  54.  Float.atan : Float -> Float
  55.  Float.atan2 : Float -> Float -> Float
  56.  Float.atanh : Float -> Float
  57.  Float.ceiling : Float -> Int
  58.  Float.cos : Float -> Float
  59.  Float.cosh : Float -> Float
  60.  Float.eq : Float -> Float -> Boolean
  61.  Float.exp : Float -> Float
  62.  Float.floor : Float -> Int
  63.  Float.fromText : Text -> Optional Float
  64.  Float.gt : Float -> Float -> Boolean
  65.  Float.gteq : Float -> Float -> Boolean
  66.  Float.log : Float -> Float
  67.  Float.logBase : Float -> Float -> Float
  68.  Float.lt : Float -> Float -> Boolean
  69.  Float.lteq : Float -> Float -> Boolean
  70.  Float.max : Float -> Float -> Float
  71.  Float.min : Float -> Float -> Float
  72.  Float.pow : Float -> Float -> Float
  73.  Float.round : Float -> Int
  74.  Float.sin : Float -> Float
  75.  Float.sinh : Float -> Float
  76.  Float.sqrt : Float -> Float
  77.  Float.tan : Float -> Float
  78.  Float.tanh : Float -> Float
  79.  Float.toText : Float -> Text
  80.  Float.truncate : Float -> Int
  81.  builtin type Int
  82.  Int.* : Int -> Int -> Int
  83.  Int.+ : Int -> Int -> Int
  84.  Int.- : Int -> Int -> Int
  85.  Int./ : Int -> Int -> Int
  86.  Int.and : Int -> Int -> Int
  87.  Int.complement : Int -> Int
  88.  Int.eq : Int -> Int -> Boolean
  89.  Int.fromText : Text -> Optional Int
  90.  Int.gt : Int -> Int -> Boolean
  91.  Int.gteq : Int -> Int -> Boolean
  92.  Int.increment : Int -> Int
  93.  Int.isEven : Int -> Boolean
  94.  Int.isOdd : Int -> Boolean
  95.  Int.leadingZeros : Int -> Nat
  96.  Int.lt : Int -> Int -> Boolean
  97.  Int.lteq : Int -> Int -> Boolean
  98.  Int.mod : Int -> Int -> Int
  99.  Int.negate : Int -> Int
  100. Int.or : Int -> Int -> Int
  101. Int.popCount : Int -> Nat
  102. Int.pow : Int -> Nat -> Int
  103. Int.shiftLeft : Int -> Nat -> Int
  104. Int.shiftRight : Int -> Nat -> Int
  105. Int.signum : Int -> Int
  106. Int.toFloat : Int -> Float
  107. Int.toText : Int -> Text
  108. Int.trailingZeros : Int -> Nat
  109. Int.truncate0 : Int -> Nat
  110. Int.xor : Int -> Int -> Int
  111. unique type IsPropagated
  112. IsPropagated.IsPropagated : IsPropagated
  113. unique type IsTest
  114. IsTest.IsTest : IsTest
  115. unique type Link
  116. builtin type Link.Term
  117. Link.Term : Term -> Link
  118. builtin type Link.Type
  119. Link.Type : Type -> Link
  120. builtin type List
  121. List.++ : [a] -> [a] -> [a]
  122. List.+: : a -> [a] -> [a]
  123. List.:+ : [a] -> a -> [a]
  124. List.at : Nat -> [a] -> Optional a
  125. List.cons : a -> [a] -> [a]
  126. List.drop : Nat -> [a] -> [a]
  127. List.empty : [a]
  128. List.size : [a] -> Nat
  129. List.snoc : [a] -> a -> [a]
  130. List.take : Nat -> [a] -> [a]
  131. builtin type Nat
  132. Nat.* : Nat -> Nat -> Nat
  133. Nat.+ : Nat -> Nat -> Nat
  134. Nat./ : Nat -> Nat -> Nat
  135. Nat.and : Nat -> Nat -> Nat
  136. Nat.complement : Nat -> Nat
  137. Nat.drop : Nat -> Nat -> Nat
  138. Nat.eq : Nat -> Nat -> Boolean
  139. Nat.fromText : Text -> Optional Nat
  140. Nat.gt : Nat -> Nat -> Boolean
  141. Nat.gteq : Nat -> Nat -> Boolean
  142. Nat.increment : Nat -> Nat
  143. Nat.isEven : Nat -> Boolean
  144. Nat.isOdd : Nat -> Boolean
  145. Nat.leadingZeros : Nat -> Nat
  146. Nat.lt : Nat -> Nat -> Boolean
  147. Nat.lteq : Nat -> Nat -> Boolean
  148. Nat.mod : Nat -> Nat -> Nat
  149. Nat.or : Nat -> Nat -> Nat
  150. Nat.popCount : Nat -> Nat
  151. Nat.pow : Nat -> Nat -> Nat
  152. Nat.shiftLeft : Nat -> Nat -> Nat
  153. Nat.shiftRight : Nat -> Nat -> Nat
  154. Nat.sub : Nat -> Nat -> Int
  155. Nat.toFloat : Nat -> Float
  156. Nat.toInt : Nat -> Int
  157. Nat.toText : Nat -> Text
  158. Nat.trailingZeros : Nat -> Nat
  159. Nat.xor : Nat -> Nat -> Nat
  160. type Optional a
  161. Optional.None : Optional a
  162. Optional.Some : a -> Optional a
  163. builtin type Request
  164. type SeqView a b
  165. SeqView.VElem : a -> b -> SeqView a b
  166. SeqView.VEmpty : SeqView a b
  167. unique type Test.Result
  168. Test.Result.Fail : Text -> Result
  169. Test.Result.Ok : Text -> Result
  170. builtin type Text
  171. Text.!= : Text -> Text -> Boolean
  172. Text.++ : Text -> Text -> Text
  173. Text.alignCenterWith : Nat -> Char -> Text -> Text
  174. Text.alignLeftWith : Nat -> Char -> Text -> Text
  175. Text.alignRightWith : Nat -> Char -> Text -> Text
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
  241. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  242. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  243. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  244. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  245. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  246. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  247. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  248. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  249. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  250. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  251. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  252. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  253. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  254. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  255. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  256. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  257. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  258. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  259. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  260. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  261. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  262. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  263. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  264. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  265. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  266. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  267. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  268. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  269. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  270. io2.IO.stdHandle : StdHandle -> Handle
  271. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  272. unique type io2.IOError
  273. io2.IOError.AlreadyExists : IOError
  274. io2.IOError.EOF : IOError
  275. io2.IOError.IllegalOperation : IOError
  276. io2.IOError.NoSuchThing : IOError
  277. io2.IOError.PermissionDenied : IOError
  278. io2.IOError.ResourceBusy : IOError
  279. io2.IOError.ResourceExhausted : IOError
  280. io2.IOError.UserError : IOError
  281. unique type io2.IOFailure
  282. builtin type io2.MVar
  283. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  284. io2.MVar.new : a ->{IO} MVar a
  285. io2.MVar.newEmpty : '{IO} MVar a
  286. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  287. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  288. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  289. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  290. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  291. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  292. io2.MVar.tryTake : MVar a ->{IO} Optional a
  293. builtin type io2.STM
  294. io2.STM.atomically : '{STM} a ->{IO} a
  295. io2.STM.retry : '{STM} a
  296. unique type io2.SeekMode
  297. io2.SeekMode.AbsoluteSeek : SeekMode
  298. io2.SeekMode.RelativeSeek : SeekMode
  299. io2.SeekMode.SeekFromEnd : SeekMode
  300. builtin type io2.Socket
  301. unique type io2.StdHandle
  302. io2.StdHandle.StdErr : StdHandle
  303. io2.StdHandle.StdIn : StdHandle
  304. io2.StdHandle.StdOut : StdHandle
  305. io2.TLS.ClientConfig.ciphers.set : [##Tls.Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  306. builtin type io2.TVar
  307. io2.TVar.new : a ->{STM} TVar a
  308. io2.TVar.newIO : a ->{IO} TVar a
  309. io2.TVar.read : TVar a ->{STM} a
  310. io2.TVar.readIO : TVar a ->{IO} a
  311. io2.TVar.swap : TVar a -> a ->{STM} a
  312. io2.TVar.write : TVar a -> a ->{STM} ()
  313. builtin type io2.ThreadId
  314. builtin type io2.Tls
  315. builtin type io2.Tls.ClientConfig
  316. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  317. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  318. io2.Tls.ClientConfig.versions.set : [##Tls.Version]
                                           -> ClientConfig
                                           -> ClientConfig
  319. builtin type io2.Tls.PrivateKey
  320. builtin type io2.Tls.ServerConfig
  321. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  322. io2.Tls.ServerConfig.ciphers.set : [##Tls.Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  323. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  324. io2.Tls.ServerConfig.versions.set : [##Tls.Version]
                                           -> ServerConfig
                                           -> ServerConfig
  325. builtin type io2.Tls.SignedCert
  326. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  327. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  328. io2.Tls.encodeCert : SignedCert -> Bytes
  329. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  330. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  331. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  332. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  333. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  334. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  335. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  336. unique type io2.TlsFailure
  337. metadata.isPropagated : IsPropagated
  338. metadata.isTest : IsTest
  339. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.isOdd        : Int -> Boolean
    2.  Int.leadingZeros : Int -> Nat
    3.  Int.lt           : Int -> Int -> Boolean
    4.  Int.lteq         : Int -> Int -> Boolean
    5.  Int.mod          : Int -> Int -> Int
    6.  Int.negate       : Int -> Int
    7.  Int.or           : Int -> Int -> Int
    8.  Int.popCount     : Int -> Nat
    9.  Int.pow          : Int -> Nat -> Int
    10. Int.shiftLeft    : Int -> Nat -> Int
    11. Int.shiftRight   : Int -> Nat -> Int
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar

.runar> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  3.  List.any : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{g} Boolean) ->{g} [a] ->{g} [a]
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
                           ->{g} [a]
                           ->{g} Boolean
    3.  List.any           : (a ->{g} Boolean)
                           ->{g} [a]
                           ->{g} Boolean
    4.  List.chunk         : Nat -> [a] -> [[a]]
    5.  List.chunksOf      : Nat -> [a] -> [[a]]
    6.  List.dropWhile     : (a ->{g} Boolean)
                           ->{g} [a]
                           ->{g} [a]
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

  1.  Int.isOdd : Int -> Boolean
  2.  Int.leadingZeros : Int -> Nat
  3.  Int.lt : Int -> Int -> Boolean
  4.  Int.lteq : Int -> Int -> Boolean
  5.  Int.mod : Int -> Int -> Int
  6.  Int.negate : Int -> Int
  7.  Int.or : Int -> Int -> Int
  8.  Int.popCount : Int -> Nat
  9.  Int.pow : Int -> Nat -> Int
  10. Int.shiftLeft : Int -> Nat -> Int
  11. Int.shiftRight : Int -> Nat -> Int
  12. List.adjacentPairs : [a] -> [(a, a)]
  13. List.all : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  14. List.any : (a ->{g} Boolean) ->{g} [a] ->{g} Boolean
  15. List.chunk : Nat -> [a] -> [[a]]
  16. List.chunksOf : Nat -> [a] -> [[a]]
  17. List.dropWhile : (a ->{g} Boolean) ->{g} [a] ->{g} [a]
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
