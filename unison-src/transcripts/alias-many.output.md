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
  121. builtin type ImmutableArray
  122. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  123. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  124. ImmutableArray.size : ImmutableArray a -> Nat
  125. builtin type ImmutableByteArray
  126. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  127. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  128. ImmutableByteArray.read24be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  129. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  130. ImmutableByteArray.read40be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  131. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  132. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  133. ImmutableByteArray.size : ImmutableByteArray -> Nat
  134. builtin type Int
  135. Int.* : Int -> Int -> Int
  136. Int.+ : Int -> Int -> Int
  137. Int.- : Int -> Int -> Int
  138. Int./ : Int -> Int -> Int
  139. Int.and : Int -> Int -> Int
  140. Int.complement : Int -> Int
  141. Int.eq : Int -> Int -> Boolean
  142. Int.fromRepresentation : Nat -> Int
  143. Int.fromText : Text -> Optional Int
  144. Int.gt : Int -> Int -> Boolean
  145. Int.gteq : Int -> Int -> Boolean
  146. Int.increment : Int -> Int
  147. Int.isEven : Int -> Boolean
  148. Int.isOdd : Int -> Boolean
  149. Int.leadingZeros : Int -> Nat
  150. Int.lt : Int -> Int -> Boolean
  151. Int.lteq : Int -> Int -> Boolean
  152. Int.mod : Int -> Int -> Int
  153. Int.negate : Int -> Int
  154. Int.or : Int -> Int -> Int
  155. Int.popCount : Int -> Nat
  156. Int.pow : Int -> Nat -> Int
  157. Int.shiftLeft : Int -> Nat -> Int
  158. Int.shiftRight : Int -> Nat -> Int
  159. Int.signum : Int -> Int
  160. Int.toFloat : Int -> Float
  161. Int.toRepresentation : Int -> Nat
  162. Int.toText : Int -> Text
  163. Int.trailingZeros : Int -> Nat
  164. Int.truncate0 : Int -> Nat
  165. Int.xor : Int -> Int -> Int
  166. unique type io2.ArithmeticFailure
  167. unique type io2.ArrayFailure
  168. unique type io2.BufferMode
  169. io2.BufferMode.BlockBuffering : BufferMode
  170. io2.BufferMode.LineBuffering : BufferMode
  171. io2.BufferMode.NoBuffering : BufferMode
  172. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  173. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  174. io2.Clock.internals.nsec : TimeSpec -> Nat
  175. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  176. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  177. io2.Clock.internals.sec : TimeSpec -> Int
  178. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  179. builtin type io2.Clock.internals.TimeSpec
  180. unique type io2.Failure
  181. io2.Failure.Failure : Type -> Text -> Any -> Failure
  182. unique type io2.FileMode
  183. io2.FileMode.Append : FileMode
  184. io2.FileMode.Read : FileMode
  185. io2.FileMode.ReadWrite : FileMode
  186. io2.FileMode.Write : FileMode
  187. builtin type io2.Handle
  188. builtin type io2.IO
  189. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  190. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  191. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  192. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  193. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  194. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  195. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  196. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  197. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  198. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  199. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  200. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  201. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  202. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  203. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  204. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  205. io2.IO.getChar.impl : Handle ->{IO} Either Failure Char
  206. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  207. io2.IO.getEcho.impl : Handle
                             ->{IO} Either Failure Boolean
  208. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  209. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  210. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  211. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  212. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  213. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  214. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  215. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  216. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  217. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  218. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  219. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  220. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  221. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  222. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  223. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  224. io2.IO.ref : a ->{IO} Ref {IO} a
  225. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  226. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  227. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  228. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  229. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  230. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  231. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  232. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  233. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  234. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  235. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  236. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  237. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  238. io2.IO.stdHandle : StdHandle -> Handle
  239. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  240. io2.IO.systemTimeMicroseconds : '{IO} Int
  241. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  242. unique type io2.IOError
  243. io2.IOError.AlreadyExists : IOError
  244. io2.IOError.EOF : IOError
  245. io2.IOError.IllegalOperation : IOError
  246. io2.IOError.NoSuchThing : IOError
  247. io2.IOError.PermissionDenied : IOError
  248. io2.IOError.ResourceBusy : IOError
  249. io2.IOError.ResourceExhausted : IOError
  250. io2.IOError.UserError : IOError
  251. unique type io2.IOFailure
  252. unique type io2.MiscFailure
  253. builtin type io2.MVar
  254. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  255. io2.MVar.new : a ->{IO} MVar a
  256. io2.MVar.newEmpty : '{IO} MVar a
  257. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  258. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  259. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  260. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  261. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  262. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  263. io2.MVar.tryTake : MVar a ->{IO} Optional a
  264. unique type io2.RuntimeFailure
  265. unique type io2.SeekMode
  266. io2.SeekMode.AbsoluteSeek : SeekMode
  267. io2.SeekMode.RelativeSeek : SeekMode
  268. io2.SeekMode.SeekFromEnd : SeekMode
  269. builtin type io2.Socket
  270. unique type io2.StdHandle
  271. io2.StdHandle.StdErr : StdHandle
  272. io2.StdHandle.StdIn : StdHandle
  273. io2.StdHandle.StdOut : StdHandle
  274. builtin type io2.STM
  275. io2.STM.atomically : '{STM} a ->{IO} a
  276. io2.STM.retry : '{STM} a
  277. unique type io2.STMFailure
  278. builtin type io2.ThreadId
  279. builtin type io2.Tls
  280. builtin type io2.Tls.Cipher
  281. builtin type io2.Tls.ClientConfig
  282. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  283. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  284. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  285. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  286. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  287. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  288. io2.Tls.encodeCert : SignedCert -> Bytes
  289. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  290. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  291. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  292. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  293. builtin type io2.Tls.PrivateKey
  294. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  295. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  296. builtin type io2.Tls.ServerConfig
  297. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  298. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  299. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  300. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  301. builtin type io2.Tls.SignedCert
  302. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  303. builtin type io2.Tls.Version
  304. unique type io2.TlsFailure
  305. builtin type io2.TVar
  306. io2.TVar.new : a ->{STM} TVar a
  307. io2.TVar.newIO : a ->{IO} TVar a
  308. io2.TVar.read : TVar a ->{STM} a
  309. io2.TVar.readIO : TVar a ->{IO} a
  310. io2.TVar.swap : TVar a -> a ->{STM} a
  311. io2.TVar.write : TVar a -> a ->{STM} ()
  312. io2.validateSandboxed : [Term] -> a -> Boolean
  313. unique type IsPropagated
  314. IsPropagated.IsPropagated : IsPropagated
  315. unique type IsTest
  316. IsTest.IsTest : IsTest
  317. unique type Link
  318. builtin type Link.Term
  319. Link.Term : Term -> Link
  320. Link.Term.toText : Term -> Text
  321. builtin type Link.Type
  322. Link.Type : Type -> Link
  323. builtin type List
  324. List.++ : [a] -> [a] -> [a]
  325. List.+: : a -> [a] -> [a]
  326. List.:+ : [a] -> a -> [a]
  327. List.at : Nat -> [a] -> Optional a
  328. List.cons : a -> [a] -> [a]
  329. List.drop : Nat -> [a] -> [a]
  330. List.empty : [a]
  331. List.size : [a] -> Nat
  332. List.snoc : [a] -> a -> [a]
  333. List.take : Nat -> [a] -> [a]
  334. metadata.isPropagated : IsPropagated
  335. metadata.isTest : IsTest
  336. builtin type MutableArray
  337. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  338. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  339. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  340. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  341. MutableArray.size : MutableArray g a -> Nat
  342. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  343. builtin type MutableByteArray
  344. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  345. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  346. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  347. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  348. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  349. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  350. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  351. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  352. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  353. MutableByteArray.size : MutableByteArray g -> Nat
  354. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  355. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  356. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  357. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  358. builtin type Nat
  359. Nat.* : Nat -> Nat -> Nat
  360. Nat.+ : Nat -> Nat -> Nat
  361. Nat./ : Nat -> Nat -> Nat
  362. Nat.and : Nat -> Nat -> Nat
  363. Nat.complement : Nat -> Nat
  364. Nat.drop : Nat -> Nat -> Nat
  365. Nat.eq : Nat -> Nat -> Boolean
  366. Nat.fromText : Text -> Optional Nat
  367. Nat.gt : Nat -> Nat -> Boolean
  368. Nat.gteq : Nat -> Nat -> Boolean
  369. Nat.increment : Nat -> Nat
  370. Nat.isEven : Nat -> Boolean
  371. Nat.isOdd : Nat -> Boolean
  372. Nat.leadingZeros : Nat -> Nat
  373. Nat.lt : Nat -> Nat -> Boolean
  374. Nat.lteq : Nat -> Nat -> Boolean
  375. Nat.mod : Nat -> Nat -> Nat
  376. Nat.or : Nat -> Nat -> Nat
  377. Nat.popCount : Nat -> Nat
  378. Nat.pow : Nat -> Nat -> Nat
  379. Nat.shiftLeft : Nat -> Nat -> Nat
  380. Nat.shiftRight : Nat -> Nat -> Nat
  381. Nat.sub : Nat -> Nat -> Int
  382. Nat.toFloat : Nat -> Float
  383. Nat.toInt : Nat -> Int
  384. Nat.toText : Nat -> Text
  385. Nat.trailingZeros : Nat -> Nat
  386. Nat.xor : Nat -> Nat -> Nat
  387. structural type Optional a
  388. Optional.None : Optional a
  389. Optional.Some : a -> Optional a
  390. builtin type Pattern
  391. Pattern.capture : Pattern a -> Pattern a
  392. Pattern.isMatch : Pattern a -> a -> Boolean
  393. Pattern.join : [Pattern a] -> Pattern a
  394. Pattern.many : Pattern a -> Pattern a
  395. Pattern.or : Pattern a -> Pattern a -> Pattern a
  396. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  397. Pattern.run : Pattern a -> a -> Optional ([a], a)
  398. builtin type Ref
  399. Ref.read : Ref g a ->{g} a
  400. Ref.write : Ref g a -> a ->{g} ()
  401. builtin type Request
  402. builtin type Scope
  403. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  404. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  405. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  406. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  407. Scope.ref : a ->{Scope s} Ref {Scope s} a
  408. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  409. structural type SeqView a b
  410. SeqView.VElem : a -> b -> SeqView a b
  411. SeqView.VEmpty : SeqView a b
  412. Socket.toText : Socket -> Text
  413. unique type Test.Result
  414. Test.Result.Fail : Text -> Result
  415. Test.Result.Ok : Text -> Result
  416. builtin type Text
  417. Text.!= : Text -> Text -> Boolean
  418. Text.++ : Text -> Text -> Text
  419. Text.drop : Nat -> Text -> Text
  420. Text.empty : Text
  421. Text.eq : Text -> Text -> Boolean
  422. Text.fromCharList : [Char] -> Text
  423. Text.fromUtf8.impl : Bytes -> Either Failure Text
  424. Text.gt : Text -> Text -> Boolean
  425. Text.gteq : Text -> Text -> Boolean
  426. Text.lt : Text -> Text -> Boolean
  427. Text.lteq : Text -> Text -> Boolean
  428. Text.patterns.anyChar : Pattern Text
  429. Text.patterns.charIn : [Char] -> Pattern Text
  430. Text.patterns.charRange : Char -> Char -> Pattern Text
  431. Text.patterns.digit : Pattern Text
  432. Text.patterns.eof : Pattern Text
  433. Text.patterns.letter : Pattern Text
  434. Text.patterns.literal : Text -> Pattern Text
  435. Text.patterns.notCharIn : [Char] -> Pattern Text
  436. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  437. Text.patterns.punctuation : Pattern Text
  438. Text.patterns.space : Pattern Text
  439. Text.repeat : Nat -> Text -> Text
  440. Text.reverse : Text -> Text
  441. Text.size : Text -> Nat
  442. Text.take : Nat -> Text -> Text
  443. Text.toCharList : Text -> [Char]
  444. Text.toLowercase : Text -> Text
  445. Text.toUppercase : Text -> Text
  446. Text.toUtf8 : Text -> Bytes
  447. Text.uncons : Text -> Optional (Char, Text)
  448. Text.unsnoc : Text -> Optional (Text, Char)
  449. ThreadId.toText : ThreadId -> Text
  450. todo : a -> b
  451. structural type Tuple a b
  452. Tuple.Cons : a -> b -> Tuple a b
  453. structural type Unit
  454. Unit.Unit : ()
  455. Universal.< : a -> a -> Boolean
  456. Universal.<= : a -> a -> Boolean
  457. Universal.== : a -> a -> Boolean
  458. Universal.> : a -> a -> Boolean
  459. Universal.>= : a -> a -> Boolean
  460. Universal.compare : a -> a -> Int
  461. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  462. builtin type Value
  463. Value.dependencies : Value -> [Term]
  464. Value.deserialize : Bytes -> Either Text Value
  465. Value.load : Value ->{IO} Either [Term] a
  466. Value.serialize : Value -> Bytes
  467. Value.value : a -> Value
  

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
