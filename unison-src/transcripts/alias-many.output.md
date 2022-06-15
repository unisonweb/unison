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
  120. builtin type ImmutableArray
  121. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  122. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  123. builtin type ImmutableByteArray
  124. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  125. ImmutableByteArray.read16 : ImmutableByteArray
                                   -> Nat
                                   ->{Exception} Nat
  126. ImmutableByteArray.read32 : ImmutableByteArray
                                   -> Nat
                                   ->{Exception} Nat
  127. ImmutableByteArray.read64 : ImmutableByteArray
                                   -> Nat
                                   ->{Exception} Nat
  128. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  129. builtin type Int
  130. Int.* : Int -> Int -> Int
  131. Int.+ : Int -> Int -> Int
  132. Int.- : Int -> Int -> Int
  133. Int./ : Int -> Int -> Int
  134. Int.and : Int -> Int -> Int
  135. Int.complement : Int -> Int
  136. Int.eq : Int -> Int -> Boolean
  137. Int.fromRepresentation : Nat -> Int
  138. Int.fromText : Text -> Optional Int
  139. Int.gt : Int -> Int -> Boolean
  140. Int.gteq : Int -> Int -> Boolean
  141. Int.increment : Int -> Int
  142. Int.isEven : Int -> Boolean
  143. Int.isOdd : Int -> Boolean
  144. Int.leadingZeros : Int -> Nat
  145. Int.lt : Int -> Int -> Boolean
  146. Int.lteq : Int -> Int -> Boolean
  147. Int.mod : Int -> Int -> Int
  148. Int.negate : Int -> Int
  149. Int.or : Int -> Int -> Int
  150. Int.popCount : Int -> Nat
  151. Int.pow : Int -> Nat -> Int
  152. Int.shiftLeft : Int -> Nat -> Int
  153. Int.shiftRight : Int -> Nat -> Int
  154. Int.signum : Int -> Int
  155. Int.toFloat : Int -> Float
  156. Int.toRepresentation : Int -> Nat
  157. Int.toText : Int -> Text
  158. Int.trailingZeros : Int -> Nat
  159. Int.truncate0 : Int -> Nat
  160. Int.xor : Int -> Int -> Int
  161. unique type io2.ArrayFailure
  162. unique type io2.BufferMode
  163. io2.BufferMode.BlockBuffering : BufferMode
  164. io2.BufferMode.LineBuffering : BufferMode
  165. io2.BufferMode.NoBuffering : BufferMode
  166. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  167. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  168. io2.Clock.internals.nsec : TimeSpec -> Nat
  169. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  170. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  171. io2.Clock.internals.sec : TimeSpec -> Int
  172. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  173. builtin type io2.Clock.internals.TimeSpec
  174. unique type io2.Failure
  175. io2.Failure.Failure : Type -> Text -> Any -> Failure
  176. unique type io2.FileMode
  177. io2.FileMode.Append : FileMode
  178. io2.FileMode.Read : FileMode
  179. io2.FileMode.ReadWrite : FileMode
  180. io2.FileMode.Write : FileMode
  181. builtin type io2.Handle
  182. builtin type io2.IO
  183. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  184. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  185. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  186. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  187. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  188. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  189. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  190. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  191. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  192. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  193. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  194. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  195. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  196. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  197. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  198. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  199. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  200. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  201. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  202. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  203. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  204. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  205. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  206. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  207. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  208. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  209. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  210. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  211. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  212. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  213. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  214. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  215. io2.IO.ref : a ->{IO} Ref {IO} a
  216. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  217. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  218. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  219. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  220. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  221. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  222. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  223. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  224. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  225. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  226. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  227. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  228. io2.IO.stdHandle : StdHandle -> Handle
  229. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  230. io2.IO.systemTimeMicroseconds : '{IO} Int
  231. unique type io2.IOError
  232. io2.IOError.AlreadyExists : IOError
  233. io2.IOError.EOF : IOError
  234. io2.IOError.IllegalOperation : IOError
  235. io2.IOError.NoSuchThing : IOError
  236. io2.IOError.PermissionDenied : IOError
  237. io2.IOError.ResourceBusy : IOError
  238. io2.IOError.ResourceExhausted : IOError
  239. io2.IOError.UserError : IOError
  240. unique type io2.IOFailure
  241. builtin type io2.MVar
  242. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  243. io2.MVar.new : a ->{IO} MVar a
  244. io2.MVar.newEmpty : '{IO} MVar a
  245. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  246. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  247. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  248. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  249. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  250. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  251. io2.MVar.tryTake : MVar a ->{IO} Optional a
  252. unique type io2.SeekMode
  253. io2.SeekMode.AbsoluteSeek : SeekMode
  254. io2.SeekMode.RelativeSeek : SeekMode
  255. io2.SeekMode.SeekFromEnd : SeekMode
  256. builtin type io2.Socket
  257. unique type io2.StdHandle
  258. io2.StdHandle.StdErr : StdHandle
  259. io2.StdHandle.StdIn : StdHandle
  260. io2.StdHandle.StdOut : StdHandle
  261. builtin type io2.STM
  262. io2.STM.atomically : '{STM} a ->{IO} a
  263. io2.STM.retry : '{STM} a
  264. builtin type io2.ThreadId
  265. builtin type io2.Tls
  266. builtin type io2.Tls.Cipher
  267. builtin type io2.Tls.ClientConfig
  268. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  269. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  270. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  271. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  272. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  273. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  274. io2.Tls.encodeCert : SignedCert -> Bytes
  275. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  276. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  277. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  278. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  279. builtin type io2.Tls.PrivateKey
  280. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  281. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  282. builtin type io2.Tls.ServerConfig
  283. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  284. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  285. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  286. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  287. builtin type io2.Tls.SignedCert
  288. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  289. builtin type io2.Tls.Version
  290. unique type io2.TlsFailure
  291. builtin type io2.TVar
  292. io2.TVar.new : a ->{STM} TVar a
  293. io2.TVar.newIO : a ->{IO} TVar a
  294. io2.TVar.read : TVar a ->{STM} a
  295. io2.TVar.readIO : TVar a ->{IO} a
  296. io2.TVar.swap : TVar a -> a ->{STM} a
  297. io2.TVar.write : TVar a -> a ->{STM} ()
  298. io2.validateSandboxed : [Term] -> a -> Boolean
  299. unique type IsPropagated
  300. IsPropagated.IsPropagated : IsPropagated
  301. unique type IsTest
  302. IsTest.IsTest : IsTest
  303. unique type Link
  304. builtin type Link.Term
  305. Link.Term : Term -> Link
  306. Link.Term.toText : Term -> Text
  307. builtin type Link.Type
  308. Link.Type : Type -> Link
  309. builtin type List
  310. List.++ : [a] -> [a] -> [a]
  311. List.+: : a -> [a] -> [a]
  312. List.:+ : [a] -> a -> [a]
  313. List.at : Nat -> [a] -> Optional a
  314. List.cons : a -> [a] -> [a]
  315. List.drop : Nat -> [a] -> [a]
  316. List.empty : [a]
  317. List.size : [a] -> Nat
  318. List.snoc : [a] -> a -> [a]
  319. List.take : Nat -> [a] -> [a]
  320. metadata.isPropagated : IsPropagated
  321. metadata.isTest : IsTest
  322. builtin type MutableArray
  323. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  324. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  325. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  326. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  327. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  328. builtin type MutableByteArray
  329. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  330. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  331. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  332. MutableByteArray.read16 : MutableByteArray g
                                 -> Nat
                                 ->{g, Exception} Nat
  333. MutableByteArray.read32 : MutableByteArray g
                                 -> Nat
                                 ->{g, Exception} Nat
  334. MutableByteArray.read64 : MutableByteArray g
                                 -> Nat
                                 ->{g, Exception} Nat
  335. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  336. MutableByteArray.write16 : MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  337. MutableByteArray.write32 : MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  338. MutableByteArray.write64 : MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  339. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  340. builtin type Nat
  341. Nat.* : Nat -> Nat -> Nat
  342. Nat.+ : Nat -> Nat -> Nat
  343. Nat./ : Nat -> Nat -> Nat
  344. Nat.and : Nat -> Nat -> Nat
  345. Nat.complement : Nat -> Nat
  346. Nat.drop : Nat -> Nat -> Nat
  347. Nat.eq : Nat -> Nat -> Boolean
  348. Nat.fromText : Text -> Optional Nat
  349. Nat.gt : Nat -> Nat -> Boolean
  350. Nat.gteq : Nat -> Nat -> Boolean
  351. Nat.increment : Nat -> Nat
  352. Nat.isEven : Nat -> Boolean
  353. Nat.isOdd : Nat -> Boolean
  354. Nat.leadingZeros : Nat -> Nat
  355. Nat.lt : Nat -> Nat -> Boolean
  356. Nat.lteq : Nat -> Nat -> Boolean
  357. Nat.mod : Nat -> Nat -> Nat
  358. Nat.or : Nat -> Nat -> Nat
  359. Nat.popCount : Nat -> Nat
  360. Nat.pow : Nat -> Nat -> Nat
  361. Nat.shiftLeft : Nat -> Nat -> Nat
  362. Nat.shiftRight : Nat -> Nat -> Nat
  363. Nat.sub : Nat -> Nat -> Int
  364. Nat.toFloat : Nat -> Float
  365. Nat.toInt : Nat -> Int
  366. Nat.toText : Nat -> Text
  367. Nat.trailingZeros : Nat -> Nat
  368. Nat.xor : Nat -> Nat -> Nat
  369. structural type Optional a
  370. Optional.None : Optional a
  371. Optional.Some : a -> Optional a
  372. builtin type Ref
  373. Ref.read : Ref g a ->{g} a
  374. Ref.write : Ref g a -> a ->{g} ()
  375. builtin type Request
  376. builtin type Scope
  377. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  378. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  379. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  380. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  381. Scope.ref : a ->{Scope s} Ref {Scope s} a
  382. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  383. structural type SeqView a b
  384. SeqView.VElem : a -> b -> SeqView a b
  385. SeqView.VEmpty : SeqView a b
  386. Socket.toText : Socket -> Text
  387. unique type Test.Result
  388. Test.Result.Fail : Text -> Result
  389. Test.Result.Ok : Text -> Result
  390. builtin type Text
  391. Text.!= : Text -> Text -> Boolean
  392. Text.++ : Text -> Text -> Text
  393. Text.drop : Nat -> Text -> Text
  394. Text.empty : Text
  395. Text.eq : Text -> Text -> Boolean
  396. Text.fromCharList : [Char] -> Text
  397. Text.fromUtf8.impl : Bytes -> Either Failure Text
  398. Text.gt : Text -> Text -> Boolean
  399. Text.gteq : Text -> Text -> Boolean
  400. Text.lt : Text -> Text -> Boolean
  401. Text.lteq : Text -> Text -> Boolean
  402. Text.repeat : Nat -> Text -> Text
  403. Text.size : Text -> Nat
  404. Text.take : Nat -> Text -> Text
  405. Text.toCharList : Text -> [Char]
  406. Text.toUtf8 : Text -> Bytes
  407. Text.uncons : Text -> Optional (Char, Text)
  408. Text.unsnoc : Text -> Optional (Text, Char)
  409. ThreadId.toText : ThreadId -> Text
  410. todo : a -> b
  411. structural type Tuple a b
  412. Tuple.Cons : a -> b -> Tuple a b
  413. structural type Unit
  414. Unit.Unit : ()
  415. Universal.< : a -> a -> Boolean
  416. Universal.<= : a -> a -> Boolean
  417. Universal.== : a -> a -> Boolean
  418. Universal.> : a -> a -> Boolean
  419. Universal.>= : a -> a -> Boolean
  420. Universal.compare : a -> a -> Int
  421. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  422. builtin type Value
  423. Value.dependencies : Value -> [Term]
  424. Value.deserialize : Bytes -> Either Text Value
  425. Value.load : Value ->{IO} Either [Term] a
  426. Value.serialize : Value -> Bytes
  427. Value.value : a -> Value
  

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
