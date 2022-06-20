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
  124. builtin type ImmutableByteArray
  125. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  126. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  127. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  128. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  129. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  130. builtin type Int
  131. Int.* : Int -> Int -> Int
  132. Int.+ : Int -> Int -> Int
  133. Int.- : Int -> Int -> Int
  134. Int./ : Int -> Int -> Int
  135. Int.and : Int -> Int -> Int
  136. Int.complement : Int -> Int
  137. Int.eq : Int -> Int -> Boolean
  138. Int.fromRepresentation : Nat -> Int
  139. Int.fromText : Text -> Optional Int
  140. Int.gt : Int -> Int -> Boolean
  141. Int.gteq : Int -> Int -> Boolean
  142. Int.increment : Int -> Int
  143. Int.isEven : Int -> Boolean
  144. Int.isOdd : Int -> Boolean
  145. Int.leadingZeros : Int -> Nat
  146. Int.lt : Int -> Int -> Boolean
  147. Int.lteq : Int -> Int -> Boolean
  148. Int.mod : Int -> Int -> Int
  149. Int.negate : Int -> Int
  150. Int.or : Int -> Int -> Int
  151. Int.popCount : Int -> Nat
  152. Int.pow : Int -> Nat -> Int
  153. Int.shiftLeft : Int -> Nat -> Int
  154. Int.shiftRight : Int -> Nat -> Int
  155. Int.signum : Int -> Int
  156. Int.toFloat : Int -> Float
  157. Int.toRepresentation : Int -> Nat
  158. Int.toText : Int -> Text
  159. Int.trailingZeros : Int -> Nat
  160. Int.truncate0 : Int -> Nat
  161. Int.xor : Int -> Int -> Int
  162. unique type io2.ArrayFailure
  163. unique type io2.BufferMode
  164. io2.BufferMode.BlockBuffering : BufferMode
  165. io2.BufferMode.LineBuffering : BufferMode
  166. io2.BufferMode.NoBuffering : BufferMode
  167. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  168. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  169. io2.Clock.internals.nsec : TimeSpec -> Nat
  170. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  171. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  172. io2.Clock.internals.sec : TimeSpec -> Int
  173. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  174. builtin type io2.Clock.internals.TimeSpec
  175. unique type io2.Failure
  176. io2.Failure.Failure : Type -> Text -> Any -> Failure
  177. unique type io2.FileMode
  178. io2.FileMode.Append : FileMode
  179. io2.FileMode.Read : FileMode
  180. io2.FileMode.ReadWrite : FileMode
  181. io2.FileMode.Write : FileMode
  182. builtin type io2.Handle
  183. builtin type io2.IO
  184. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  185. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  186. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  187. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  188. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  189. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  190. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  191. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  192. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  193. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  194. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  195. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  196. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  197. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  198. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  199. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  200. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  201. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  202. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  203. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  204. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  205. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  206. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  207. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  208. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  209. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  210. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  211. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  212. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  213. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  214. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  215. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  216. io2.IO.ref : a ->{IO} Ref {IO} a
  217. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  218. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  219. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  220. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  221. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  222. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  223. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  224. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  225. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  226. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  227. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  228. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  229. io2.IO.stdHandle : StdHandle -> Handle
  230. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  231. io2.IO.systemTimeMicroseconds : '{IO} Int
  232. unique type io2.IOError
  233. io2.IOError.AlreadyExists : IOError
  234. io2.IOError.EOF : IOError
  235. io2.IOError.IllegalOperation : IOError
  236. io2.IOError.NoSuchThing : IOError
  237. io2.IOError.PermissionDenied : IOError
  238. io2.IOError.ResourceBusy : IOError
  239. io2.IOError.ResourceExhausted : IOError
  240. io2.IOError.UserError : IOError
  241. unique type io2.IOFailure
  242. builtin type io2.MVar
  243. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  244. io2.MVar.new : a ->{IO} MVar a
  245. io2.MVar.newEmpty : '{IO} MVar a
  246. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  247. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  248. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  249. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  250. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  251. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  252. io2.MVar.tryTake : MVar a ->{IO} Optional a
  253. unique type io2.SeekMode
  254. io2.SeekMode.AbsoluteSeek : SeekMode
  255. io2.SeekMode.RelativeSeek : SeekMode
  256. io2.SeekMode.SeekFromEnd : SeekMode
  257. builtin type io2.Socket
  258. unique type io2.StdHandle
  259. io2.StdHandle.StdErr : StdHandle
  260. io2.StdHandle.StdIn : StdHandle
  261. io2.StdHandle.StdOut : StdHandle
  262. builtin type io2.STM
  263. io2.STM.atomically : '{STM} a ->{IO} a
  264. io2.STM.retry : '{STM} a
  265. builtin type io2.ThreadId
  266. builtin type io2.Tls
  267. builtin type io2.Tls.Cipher
  268. builtin type io2.Tls.ClientConfig
  269. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  270. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  271. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  272. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  273. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  274. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  275. io2.Tls.encodeCert : SignedCert -> Bytes
  276. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  277. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  278. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  279. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  280. builtin type io2.Tls.PrivateKey
  281. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  282. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  283. builtin type io2.Tls.ServerConfig
  284. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  285. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  286. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  287. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  288. builtin type io2.Tls.SignedCert
  289. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  290. builtin type io2.Tls.Version
  291. unique type io2.TlsFailure
  292. builtin type io2.TVar
  293. io2.TVar.new : a ->{STM} TVar a
  294. io2.TVar.newIO : a ->{IO} TVar a
  295. io2.TVar.read : TVar a ->{STM} a
  296. io2.TVar.readIO : TVar a ->{IO} a
  297. io2.TVar.swap : TVar a -> a ->{STM} a
  298. io2.TVar.write : TVar a -> a ->{STM} ()
  299. io2.validateSandboxed : [Term] -> a -> Boolean
  300. unique type IsPropagated
  301. IsPropagated.IsPropagated : IsPropagated
  302. unique type IsTest
  303. IsTest.IsTest : IsTest
  304. unique type Link
  305. builtin type Link.Term
  306. Link.Term : Term -> Link
  307. Link.Term.toText : Term -> Text
  308. builtin type Link.Type
  309. Link.Type : Type -> Link
  310. builtin type List
  311. List.++ : [a] -> [a] -> [a]
  312. List.+: : a -> [a] -> [a]
  313. List.:+ : [a] -> a -> [a]
  314. List.at : Nat -> [a] -> Optional a
  315. List.cons : a -> [a] -> [a]
  316. List.drop : Nat -> [a] -> [a]
  317. List.empty : [a]
  318. List.size : [a] -> Nat
  319. List.snoc : [a] -> a -> [a]
  320. List.take : Nat -> [a] -> [a]
  321. metadata.isPropagated : IsPropagated
  322. metadata.isTest : IsTest
  323. builtin type MutableArray
  324. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  325. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  326. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  327. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  328. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  329. builtin type MutableByteArray
  330. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  331. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  332. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  333. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  334. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  335. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  336. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  337. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  338. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  339. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  340. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  341. builtin type Nat
  342. Nat.* : Nat -> Nat -> Nat
  343. Nat.+ : Nat -> Nat -> Nat
  344. Nat./ : Nat -> Nat -> Nat
  345. Nat.and : Nat -> Nat -> Nat
  346. Nat.complement : Nat -> Nat
  347. Nat.drop : Nat -> Nat -> Nat
  348. Nat.eq : Nat -> Nat -> Boolean
  349. Nat.fromText : Text -> Optional Nat
  350. Nat.gt : Nat -> Nat -> Boolean
  351. Nat.gteq : Nat -> Nat -> Boolean
  352. Nat.increment : Nat -> Nat
  353. Nat.isEven : Nat -> Boolean
  354. Nat.isOdd : Nat -> Boolean
  355. Nat.leadingZeros : Nat -> Nat
  356. Nat.lt : Nat -> Nat -> Boolean
  357. Nat.lteq : Nat -> Nat -> Boolean
  358. Nat.mod : Nat -> Nat -> Nat
  359. Nat.or : Nat -> Nat -> Nat
  360. Nat.popCount : Nat -> Nat
  361. Nat.pow : Nat -> Nat -> Nat
  362. Nat.shiftLeft : Nat -> Nat -> Nat
  363. Nat.shiftRight : Nat -> Nat -> Nat
  364. Nat.sub : Nat -> Nat -> Int
  365. Nat.toFloat : Nat -> Float
  366. Nat.toInt : Nat -> Int
  367. Nat.toText : Nat -> Text
  368. Nat.trailingZeros : Nat -> Nat
  369. Nat.xor : Nat -> Nat -> Nat
  370. structural type Optional a
  371. Optional.None : Optional a
  372. Optional.Some : a -> Optional a
  373. builtin type Ref
  374. Ref.read : Ref g a ->{g} a
  375. Ref.write : Ref g a -> a ->{g} ()
  376. builtin type Request
  377. builtin type Scope
  378. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  379. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  380. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  381. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  382. Scope.ref : a ->{Scope s} Ref {Scope s} a
  383. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  384. structural type SeqView a b
  385. SeqView.VElem : a -> b -> SeqView a b
  386. SeqView.VEmpty : SeqView a b
  387. Socket.toText : Socket -> Text
  388. unique type Test.Result
  389. Test.Result.Fail : Text -> Result
  390. Test.Result.Ok : Text -> Result
  391. builtin type Text
  392. Text.!= : Text -> Text -> Boolean
  393. Text.++ : Text -> Text -> Text
  394. Text.drop : Nat -> Text -> Text
  395. Text.empty : Text
  396. Text.eq : Text -> Text -> Boolean
  397. Text.fromCharList : [Char] -> Text
  398. Text.fromUtf8.impl : Bytes -> Either Failure Text
  399. Text.gt : Text -> Text -> Boolean
  400. Text.gteq : Text -> Text -> Boolean
  401. Text.lt : Text -> Text -> Boolean
  402. Text.lteq : Text -> Text -> Boolean
  403. Text.repeat : Nat -> Text -> Text
  404. Text.size : Text -> Nat
  405. Text.take : Nat -> Text -> Text
  406. Text.toCharList : Text -> [Char]
  407. Text.toUtf8 : Text -> Bytes
  408. Text.uncons : Text -> Optional (Char, Text)
  409. Text.unsnoc : Text -> Optional (Text, Char)
  410. ThreadId.toText : ThreadId -> Text
  411. todo : a -> b
  412. structural type Tuple a b
  413. Tuple.Cons : a -> b -> Tuple a b
  414. structural type Unit
  415. Unit.Unit : ()
  416. Universal.< : a -> a -> Boolean
  417. Universal.<= : a -> a -> Boolean
  418. Universal.== : a -> a -> Boolean
  419. Universal.> : a -> a -> Boolean
  420. Universal.>= : a -> a -> Boolean
  421. Universal.compare : a -> a -> Int
  422. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  423. builtin type Value
  424. Value.dependencies : Value -> [Term]
  425. Value.deserialize : Bytes -> Either Text Value
  426. Value.load : Value ->{IO} Either [Term] a
  427. Value.serialize : Value -> Bytes
  428. Value.value : a -> Value
  

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
