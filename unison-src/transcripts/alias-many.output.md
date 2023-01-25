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
  67.  Debug.toText : a -> Optional (Either Text Text)
  68.  Debug.trace : Text -> a -> ()
  69.  Debug.watch : Text -> a -> a
  70.  unique type Doc
  71.  Doc.Blob : Text -> Doc
  72.  Doc.Evaluate : Term -> Doc
  73.  Doc.Join : [Doc] -> Doc
  74.  Doc.Link : Link -> Doc
  75.  Doc.Signature : Term -> Doc
  76.  Doc.Source : Link -> Doc
  77.  structural type Either a b
  78.  Either.Left : a -> Either a b
  79.  Either.Right : b -> Either a b
  80.  structural ability Exception
  81.  Exception.raise : Failure ->{Exception} x
  82.  builtin type Float
  83.  Float.* : Float -> Float -> Float
  84.  Float.+ : Float -> Float -> Float
  85.  Float.- : Float -> Float -> Float
  86.  Float./ : Float -> Float -> Float
  87.  Float.abs : Float -> Float
  88.  Float.acos : Float -> Float
  89.  Float.acosh : Float -> Float
  90.  Float.asin : Float -> Float
  91.  Float.asinh : Float -> Float
  92.  Float.atan : Float -> Float
  93.  Float.atan2 : Float -> Float -> Float
  94.  Float.atanh : Float -> Float
  95.  Float.ceiling : Float -> Int
  96.  Float.cos : Float -> Float
  97.  Float.cosh : Float -> Float
  98.  Float.eq : Float -> Float -> Boolean
  99.  Float.exp : Float -> Float
  100. Float.floor : Float -> Int
  101. Float.fromRepresentation : Nat -> Float
  102. Float.fromText : Text -> Optional Float
  103. Float.gt : Float -> Float -> Boolean
  104. Float.gteq : Float -> Float -> Boolean
  105. Float.log : Float -> Float
  106. Float.logBase : Float -> Float -> Float
  107. Float.lt : Float -> Float -> Boolean
  108. Float.lteq : Float -> Float -> Boolean
  109. Float.max : Float -> Float -> Float
  110. Float.min : Float -> Float -> Float
  111. Float.pow : Float -> Float -> Float
  112. Float.round : Float -> Int
  113. Float.sin : Float -> Float
  114. Float.sinh : Float -> Float
  115. Float.sqrt : Float -> Float
  116. Float.tan : Float -> Float
  117. Float.tanh : Float -> Float
  118. Float.toRepresentation : Float -> Nat
  119. Float.toText : Float -> Text
  120. Float.truncate : Float -> Int
  121. Handle.toText : Handle -> Text
  122. builtin type ImmutableArray
  123. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  124. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  125. ImmutableArray.size : ImmutableArray a -> Nat
  126. builtin type ImmutableByteArray
  127. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  128. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  129. ImmutableByteArray.read24be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  130. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  131. ImmutableByteArray.read40be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  132. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  133. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  134. ImmutableByteArray.size : ImmutableByteArray -> Nat
  135. builtin type Int
  136. Int.* : Int -> Int -> Int
  137. Int.+ : Int -> Int -> Int
  138. Int.- : Int -> Int -> Int
  139. Int./ : Int -> Int -> Int
  140. Int.and : Int -> Int -> Int
  141. Int.complement : Int -> Int
  142. Int.eq : Int -> Int -> Boolean
  143. Int.fromRepresentation : Nat -> Int
  144. Int.fromText : Text -> Optional Int
  145. Int.gt : Int -> Int -> Boolean
  146. Int.gteq : Int -> Int -> Boolean
  147. Int.increment : Int -> Int
  148. Int.isEven : Int -> Boolean
  149. Int.isOdd : Int -> Boolean
  150. Int.leadingZeros : Int -> Nat
  151. Int.lt : Int -> Int -> Boolean
  152. Int.lteq : Int -> Int -> Boolean
  153. Int.mod : Int -> Int -> Int
  154. Int.negate : Int -> Int
  155. Int.or : Int -> Int -> Int
  156. Int.popCount : Int -> Nat
  157. Int.pow : Int -> Nat -> Int
  158. Int.shiftLeft : Int -> Nat -> Int
  159. Int.shiftRight : Int -> Nat -> Int
  160. Int.signum : Int -> Int
  161. Int.toFloat : Int -> Float
  162. Int.toRepresentation : Int -> Nat
  163. Int.toText : Int -> Text
  164. Int.trailingZeros : Int -> Nat
  165. Int.truncate0 : Int -> Nat
  166. Int.xor : Int -> Int -> Int
  167. unique type io2.ArithmeticFailure
  168. unique type io2.ArrayFailure
  169. unique type io2.BufferMode
  170. io2.BufferMode.BlockBuffering : BufferMode
  171. io2.BufferMode.LineBuffering : BufferMode
  172. io2.BufferMode.NoBuffering : BufferMode
  173. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  174. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  175. io2.Clock.internals.nsec : TimeSpec -> Nat
  176. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  177. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  178. io2.Clock.internals.sec : TimeSpec -> Int
  179. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  180. builtin type io2.Clock.internals.TimeSpec
  181. unique type io2.Failure
  182. io2.Failure.Failure : Type -> Text -> Any -> Failure
  183. unique type io2.FileMode
  184. io2.FileMode.Append : FileMode
  185. io2.FileMode.Read : FileMode
  186. io2.FileMode.ReadWrite : FileMode
  187. io2.FileMode.Write : FileMode
  188. builtin type io2.Handle
  189. builtin type io2.IO
  190. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  191. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  192. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  193. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  194. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  195. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  196. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  197. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  198. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  199. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  200. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  201. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  202. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  203. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  204. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  205. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  206. io2.IO.getChar.impl : Handle ->{IO} Either Failure Char
  207. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  208. io2.IO.getEcho.impl : Handle
                             ->{IO} Either Failure Boolean
  209. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  210. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  211. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  212. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  213. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  214. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  215. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  216. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  217. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  218. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  219. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  220. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  221. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  222. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  223. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  224. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  225. io2.IO.ref : a ->{IO} Ref {IO} a
  226. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  227. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  228. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  229. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  230. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  231. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  232. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  233. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  234. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  235. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  236. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  237. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  238. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  239. io2.IO.stdHandle : StdHandle -> Handle
  240. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  241. io2.IO.systemTimeMicroseconds : '{IO} Int
  242. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  243. unique type io2.IOError
  244. io2.IOError.AlreadyExists : IOError
  245. io2.IOError.EOF : IOError
  246. io2.IOError.IllegalOperation : IOError
  247. io2.IOError.NoSuchThing : IOError
  248. io2.IOError.PermissionDenied : IOError
  249. io2.IOError.ResourceBusy : IOError
  250. io2.IOError.ResourceExhausted : IOError
  251. io2.IOError.UserError : IOError
  252. unique type io2.IOFailure
  253. unique type io2.MiscFailure
  254. builtin type io2.MVar
  255. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  256. io2.MVar.new : a ->{IO} MVar a
  257. io2.MVar.newEmpty : '{IO} MVar a
  258. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  259. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  260. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  261. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  262. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  263. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  264. io2.MVar.tryTake : MVar a ->{IO} Optional a
  265. builtin type io2.Promise
  266. io2.Promise.new : '{IO} Promise a
  267. io2.Promise.read : Promise a ->{IO} a
  268. io2.Promise.tryRead : Promise a ->{IO} Optional a
  269. io2.Promise.write : Promise a -> a ->{IO} Boolean
  270. io2.Ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
  271. io2.Ref.readForCas : Ref {IO} a ->{IO} Ticket a
  272. builtin type io2.Ref.Ticket
  273. io2.Ref.Ticket.read : Ticket a -> a
  274. unique type io2.RuntimeFailure
  275. unique type io2.SeekMode
  276. io2.SeekMode.AbsoluteSeek : SeekMode
  277. io2.SeekMode.RelativeSeek : SeekMode
  278. io2.SeekMode.SeekFromEnd : SeekMode
  279. builtin type io2.Socket
  280. unique type io2.StdHandle
  281. io2.StdHandle.StdErr : StdHandle
  282. io2.StdHandle.StdIn : StdHandle
  283. io2.StdHandle.StdOut : StdHandle
  284. builtin type io2.STM
  285. io2.STM.atomically : '{STM} a ->{IO} a
  286. io2.STM.retry : '{STM} a
  287. unique type io2.STMFailure
  288. builtin type io2.ThreadId
  289. builtin type io2.Tls
  290. builtin type io2.Tls.Cipher
  291. builtin type io2.Tls.ClientConfig
  292. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  293. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  294. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  295. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  296. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  297. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  298. io2.Tls.encodeCert : SignedCert -> Bytes
  299. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  300. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  301. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  302. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  303. builtin type io2.Tls.PrivateKey
  304. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  305. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  306. builtin type io2.Tls.ServerConfig
  307. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  308. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  309. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  310. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  311. builtin type io2.Tls.SignedCert
  312. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  313. builtin type io2.Tls.Version
  314. unique type io2.TlsFailure
  315. builtin type io2.TVar
  316. io2.TVar.new : a ->{STM} TVar a
  317. io2.TVar.newIO : a ->{IO} TVar a
  318. io2.TVar.read : TVar a ->{STM} a
  319. io2.TVar.readIO : TVar a ->{IO} a
  320. io2.TVar.swap : TVar a -> a ->{STM} a
  321. io2.TVar.write : TVar a -> a ->{STM} ()
  322. io2.validateSandboxed : [Term] -> a -> Boolean
  323. unique type IsPropagated
  324. IsPropagated.IsPropagated : IsPropagated
  325. unique type IsTest
  326. IsTest.IsTest : IsTest
  327. unique type Link
  328. builtin type Link.Term
  329. Link.Term : Term -> Link
  330. Link.Term.toText : Term -> Text
  331. builtin type Link.Type
  332. Link.Type : Type -> Link
  333. builtin type List
  334. List.++ : [a] -> [a] -> [a]
  335. List.+: : a -> [a] -> [a]
  336. List.:+ : [a] -> a -> [a]
  337. List.at : Nat -> [a] -> Optional a
  338. List.cons : a -> [a] -> [a]
  339. List.drop : Nat -> [a] -> [a]
  340. List.empty : [a]
  341. List.size : [a] -> Nat
  342. List.snoc : [a] -> a -> [a]
  343. List.take : Nat -> [a] -> [a]
  344. metadata.isPropagated : IsPropagated
  345. metadata.isTest : IsTest
  346. builtin type MutableArray
  347. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  348. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  349. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  350. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  351. MutableArray.size : MutableArray g a -> Nat
  352. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  353. builtin type MutableByteArray
  354. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  355. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  356. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  357. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  358. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  359. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  360. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  361. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  362. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  363. MutableByteArray.size : MutableByteArray g -> Nat
  364. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  365. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  366. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  367. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  368. builtin type Nat
  369. Nat.* : Nat -> Nat -> Nat
  370. Nat.+ : Nat -> Nat -> Nat
  371. Nat./ : Nat -> Nat -> Nat
  372. Nat.and : Nat -> Nat -> Nat
  373. Nat.complement : Nat -> Nat
  374. Nat.drop : Nat -> Nat -> Nat
  375. Nat.eq : Nat -> Nat -> Boolean
  376. Nat.fromText : Text -> Optional Nat
  377. Nat.gt : Nat -> Nat -> Boolean
  378. Nat.gteq : Nat -> Nat -> Boolean
  379. Nat.increment : Nat -> Nat
  380. Nat.isEven : Nat -> Boolean
  381. Nat.isOdd : Nat -> Boolean
  382. Nat.leadingZeros : Nat -> Nat
  383. Nat.lt : Nat -> Nat -> Boolean
  384. Nat.lteq : Nat -> Nat -> Boolean
  385. Nat.mod : Nat -> Nat -> Nat
  386. Nat.or : Nat -> Nat -> Nat
  387. Nat.popCount : Nat -> Nat
  388. Nat.pow : Nat -> Nat -> Nat
  389. Nat.shiftLeft : Nat -> Nat -> Nat
  390. Nat.shiftRight : Nat -> Nat -> Nat
  391. Nat.sub : Nat -> Nat -> Int
  392. Nat.toFloat : Nat -> Float
  393. Nat.toInt : Nat -> Int
  394. Nat.toText : Nat -> Text
  395. Nat.trailingZeros : Nat -> Nat
  396. Nat.xor : Nat -> Nat -> Nat
  397. structural type Optional a
  398. Optional.None : Optional a
  399. Optional.Some : a -> Optional a
  400. builtin type Pattern
  401. Pattern.capture : Pattern a -> Pattern a
  402. Pattern.isMatch : Pattern a -> a -> Boolean
  403. Pattern.join : [Pattern a] -> Pattern a
  404. Pattern.many : Pattern a -> Pattern a
  405. Pattern.or : Pattern a -> Pattern a -> Pattern a
  406. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  407. Pattern.run : Pattern a -> a -> Optional ([a], a)
  408. builtin type Ref
  409. Ref.read : Ref g a ->{g} a
  410. Ref.write : Ref g a -> a ->{g} ()
  411. builtin type Request
  412. builtin type Scope
  413. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  414. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  415. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  416. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  417. Scope.ref : a ->{Scope s} Ref {Scope s} a
  418. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  419. structural type SeqView a b
  420. SeqView.VElem : a -> b -> SeqView a b
  421. SeqView.VEmpty : SeqView a b
  422. Socket.toText : Socket -> Text
  423. unique type Test.Result
  424. Test.Result.Fail : Text -> Result
  425. Test.Result.Ok : Text -> Result
  426. builtin type Text
  427. Text.!= : Text -> Text -> Boolean
  428. Text.++ : Text -> Text -> Text
  429. Text.drop : Nat -> Text -> Text
  430. Text.empty : Text
  431. Text.eq : Text -> Text -> Boolean
  432. Text.fromCharList : [Char] -> Text
  433. Text.fromUtf8.impl : Bytes -> Either Failure Text
  434. Text.gt : Text -> Text -> Boolean
  435. Text.gteq : Text -> Text -> Boolean
  436. Text.lt : Text -> Text -> Boolean
  437. Text.lteq : Text -> Text -> Boolean
  438. Text.patterns.anyChar : Pattern Text
  439. Text.patterns.charIn : [Char] -> Pattern Text
  440. Text.patterns.charRange : Char -> Char -> Pattern Text
  441. Text.patterns.digit : Pattern Text
  442. Text.patterns.eof : Pattern Text
  443. Text.patterns.letter : Pattern Text
  444. Text.patterns.literal : Text -> Pattern Text
  445. Text.patterns.notCharIn : [Char] -> Pattern Text
  446. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  447. Text.patterns.punctuation : Pattern Text
  448. Text.patterns.space : Pattern Text
  449. Text.repeat : Nat -> Text -> Text
  450. Text.reverse : Text -> Text
  451. Text.size : Text -> Nat
  452. Text.take : Nat -> Text -> Text
  453. Text.toCharList : Text -> [Char]
  454. Text.toLowercase : Text -> Text
  455. Text.toUppercase : Text -> Text
  456. Text.toUtf8 : Text -> Bytes
  457. Text.uncons : Text -> Optional (Char, Text)
  458. Text.unsnoc : Text -> Optional (Text, Char)
  459. ThreadId.toText : ThreadId -> Text
  460. todo : a -> b
  461. structural type Tuple a b
  462. Tuple.Cons : a -> b -> Tuple a b
  463. structural type Unit
  464. Unit.Unit : ()
  465. Universal.< : a -> a -> Boolean
  466. Universal.<= : a -> a -> Boolean
  467. Universal.== : a -> a -> Boolean
  468. Universal.> : a -> a -> Boolean
  469. Universal.>= : a -> a -> Boolean
  470. Universal.compare : a -> a -> Int
  471. Universal.murmurHash : a -> Nat
  472. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  473. builtin type Value
  474. Value.dependencies : Value -> [Term]
  475. Value.deserialize : Bytes -> Either Text Value
  476. Value.load : Value ->{IO} Either [Term] a
  477. Value.serialize : Value -> Bytes
  478. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Float.atanh              : Float -> Float
    2.  Float.ceiling            : Float -> Int
    3.  Float.cos                : Float -> Float
    4.  Float.cosh               : Float -> Float
    5.  Float.eq                 : Float -> Float -> Boolean
    6.  Float.exp                : Float -> Float
    7.  Float.floor              : Float -> Int
    8.  Float.fromRepresentation : Nat -> Float
    9.  Float.fromText           : Text -> Optional Float
    10. Float.gt                 : Float -> Float -> Boolean
    11. Float.gteq               : Float -> Float -> Boolean
  
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

  1.  Float.atanh : Float -> Float
  2.  Float.ceiling : Float -> Int
  3.  Float.cos : Float -> Float
  4.  Float.cosh : Float -> Float
  5.  Float.eq : Float -> Float -> Boolean
  6.  Float.exp : Float -> Float
  7.  Float.floor : Float -> Int
  8.  Float.fromRepresentation : Nat -> Float
  9.  Float.fromText : Text -> Optional Float
  10. Float.gt : Float -> Float -> Boolean
  11. Float.gteq : Float -> Float -> Boolean
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
