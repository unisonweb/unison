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
  223. io2.IO.process.call : Text -> [Text] ->{IO} Nat
  224. io2.IO.process.exitCode : ProcessHandle
                                 ->{IO} Optional Nat
  225. io2.IO.process.kill : ProcessHandle ->{IO} ()
  226. io2.IO.process.start : Text
                              -> [Text]
                              ->{IO} ( Handle,
                                Handle,
                                Handle,
                                ProcessHandle)
  227. io2.IO.process.wait : ProcessHandle ->{IO} Nat
  228. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  229. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  230. io2.IO.ref : a ->{IO} Ref {IO} a
  231. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  232. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  233. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  234. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  235. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  236. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  237. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  238. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  239. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  240. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  241. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  242. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  243. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  244. io2.IO.stdHandle : StdHandle -> Handle
  245. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  246. io2.IO.systemTimeMicroseconds : '{IO} Int
  247. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  248. unique type io2.IOError
  249. io2.IOError.AlreadyExists : IOError
  250. io2.IOError.EOF : IOError
  251. io2.IOError.IllegalOperation : IOError
  252. io2.IOError.NoSuchThing : IOError
  253. io2.IOError.PermissionDenied : IOError
  254. io2.IOError.ResourceBusy : IOError
  255. io2.IOError.ResourceExhausted : IOError
  256. io2.IOError.UserError : IOError
  257. unique type io2.IOFailure
  258. unique type io2.MiscFailure
  259. builtin type io2.MVar
  260. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  261. io2.MVar.new : a ->{IO} MVar a
  262. io2.MVar.newEmpty : '{IO} MVar a
  263. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  264. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  265. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  266. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  267. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  268. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  269. io2.MVar.tryTake : MVar a ->{IO} Optional a
  270. builtin type io2.ProcessHandle
  271. builtin type io2.Promise
  272. io2.Promise.new : '{IO} Promise a
  273. io2.Promise.read : Promise a ->{IO} a
  274. io2.Promise.tryRead : Promise a ->{IO} Optional a
  275. io2.Promise.write : Promise a -> a ->{IO} Boolean
  276. io2.Ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
  277. io2.Ref.readForCas : Ref {IO} a ->{IO} Ticket a
  278. builtin type io2.Ref.Ticket
  279. io2.Ref.Ticket.read : Ticket a -> a
  280. unique type io2.RuntimeFailure
  281. unique type io2.SeekMode
  282. io2.SeekMode.AbsoluteSeek : SeekMode
  283. io2.SeekMode.RelativeSeek : SeekMode
  284. io2.SeekMode.SeekFromEnd : SeekMode
  285. builtin type io2.Socket
  286. unique type io2.StdHandle
  287. io2.StdHandle.StdErr : StdHandle
  288. io2.StdHandle.StdIn : StdHandle
  289. io2.StdHandle.StdOut : StdHandle
  290. builtin type io2.STM
  291. io2.STM.atomically : '{STM} a ->{IO} a
  292. io2.STM.retry : '{STM} a
  293. unique type io2.STMFailure
  294. builtin type io2.ThreadId
  295. builtin type io2.Tls
  296. builtin type io2.Tls.Cipher
  297. builtin type io2.Tls.ClientConfig
  298. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  299. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  300. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  301. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  302. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  303. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  304. io2.Tls.encodeCert : SignedCert -> Bytes
  305. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  306. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  307. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  308. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  309. builtin type io2.Tls.PrivateKey
  310. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  311. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  312. builtin type io2.Tls.ServerConfig
  313. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  314. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  315. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  316. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  317. builtin type io2.Tls.SignedCert
  318. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  319. builtin type io2.Tls.Version
  320. unique type io2.TlsFailure
  321. builtin type io2.TVar
  322. io2.TVar.new : a ->{STM} TVar a
  323. io2.TVar.newIO : a ->{IO} TVar a
  324. io2.TVar.read : TVar a ->{STM} a
  325. io2.TVar.readIO : TVar a ->{IO} a
  326. io2.TVar.swap : TVar a -> a ->{STM} a
  327. io2.TVar.write : TVar a -> a ->{STM} ()
  328. io2.validateSandboxed : [Term] -> a -> Boolean
  329. unique type IsPropagated
  330. IsPropagated.IsPropagated : IsPropagated
  331. unique type IsTest
  332. IsTest.IsTest : IsTest
  333. unique type Link
  334. builtin type Link.Term
  335. Link.Term : Term -> Link
  336. Link.Term.toText : Term -> Text
  337. builtin type Link.Type
  338. Link.Type : Type -> Link
  339. builtin type List
  340. List.++ : [a] -> [a] -> [a]
  341. List.+: : a -> [a] -> [a]
  342. List.:+ : [a] -> a -> [a]
  343. List.at : Nat -> [a] -> Optional a
  344. List.cons : a -> [a] -> [a]
  345. List.drop : Nat -> [a] -> [a]
  346. List.empty : [a]
  347. List.size : [a] -> Nat
  348. List.snoc : [a] -> a -> [a]
  349. List.take : Nat -> [a] -> [a]
  350. metadata.isPropagated : IsPropagated
  351. metadata.isTest : IsTest
  352. builtin type MutableArray
  353. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  354. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  355. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  356. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  357. MutableArray.size : MutableArray g a -> Nat
  358. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  359. builtin type MutableByteArray
  360. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  361. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  362. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  363. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  364. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  365. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  366. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  367. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  368. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  369. MutableByteArray.size : MutableByteArray g -> Nat
  370. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  371. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  372. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  373. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  374. builtin type Nat
  375. Nat.* : Nat -> Nat -> Nat
  376. Nat.+ : Nat -> Nat -> Nat
  377. Nat./ : Nat -> Nat -> Nat
  378. Nat.and : Nat -> Nat -> Nat
  379. Nat.complement : Nat -> Nat
  380. Nat.drop : Nat -> Nat -> Nat
  381. Nat.eq : Nat -> Nat -> Boolean
  382. Nat.fromText : Text -> Optional Nat
  383. Nat.gt : Nat -> Nat -> Boolean
  384. Nat.gteq : Nat -> Nat -> Boolean
  385. Nat.increment : Nat -> Nat
  386. Nat.isEven : Nat -> Boolean
  387. Nat.isOdd : Nat -> Boolean
  388. Nat.leadingZeros : Nat -> Nat
  389. Nat.lt : Nat -> Nat -> Boolean
  390. Nat.lteq : Nat -> Nat -> Boolean
  391. Nat.mod : Nat -> Nat -> Nat
  392. Nat.or : Nat -> Nat -> Nat
  393. Nat.popCount : Nat -> Nat
  394. Nat.pow : Nat -> Nat -> Nat
  395. Nat.shiftLeft : Nat -> Nat -> Nat
  396. Nat.shiftRight : Nat -> Nat -> Nat
  397. Nat.sub : Nat -> Nat -> Int
  398. Nat.toFloat : Nat -> Float
  399. Nat.toInt : Nat -> Int
  400. Nat.toText : Nat -> Text
  401. Nat.trailingZeros : Nat -> Nat
  402. Nat.xor : Nat -> Nat -> Nat
  403. structural type Optional a
  404. Optional.None : Optional a
  405. Optional.Some : a -> Optional a
  406. builtin type Pattern
  407. Pattern.capture : Pattern a -> Pattern a
  408. Pattern.isMatch : Pattern a -> a -> Boolean
  409. Pattern.join : [Pattern a] -> Pattern a
  410. Pattern.many : Pattern a -> Pattern a
  411. Pattern.or : Pattern a -> Pattern a -> Pattern a
  412. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  413. Pattern.run : Pattern a -> a -> Optional ([a], a)
  414. builtin type Ref
  415. Ref.read : Ref g a ->{g} a
  416. Ref.write : Ref g a -> a ->{g} ()
  417. builtin type Request
  418. builtin type Scope
  419. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  420. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  421. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  422. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  423. Scope.ref : a ->{Scope s} Ref {Scope s} a
  424. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  425. structural type SeqView a b
  426. SeqView.VElem : a -> b -> SeqView a b
  427. SeqView.VEmpty : SeqView a b
  428. Socket.toText : Socket -> Text
  429. unique type Test.Result
  430. Test.Result.Fail : Text -> Result
  431. Test.Result.Ok : Text -> Result
  432. builtin type Text
  433. Text.!= : Text -> Text -> Boolean
  434. Text.++ : Text -> Text -> Text
  435. Text.drop : Nat -> Text -> Text
  436. Text.empty : Text
  437. Text.eq : Text -> Text -> Boolean
  438. Text.fromCharList : [Char] -> Text
  439. Text.fromUtf8.impl : Bytes -> Either Failure Text
  440. Text.gt : Text -> Text -> Boolean
  441. Text.gteq : Text -> Text -> Boolean
  442. Text.lt : Text -> Text -> Boolean
  443. Text.lteq : Text -> Text -> Boolean
  444. Text.patterns.anyChar : Pattern Text
  445. Text.patterns.charIn : [Char] -> Pattern Text
  446. Text.patterns.charRange : Char -> Char -> Pattern Text
  447. Text.patterns.digit : Pattern Text
  448. Text.patterns.eof : Pattern Text
  449. Text.patterns.letter : Pattern Text
  450. Text.patterns.literal : Text -> Pattern Text
  451. Text.patterns.notCharIn : [Char] -> Pattern Text
  452. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  453. Text.patterns.punctuation : Pattern Text
  454. Text.patterns.space : Pattern Text
  455. Text.repeat : Nat -> Text -> Text
  456. Text.reverse : Text -> Text
  457. Text.size : Text -> Nat
  458. Text.take : Nat -> Text -> Text
  459. Text.toCharList : Text -> [Char]
  460. Text.toLowercase : Text -> Text
  461. Text.toUppercase : Text -> Text
  462. Text.toUtf8 : Text -> Bytes
  463. Text.uncons : Text -> Optional (Char, Text)
  464. Text.unsnoc : Text -> Optional (Text, Char)
  465. ThreadId.toText : ThreadId -> Text
  466. todo : a -> b
  467. structural type Tuple a b
  468. Tuple.Cons : a -> b -> Tuple a b
  469. structural type Unit
  470. Unit.Unit : ()
  471. Universal.< : a -> a -> Boolean
  472. Universal.<= : a -> a -> Boolean
  473. Universal.== : a -> a -> Boolean
  474. Universal.> : a -> a -> Boolean
  475. Universal.>= : a -> a -> Boolean
  476. Universal.compare : a -> a -> Int
  477. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  478. builtin type Value
  479. Value.dependencies : Value -> [Term]
  480. Value.deserialize : Bytes -> Either Text Value
  481. Value.load : Value ->{IO} Either [Term] a
  482. Value.serialize : Value -> Bytes
  483. Value.value : a -> Value
  

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
