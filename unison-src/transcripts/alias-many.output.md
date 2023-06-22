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
  32.  Bytes.indexOf : Bytes -> Bytes -> Optional Nat
  33.  Bytes.size : Bytes -> Nat
  34.  Bytes.take : Nat -> Bytes -> Bytes
  35.  Bytes.toBase16 : Bytes -> Bytes
  36.  Bytes.toBase32 : Bytes -> Bytes
  37.  Bytes.toBase64 : Bytes -> Bytes
  38.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  39.  Bytes.toList : Bytes -> [Nat]
  40.  Bytes.zlib.compress : Bytes -> Bytes
  41.  Bytes.zlib.decompress : Bytes -> Either Text Bytes
  42.  builtin type Char
  43.  builtin type Char.Class
  44.  Char.Class.alphanumeric : Class
  45.  Char.Class.and : Class -> Class -> Class
  46.  Char.Class.any : Class
  47.  Char.Class.anyOf : [Char] -> Class
  48.  Char.Class.control : Class
  49.  Char.Class.is : Class -> Char -> Boolean
  50.  Char.Class.letter : Class
  51.  Char.Class.lower : Class
  52.  Char.Class.mark : Class
  53.  Char.Class.not : Class -> Class
  54.  Char.Class.number : Class
  55.  Char.Class.or : Class -> Class -> Class
  56.  Char.Class.printable : Class
  57.  Char.Class.punctuation : Class
  58.  Char.Class.range : Char -> Char -> Class
  59.  Char.Class.separator : Class
  60.  Char.Class.symbol : Class
  61.  Char.Class.upper : Class
  62.  Char.Class.whitespace : Class
  63.  Char.fromNat : Nat -> Char
  64.  Char.toNat : Char -> Nat
  65.  Char.toText : Char -> Text
  66.  builtin type Code
  67.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  68.  Code.dependencies : Code -> [Term]
  69.  Code.deserialize : Bytes -> Either Text Code
  70.  Code.display : Text -> Code -> Text
  71.  Code.isMissing : Term ->{IO} Boolean
  72.  Code.lookup : Term ->{IO} Optional Code
  73.  Code.serialize : Code -> Bytes
  74.  Code.validate : [(Term, Code)] ->{IO} Optional Failure
  75.  crypto.hash : HashAlgorithm -> a -> Bytes
  76.  builtin type crypto.HashAlgorithm
  77.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  78.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  79.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  80.  crypto.HashAlgorithm.Md5 : HashAlgorithm
  81.  crypto.HashAlgorithm.Sha1 : HashAlgorithm
  82.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  83.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  84.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  85.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  86.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  87.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  88.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  89.  Debug.toText : a -> Optional (Either Text Text)
  90.  Debug.trace : Text -> a -> ()
  91.  Debug.watch : Text -> a -> a
  92.  unique type Doc
  93.  Doc.Blob : Text -> Doc
  94.  Doc.Evaluate : Term -> Doc
  95.  Doc.Join : [Doc] -> Doc
  96.  Doc.Link : Link -> Doc
  97.  Doc.Signature : Term -> Doc
  98.  Doc.Source : Link -> Doc
  99.  structural type Either a b
  100. Either.Left : a -> Either a b
  101. Either.Right : b -> Either a b
  102. structural ability Exception
  103. Exception.raise : Failure ->{Exception} x
  104. builtin type Float
  105. Float.* : Float -> Float -> Float
  106. Float.+ : Float -> Float -> Float
  107. Float.- : Float -> Float -> Float
  108. Float./ : Float -> Float -> Float
  109. Float.abs : Float -> Float
  110. Float.acos : Float -> Float
  111. Float.acosh : Float -> Float
  112. Float.asin : Float -> Float
  113. Float.asinh : Float -> Float
  114. Float.atan : Float -> Float
  115. Float.atan2 : Float -> Float -> Float
  116. Float.atanh : Float -> Float
  117. Float.ceiling : Float -> Int
  118. Float.cos : Float -> Float
  119. Float.cosh : Float -> Float
  120. Float.eq : Float -> Float -> Boolean
  121. Float.exp : Float -> Float
  122. Float.floor : Float -> Int
  123. Float.fromRepresentation : Nat -> Float
  124. Float.fromText : Text -> Optional Float
  125. Float.gt : Float -> Float -> Boolean
  126. Float.gteq : Float -> Float -> Boolean
  127. Float.log : Float -> Float
  128. Float.logBase : Float -> Float -> Float
  129. Float.lt : Float -> Float -> Boolean
  130. Float.lteq : Float -> Float -> Boolean
  131. Float.max : Float -> Float -> Float
  132. Float.min : Float -> Float -> Float
  133. Float.pow : Float -> Float -> Float
  134. Float.round : Float -> Int
  135. Float.sin : Float -> Float
  136. Float.sinh : Float -> Float
  137. Float.sqrt : Float -> Float
  138. Float.tan : Float -> Float
  139. Float.tanh : Float -> Float
  140. Float.toRepresentation : Float -> Nat
  141. Float.toText : Float -> Text
  142. Float.truncate : Float -> Int
  143. Handle.toText : Handle -> Text
  144. builtin type ImmutableArray
  145. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  146. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  147. ImmutableArray.size : ImmutableArray a -> Nat
  148. builtin type ImmutableByteArray
  149. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  150. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  151. ImmutableByteArray.read24be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  152. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  153. ImmutableByteArray.read40be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  154. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  155. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  156. ImmutableByteArray.size : ImmutableByteArray -> Nat
  157. builtin type Int
  158. Int.* : Int -> Int -> Int
  159. Int.+ : Int -> Int -> Int
  160. Int.- : Int -> Int -> Int
  161. Int./ : Int -> Int -> Int
  162. Int.and : Int -> Int -> Int
  163. Int.complement : Int -> Int
  164. Int.eq : Int -> Int -> Boolean
  165. Int.fromRepresentation : Nat -> Int
  166. Int.fromText : Text -> Optional Int
  167. Int.gt : Int -> Int -> Boolean
  168. Int.gteq : Int -> Int -> Boolean
  169. Int.increment : Int -> Int
  170. Int.isEven : Int -> Boolean
  171. Int.isOdd : Int -> Boolean
  172. Int.leadingZeros : Int -> Nat
  173. Int.lt : Int -> Int -> Boolean
  174. Int.lteq : Int -> Int -> Boolean
  175. Int.mod : Int -> Int -> Int
  176. Int.negate : Int -> Int
  177. Int.or : Int -> Int -> Int
  178. Int.popCount : Int -> Nat
  179. Int.pow : Int -> Nat -> Int
  180. Int.shiftLeft : Int -> Nat -> Int
  181. Int.shiftRight : Int -> Nat -> Int
  182. Int.signum : Int -> Int
  183. Int.toFloat : Int -> Float
  184. Int.toRepresentation : Int -> Nat
  185. Int.toText : Int -> Text
  186. Int.trailingZeros : Int -> Nat
  187. Int.truncate0 : Int -> Nat
  188. Int.xor : Int -> Int -> Int
  189. unique type io2.ArithmeticFailure
  190. unique type io2.ArrayFailure
  191. unique type io2.BufferMode
  192. io2.BufferMode.BlockBuffering : BufferMode
  193. io2.BufferMode.LineBuffering : BufferMode
  194. io2.BufferMode.NoBuffering : BufferMode
  195. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  196. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  197. io2.Clock.internals.nsec : TimeSpec -> Nat
  198. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  199. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  200. io2.Clock.internals.sec : TimeSpec -> Int
  201. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  202. builtin type io2.Clock.internals.TimeSpec
  203. unique type io2.Failure
  204. io2.Failure.Failure : Type -> Text -> Any -> Failure
  205. unique type io2.FileMode
  206. io2.FileMode.Append : FileMode
  207. io2.FileMode.Read : FileMode
  208. io2.FileMode.ReadWrite : FileMode
  209. io2.FileMode.Write : FileMode
  210. builtin type io2.Handle
  211. builtin type io2.IO
  212. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  213. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  214. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  215. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  216. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  217. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  218. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  219. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  220. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  221. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  222. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  223. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  224. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  225. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  226. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  227. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  228. io2.IO.getChar.impl : Handle ->{IO} Either Failure Char
  229. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  230. io2.IO.getEcho.impl : Handle
                             ->{IO} Either Failure Boolean
  231. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  232. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  233. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  234. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  235. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  236. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  237. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  238. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  239. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  240. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  241. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  242. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  243. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  244. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  245. io2.IO.process.call : Text -> [Text] ->{IO} Nat
  246. io2.IO.process.exitCode : ProcessHandle
                                 ->{IO} Optional Nat
  247. io2.IO.process.kill : ProcessHandle ->{IO} ()
  248. io2.IO.process.start : Text
                              -> [Text]
                              ->{IO} ( Handle,
                                Handle,
                                Handle,
                                ProcessHandle)
  249. io2.IO.process.wait : ProcessHandle ->{IO} Nat
  250. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  251. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  252. io2.IO.ref : a ->{IO} Ref {IO} a
  253. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  254. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  255. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  256. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  257. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  258. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  259. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  260. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  261. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  262. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  263. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  264. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  265. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  266. io2.IO.stdHandle : StdHandle -> Handle
  267. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  268. io2.IO.systemTimeMicroseconds : '{IO} Int
  269. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  270. unique type io2.IOError
  271. io2.IOError.AlreadyExists : IOError
  272. io2.IOError.EOF : IOError
  273. io2.IOError.IllegalOperation : IOError
  274. io2.IOError.NoSuchThing : IOError
  275. io2.IOError.PermissionDenied : IOError
  276. io2.IOError.ResourceBusy : IOError
  277. io2.IOError.ResourceExhausted : IOError
  278. io2.IOError.UserError : IOError
  279. unique type io2.IOFailure
  280. unique type io2.MiscFailure
  281. builtin type io2.MVar
  282. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  283. io2.MVar.new : a ->{IO} MVar a
  284. io2.MVar.newEmpty : '{IO} MVar a
  285. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  286. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  287. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  288. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  289. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  290. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  291. io2.MVar.tryTake : MVar a ->{IO} Optional a
  292. builtin type io2.ProcessHandle
  293. builtin type io2.Promise
  294. io2.Promise.new : '{IO} Promise a
  295. io2.Promise.read : Promise a ->{IO} a
  296. io2.Promise.tryRead : Promise a ->{IO} Optional a
  297. io2.Promise.write : Promise a -> a ->{IO} Boolean
  298. io2.Ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
  299. io2.Ref.readForCas : Ref {IO} a ->{IO} Ticket a
  300. builtin type io2.Ref.Ticket
  301. io2.Ref.Ticket.read : Ticket a -> a
  302. unique type io2.RuntimeFailure
  303. unique type io2.SeekMode
  304. io2.SeekMode.AbsoluteSeek : SeekMode
  305. io2.SeekMode.RelativeSeek : SeekMode
  306. io2.SeekMode.SeekFromEnd : SeekMode
  307. builtin type io2.Socket
  308. unique type io2.StdHandle
  309. io2.StdHandle.StdErr : StdHandle
  310. io2.StdHandle.StdIn : StdHandle
  311. io2.StdHandle.StdOut : StdHandle
  312. builtin type io2.STM
  313. io2.STM.atomically : '{STM} a ->{IO} a
  314. io2.STM.retry : '{STM} a
  315. unique type io2.STMFailure
  316. builtin type io2.ThreadId
  317. unique type io2.ThreadKilledFailure
  318. builtin type io2.Tls
  319. builtin type io2.Tls.Cipher
  320. builtin type io2.Tls.ClientConfig
  321. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  322. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  323. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  324. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  325. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  326. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  327. io2.Tls.encodeCert : SignedCert -> Bytes
  328. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  329. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  330. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  331. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  332. builtin type io2.Tls.PrivateKey
  333. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  334. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  335. builtin type io2.Tls.ServerConfig
  336. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  337. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  338. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  339. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  340. builtin type io2.Tls.SignedCert
  341. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  342. builtin type io2.Tls.Version
  343. unique type io2.TlsFailure
  344. builtin type io2.TVar
  345. io2.TVar.new : a ->{STM} TVar a
  346. io2.TVar.newIO : a ->{IO} TVar a
  347. io2.TVar.read : TVar a ->{STM} a
  348. io2.TVar.readIO : TVar a ->{IO} a
  349. io2.TVar.swap : TVar a -> a ->{STM} a
  350. io2.TVar.write : TVar a -> a ->{STM} ()
  351. io2.validateSandboxed : [Term] -> a -> Boolean
  352. unique type IsPropagated
  353. IsPropagated.IsPropagated : IsPropagated
  354. unique type IsTest
  355. IsTest.IsTest : IsTest
  356. unique type Link
  357. builtin type Link.Term
  358. Link.Term : Term -> Link
  359. Link.Term.toText : Term -> Text
  360. builtin type Link.Type
  361. Link.Type : Type -> Link
  362. builtin type List
  363. List.++ : [a] -> [a] -> [a]
  364. List.+: : a -> [a] -> [a]
  365. List.:+ : [a] -> a -> [a]
  366. List.at : Nat -> [a] -> Optional a
  367. List.cons : a -> [a] -> [a]
  368. List.drop : Nat -> [a] -> [a]
  369. List.empty : [a]
  370. List.size : [a] -> Nat
  371. List.snoc : [a] -> a -> [a]
  372. List.take : Nat -> [a] -> [a]
  373. metadata.isPropagated : IsPropagated
  374. metadata.isTest : IsTest
  375. builtin type MutableArray
  376. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  377. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  378. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  379. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  380. MutableArray.size : MutableArray g a -> Nat
  381. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  382. builtin type MutableByteArray
  383. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  384. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  385. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  386. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  387. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  388. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  389. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  390. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  391. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  392. MutableByteArray.size : MutableByteArray g -> Nat
  393. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  394. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  395. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  396. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  397. builtin type Nat
  398. Nat.* : Nat -> Nat -> Nat
  399. Nat.+ : Nat -> Nat -> Nat
  400. Nat./ : Nat -> Nat -> Nat
  401. Nat.and : Nat -> Nat -> Nat
  402. Nat.complement : Nat -> Nat
  403. Nat.drop : Nat -> Nat -> Nat
  404. Nat.eq : Nat -> Nat -> Boolean
  405. Nat.fromText : Text -> Optional Nat
  406. Nat.gt : Nat -> Nat -> Boolean
  407. Nat.gteq : Nat -> Nat -> Boolean
  408. Nat.increment : Nat -> Nat
  409. Nat.isEven : Nat -> Boolean
  410. Nat.isOdd : Nat -> Boolean
  411. Nat.leadingZeros : Nat -> Nat
  412. Nat.lt : Nat -> Nat -> Boolean
  413. Nat.lteq : Nat -> Nat -> Boolean
  414. Nat.mod : Nat -> Nat -> Nat
  415. Nat.or : Nat -> Nat -> Nat
  416. Nat.popCount : Nat -> Nat
  417. Nat.pow : Nat -> Nat -> Nat
  418. Nat.shiftLeft : Nat -> Nat -> Nat
  419. Nat.shiftRight : Nat -> Nat -> Nat
  420. Nat.sub : Nat -> Nat -> Int
  421. Nat.toFloat : Nat -> Float
  422. Nat.toInt : Nat -> Int
  423. Nat.toText : Nat -> Text
  424. Nat.trailingZeros : Nat -> Nat
  425. Nat.xor : Nat -> Nat -> Nat
  426. structural type Optional a
  427. Optional.None : Optional a
  428. Optional.Some : a -> Optional a
  429. builtin type Pattern
  430. Pattern.capture : Pattern a -> Pattern a
  431. Pattern.isMatch : Pattern a -> a -> Boolean
  432. Pattern.join : [Pattern a] -> Pattern a
  433. Pattern.many : Pattern a -> Pattern a
  434. Pattern.or : Pattern a -> Pattern a -> Pattern a
  435. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  436. Pattern.run : Pattern a -> a -> Optional ([a], a)
  437. builtin type Ref
  438. Ref.read : Ref g a ->{g} a
  439. Ref.write : Ref g a -> a ->{g} ()
  440. builtin type Request
  441. builtin type Scope
  442. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  443. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  444. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  445. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  446. Scope.ref : a ->{Scope s} Ref {Scope s} a
  447. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  448. structural type SeqView a b
  449. SeqView.VElem : a -> b -> SeqView a b
  450. SeqView.VEmpty : SeqView a b
  451. Socket.toText : Socket -> Text
  452. unique type Test.Result
  453. Test.Result.Fail : Text -> Result
  454. Test.Result.Ok : Text -> Result
  455. builtin type Text
  456. Text.!= : Text -> Text -> Boolean
  457. Text.++ : Text -> Text -> Text
  458. Text.drop : Nat -> Text -> Text
  459. Text.empty : Text
  460. Text.eq : Text -> Text -> Boolean
  461. Text.fromCharList : [Char] -> Text
  462. Text.fromUtf8.impl : Bytes -> Either Failure Text
  463. Text.gt : Text -> Text -> Boolean
  464. Text.gteq : Text -> Text -> Boolean
  465. Text.indexOf : Text -> Text -> Optional Nat
  466. Text.lt : Text -> Text -> Boolean
  467. Text.lteq : Text -> Text -> Boolean
  468. Text.patterns.anyChar : Pattern Text
  469. Text.patterns.char : Class -> Pattern Text
  470. Text.patterns.charIn : [Char] -> Pattern Text
  471. Text.patterns.charRange : Char -> Char -> Pattern Text
  472. Text.patterns.digit : Pattern Text
  473. Text.patterns.eof : Pattern Text
  474. Text.patterns.letter : Pattern Text
  475. Text.patterns.literal : Text -> Pattern Text
  476. Text.patterns.notCharIn : [Char] -> Pattern Text
  477. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  478. Text.patterns.punctuation : Pattern Text
  479. Text.patterns.space : Pattern Text
  480. Text.repeat : Nat -> Text -> Text
  481. Text.reverse : Text -> Text
  482. Text.size : Text -> Nat
  483. Text.take : Nat -> Text -> Text
  484. Text.toCharList : Text -> [Char]
  485. Text.toLowercase : Text -> Text
  486. Text.toUppercase : Text -> Text
  487. Text.toUtf8 : Text -> Bytes
  488. Text.uncons : Text -> Optional (Char, Text)
  489. Text.unsnoc : Text -> Optional (Text, Char)
  490. ThreadId.toText : ThreadId -> Text
  491. todo : a -> b
  492. structural type Tuple a b
  493. Tuple.Cons : a -> b -> Tuple a b
  494. structural type Unit
  495. Unit.Unit : ()
  496. Universal.< : a -> a -> Boolean
  497. Universal.<= : a -> a -> Boolean
  498. Universal.== : a -> a -> Boolean
  499. Universal.> : a -> a -> Boolean
  500. Universal.>= : a -> a -> Boolean
  501. Universal.compare : a -> a -> Int
  502. Universal.murmurHash : a -> Nat
  503. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  504. builtin type Value
  505. Value.dependencies : Value -> [Term]
  506. Value.deserialize : Bytes -> Either Text Value
  507. Value.load : Value ->{IO} Either [Term] a
  508. Value.serialize : Value -> Bytes
  509. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  structural type Either a b
    2.  structural ability Exception
    3.  builtin type Float
    4.  Doc.Evaluate    : Term -> Doc
    5.  Doc.Join        : [Doc] -> Doc
    6.  Either.Left     : a -> Either a b
    7.  Doc.Link        : Link -> Doc
    8.  Either.Right    : b -> Either a b
    9.  Doc.Signature   : Term -> Doc
    10. Doc.Source      : Link -> Doc
    11. Exception.raise : Failure ->{Exception} x
  
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

  1.  Doc.Evaluate : Term -> Doc
  2.  Doc.Join : [Doc] -> Doc
  3.  Doc.Link : Link -> Doc
  4.  Doc.Signature : Term -> Doc
  5.  Doc.Source : Link -> Doc
  6.  structural type Either a b
  7.  Either.Left : a -> Either a b
  8.  Either.Right : b -> Either a b
  9.  structural ability Exception
  10. Exception.raise : Failure ->{Exception} x
  11. builtin type Float
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
