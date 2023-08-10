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
  75.  Code.validateLinks : [(Term, Code)] ->{Exception} [Term]
  76.  crypto.hash : HashAlgorithm -> a -> Bytes
  77.  builtin type crypto.HashAlgorithm
  78.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  79.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  80.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  81.  crypto.HashAlgorithm.Md5 : HashAlgorithm
  82.  crypto.HashAlgorithm.Sha1 : HashAlgorithm
  83.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  84.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  85.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  86.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  87.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  88.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  89.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  90.  Debug.toText : a -> Optional (Either Text Text)
  91.  Debug.trace : Text -> a -> ()
  92.  Debug.watch : Text -> a -> a
  93.  unique type Doc
  94.  Doc.Blob : Text -> Doc
  95.  Doc.Evaluate : Term -> Doc
  96.  Doc.Join : [Doc] -> Doc
  97.  Doc.Link : Link -> Doc
  98.  Doc.Signature : Term -> Doc
  99.  Doc.Source : Link -> Doc
  100. structural type Either a b
  101. Either.Left : a -> Either a b
  102. Either.Right : b -> Either a b
  103. structural ability Exception
  104. Exception.raise : Failure ->{Exception} x
  105. builtin type Float
  106. Float.* : Float -> Float -> Float
  107. Float.+ : Float -> Float -> Float
  108. Float.- : Float -> Float -> Float
  109. Float./ : Float -> Float -> Float
  110. Float.abs : Float -> Float
  111. Float.acos : Float -> Float
  112. Float.acosh : Float -> Float
  113. Float.asin : Float -> Float
  114. Float.asinh : Float -> Float
  115. Float.atan : Float -> Float
  116. Float.atan2 : Float -> Float -> Float
  117. Float.atanh : Float -> Float
  118. Float.ceiling : Float -> Int
  119. Float.cos : Float -> Float
  120. Float.cosh : Float -> Float
  121. Float.eq : Float -> Float -> Boolean
  122. Float.exp : Float -> Float
  123. Float.floor : Float -> Int
  124. Float.fromRepresentation : Nat -> Float
  125. Float.fromText : Text -> Optional Float
  126. Float.gt : Float -> Float -> Boolean
  127. Float.gteq : Float -> Float -> Boolean
  128. Float.log : Float -> Float
  129. Float.logBase : Float -> Float -> Float
  130. Float.lt : Float -> Float -> Boolean
  131. Float.lteq : Float -> Float -> Boolean
  132. Float.max : Float -> Float -> Float
  133. Float.min : Float -> Float -> Float
  134. Float.pow : Float -> Float -> Float
  135. Float.round : Float -> Int
  136. Float.sin : Float -> Float
  137. Float.sinh : Float -> Float
  138. Float.sqrt : Float -> Float
  139. Float.tan : Float -> Float
  140. Float.tanh : Float -> Float
  141. Float.toRepresentation : Float -> Nat
  142. Float.toText : Float -> Text
  143. Float.truncate : Float -> Int
  144. Handle.toText : Handle -> Text
  145. builtin type ImmutableArray
  146. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  147. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  148. ImmutableArray.size : ImmutableArray a -> Nat
  149. builtin type ImmutableByteArray
  150. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  151. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  152. ImmutableByteArray.read24be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  153. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  154. ImmutableByteArray.read40be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  155. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  156. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  157. ImmutableByteArray.size : ImmutableByteArray -> Nat
  158. builtin type Int
  159. Int.* : Int -> Int -> Int
  160. Int.+ : Int -> Int -> Int
  161. Int.- : Int -> Int -> Int
  162. Int./ : Int -> Int -> Int
  163. Int.and : Int -> Int -> Int
  164. Int.complement : Int -> Int
  165. Int.eq : Int -> Int -> Boolean
  166. Int.fromRepresentation : Nat -> Int
  167. Int.fromText : Text -> Optional Int
  168. Int.gt : Int -> Int -> Boolean
  169. Int.gteq : Int -> Int -> Boolean
  170. Int.increment : Int -> Int
  171. Int.isEven : Int -> Boolean
  172. Int.isOdd : Int -> Boolean
  173. Int.leadingZeros : Int -> Nat
  174. Int.lt : Int -> Int -> Boolean
  175. Int.lteq : Int -> Int -> Boolean
  176. Int.mod : Int -> Int -> Int
  177. Int.negate : Int -> Int
  178. Int.or : Int -> Int -> Int
  179. Int.popCount : Int -> Nat
  180. Int.pow : Int -> Nat -> Int
  181. Int.shiftLeft : Int -> Nat -> Int
  182. Int.shiftRight : Int -> Nat -> Int
  183. Int.signum : Int -> Int
  184. Int.toFloat : Int -> Float
  185. Int.toRepresentation : Int -> Nat
  186. Int.toText : Int -> Text
  187. Int.trailingZeros : Int -> Nat
  188. Int.truncate0 : Int -> Nat
  189. Int.xor : Int -> Int -> Int
  190. unique type io2.ArithmeticFailure
  191. unique type io2.ArrayFailure
  192. unique type io2.BufferMode
  193. io2.BufferMode.BlockBuffering : BufferMode
  194. io2.BufferMode.LineBuffering : BufferMode
  195. io2.BufferMode.NoBuffering : BufferMode
  196. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  197. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  198. io2.Clock.internals.nsec : TimeSpec -> Nat
  199. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  200. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  201. io2.Clock.internals.sec : TimeSpec -> Int
  202. io2.Clock.internals.systemTimeZone : Int
                                            ->{IO} ( Int,
                                              Nat,
                                              Text)
  203. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  204. builtin type io2.Clock.internals.TimeSpec
  205. unique type io2.Failure
  206. io2.Failure.Failure : Type -> Text -> Any -> Failure
  207. unique type io2.FileMode
  208. io2.FileMode.Append : FileMode
  209. io2.FileMode.Read : FileMode
  210. io2.FileMode.ReadWrite : FileMode
  211. io2.FileMode.Write : FileMode
  212. builtin type io2.Handle
  213. builtin type io2.IO
  214. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  215. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  216. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  217. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  218. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  219. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  220. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  221. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  222. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  223. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  224. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  225. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  226. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  227. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  228. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  229. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  230. io2.IO.getChar.impl : Handle ->{IO} Either Failure Char
  231. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  232. io2.IO.getEcho.impl : Handle
                             ->{IO} Either Failure Boolean
  233. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  234. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  235. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  236. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  237. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  238. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  239. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  240. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  241. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  242. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  243. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  244. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  245. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  246. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  247. io2.IO.process.call : Text -> [Text] ->{IO} Nat
  248. io2.IO.process.exitCode : ProcessHandle
                                 ->{IO} Optional Nat
  249. io2.IO.process.kill : ProcessHandle ->{IO} ()
  250. io2.IO.process.start : Text
                              -> [Text]
                              ->{IO} ( Handle,
                                Handle,
                                Handle,
                                ProcessHandle)
  251. io2.IO.process.wait : ProcessHandle ->{IO} Nat
  252. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  253. io2.IO.randomBytes : Nat ->{IO} Bytes
  254. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  255. io2.IO.ref : a ->{IO} Ref {IO} a
  256. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  257. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  258. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  259. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  260. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  261. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  262. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  263. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  264. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  265. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  266. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  267. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  268. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  269. io2.IO.stdHandle : StdHandle -> Handle
  270. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  271. io2.IO.systemTimeMicroseconds : '{IO} Int
  272. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  273. unique type io2.IOError
  274. io2.IOError.AlreadyExists : IOError
  275. io2.IOError.EOF : IOError
  276. io2.IOError.IllegalOperation : IOError
  277. io2.IOError.NoSuchThing : IOError
  278. io2.IOError.PermissionDenied : IOError
  279. io2.IOError.ResourceBusy : IOError
  280. io2.IOError.ResourceExhausted : IOError
  281. io2.IOError.UserError : IOError
  282. unique type io2.IOFailure
  283. unique type io2.MiscFailure
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
  295. builtin type io2.ProcessHandle
  296. builtin type io2.Promise
  297. io2.Promise.new : '{IO} Promise a
  298. io2.Promise.read : Promise a ->{IO} a
  299. io2.Promise.tryRead : Promise a ->{IO} Optional a
  300. io2.Promise.write : Promise a -> a ->{IO} Boolean
  301. io2.Ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
  302. io2.Ref.readForCas : Ref {IO} a ->{IO} Ticket a
  303. builtin type io2.Ref.Ticket
  304. io2.Ref.Ticket.read : Ticket a -> a
  305. unique type io2.RuntimeFailure
  306. unique type io2.SeekMode
  307. io2.SeekMode.AbsoluteSeek : SeekMode
  308. io2.SeekMode.RelativeSeek : SeekMode
  309. io2.SeekMode.SeekFromEnd : SeekMode
  310. builtin type io2.Socket
  311. unique type io2.StdHandle
  312. io2.StdHandle.StdErr : StdHandle
  313. io2.StdHandle.StdIn : StdHandle
  314. io2.StdHandle.StdOut : StdHandle
  315. builtin type io2.STM
  316. io2.STM.atomically : '{STM} a ->{IO} a
  317. io2.STM.retry : '{STM} a
  318. unique type io2.STMFailure
  319. builtin type io2.ThreadId
  320. unique type io2.ThreadKilledFailure
  321. builtin type io2.Tls
  322. builtin type io2.Tls.Cipher
  323. builtin type io2.Tls.ClientConfig
  324. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  325. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  326. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  327. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  328. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  329. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  330. io2.Tls.encodeCert : SignedCert -> Bytes
  331. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  332. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  333. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  334. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  335. builtin type io2.Tls.PrivateKey
  336. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  337. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  338. builtin type io2.Tls.ServerConfig
  339. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  340. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  341. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  342. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  343. builtin type io2.Tls.SignedCert
  344. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  345. builtin type io2.Tls.Version
  346. unique type io2.TlsFailure
  347. builtin type io2.TVar
  348. io2.TVar.new : a ->{STM} TVar a
  349. io2.TVar.newIO : a ->{IO} TVar a
  350. io2.TVar.read : TVar a ->{STM} a
  351. io2.TVar.readIO : TVar a ->{IO} a
  352. io2.TVar.swap : TVar a -> a ->{STM} a
  353. io2.TVar.write : TVar a -> a ->{STM} ()
  354. io2.validateSandboxed : [Term] -> a -> Boolean
  355. unique type IsPropagated
  356. IsPropagated.IsPropagated : IsPropagated
  357. unique type IsTest
  358. IsTest.IsTest : IsTest
  359. unique type Link
  360. builtin type Link.Term
  361. Link.Term : Term -> Link
  362. Link.Term.toText : Term -> Text
  363. builtin type Link.Type
  364. Link.Type : Type -> Link
  365. builtin type List
  366. List.++ : [a] -> [a] -> [a]
  367. List.+: : a -> [a] -> [a]
  368. List.:+ : [a] -> a -> [a]
  369. List.at : Nat -> [a] -> Optional a
  370. List.cons : a -> [a] -> [a]
  371. List.drop : Nat -> [a] -> [a]
  372. List.empty : [a]
  373. List.size : [a] -> Nat
  374. List.snoc : [a] -> a -> [a]
  375. List.take : Nat -> [a] -> [a]
  376. metadata.isPropagated : IsPropagated
  377. metadata.isTest : IsTest
  378. builtin type MutableArray
  379. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  380. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  381. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  382. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  383. MutableArray.size : MutableArray g a -> Nat
  384. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  385. builtin type MutableByteArray
  386. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  387. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  388. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  389. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  390. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  391. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  392. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  393. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  394. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  395. MutableByteArray.size : MutableByteArray g -> Nat
  396. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  397. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  398. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  399. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  400. builtin type Nat
  401. Nat.* : Nat -> Nat -> Nat
  402. Nat.+ : Nat -> Nat -> Nat
  403. Nat./ : Nat -> Nat -> Nat
  404. Nat.and : Nat -> Nat -> Nat
  405. Nat.complement : Nat -> Nat
  406. Nat.drop : Nat -> Nat -> Nat
  407. Nat.eq : Nat -> Nat -> Boolean
  408. Nat.fromText : Text -> Optional Nat
  409. Nat.gt : Nat -> Nat -> Boolean
  410. Nat.gteq : Nat -> Nat -> Boolean
  411. Nat.increment : Nat -> Nat
  412. Nat.isEven : Nat -> Boolean
  413. Nat.isOdd : Nat -> Boolean
  414. Nat.leadingZeros : Nat -> Nat
  415. Nat.lt : Nat -> Nat -> Boolean
  416. Nat.lteq : Nat -> Nat -> Boolean
  417. Nat.mod : Nat -> Nat -> Nat
  418. Nat.or : Nat -> Nat -> Nat
  419. Nat.popCount : Nat -> Nat
  420. Nat.pow : Nat -> Nat -> Nat
  421. Nat.shiftLeft : Nat -> Nat -> Nat
  422. Nat.shiftRight : Nat -> Nat -> Nat
  423. Nat.sub : Nat -> Nat -> Int
  424. Nat.toFloat : Nat -> Float
  425. Nat.toInt : Nat -> Int
  426. Nat.toText : Nat -> Text
  427. Nat.trailingZeros : Nat -> Nat
  428. Nat.xor : Nat -> Nat -> Nat
  429. structural type Optional a
  430. Optional.None : Optional a
  431. Optional.Some : a -> Optional a
  432. builtin type Pattern
  433. Pattern.capture : Pattern a -> Pattern a
  434. Pattern.isMatch : Pattern a -> a -> Boolean
  435. Pattern.join : [Pattern a] -> Pattern a
  436. Pattern.many : Pattern a -> Pattern a
  437. Pattern.or : Pattern a -> Pattern a -> Pattern a
  438. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  439. Pattern.run : Pattern a -> a -> Optional ([a], a)
  440. builtin type Ref
  441. Ref.read : Ref g a ->{g} a
  442. Ref.write : Ref g a -> a ->{g} ()
  443. builtin type Request
  444. unique type RewriteCase a b
  445. RewriteCase.RewriteCase : a -> b -> RewriteCase a b
  446. unique type Rewrites a
  447. Rewrites.Rewrites : a -> Rewrites a
  448. unique type RewriteSignature a b
  449. RewriteSignature.RewriteSignature : (a -> b -> ())
                                           -> RewriteSignature
                                             a b
  450. unique type RewriteTerm a b
  451. RewriteTerm.RewriteTerm : a -> b -> RewriteTerm a b
  452. builtin type Scope
  453. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  454. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  455. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  456. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  457. Scope.ref : a ->{Scope s} Ref {Scope s} a
  458. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  459. structural type SeqView a b
  460. SeqView.VElem : a -> b -> SeqView a b
  461. SeqView.VEmpty : SeqView a b
  462. Socket.toText : Socket -> Text
  463. unique type Test.Result
  464. Test.Result.Fail : Text -> Result
  465. Test.Result.Ok : Text -> Result
  466. builtin type Text
  467. Text.!= : Text -> Text -> Boolean
  468. Text.++ : Text -> Text -> Text
  469. Text.drop : Nat -> Text -> Text
  470. Text.empty : Text
  471. Text.eq : Text -> Text -> Boolean
  472. Text.fromCharList : [Char] -> Text
  473. Text.fromUtf8.impl : Bytes -> Either Failure Text
  474. Text.gt : Text -> Text -> Boolean
  475. Text.gteq : Text -> Text -> Boolean
  476. Text.indexOf : Text -> Text -> Optional Nat
  477. Text.lt : Text -> Text -> Boolean
  478. Text.lteq : Text -> Text -> Boolean
  479. Text.patterns.anyChar : Pattern Text
  480. Text.patterns.char : Class -> Pattern Text
  481. Text.patterns.charIn : [Char] -> Pattern Text
  482. Text.patterns.charRange : Char -> Char -> Pattern Text
  483. Text.patterns.digit : Pattern Text
  484. Text.patterns.eof : Pattern Text
  485. Text.patterns.letter : Pattern Text
  486. Text.patterns.literal : Text -> Pattern Text
  487. Text.patterns.notCharIn : [Char] -> Pattern Text
  488. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  489. Text.patterns.punctuation : Pattern Text
  490. Text.patterns.space : Pattern Text
  491. Text.repeat : Nat -> Text -> Text
  492. Text.reverse : Text -> Text
  493. Text.size : Text -> Nat
  494. Text.take : Nat -> Text -> Text
  495. Text.toCharList : Text -> [Char]
  496. Text.toLowercase : Text -> Text
  497. Text.toUppercase : Text -> Text
  498. Text.toUtf8 : Text -> Bytes
  499. Text.uncons : Text -> Optional (Char, Text)
  500. Text.unsnoc : Text -> Optional (Text, Char)
  501. ThreadId.toText : ThreadId -> Text
  502. todo : a -> b
  503. structural type Tuple a b
  504. Tuple.Cons : a -> b -> Tuple a b
  505. structural type Unit
  506. Unit.Unit : ()
  507. Universal.< : a -> a -> Boolean
  508. Universal.<= : a -> a -> Boolean
  509. Universal.== : a -> a -> Boolean
  510. Universal.> : a -> a -> Boolean
  511. Universal.>= : a -> a -> Boolean
  512. Universal.compare : a -> a -> Int
  513. Universal.murmurHash : a -> Nat
  514. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  515. builtin type Value
  516. Value.dependencies : Value -> [Term]
  517. Value.deserialize : Bytes -> Either Text Value
  518. Value.load : Value ->{IO} Either [Term] a
  519. Value.serialize : Value -> Bytes
  520. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  structural type Either a b
    2.  structural ability Exception
    3.  Doc.Blob        : Text -> Doc
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

  1.  Doc.Blob : Text -> Doc
  2.  Doc.Evaluate : Term -> Doc
  3.  Doc.Join : [Doc] -> Doc
  4.  Doc.Link : Link -> Doc
  5.  Doc.Signature : Term -> Doc
  6.  Doc.Source : Link -> Doc
  7.  structural type Either a b
  8.  Either.Left : a -> Either a b
  9.  Either.Right : b -> Either a b
  10. structural ability Exception
  11. Exception.raise : Failure ->{Exception} x
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
