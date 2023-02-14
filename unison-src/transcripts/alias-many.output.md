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
  42.  builtin type Char.Class
  43.  Char.Class.alphanumeric : Class
  44.  Char.Class.and : Class -> Class -> Class
  45.  Char.Class.any : Class
  46.  Char.Class.anyOf : [Char] -> Class
  47.  Char.Class.control : Class
  48.  Char.Class.is : Class -> Char -> Boolean
  49.  Char.Class.letter : Class
  50.  Char.Class.lower : Class
  51.  Char.Class.mark : Class
  52.  Char.Class.not : Class -> Class
  53.  Char.Class.number : Class
  54.  Char.Class.or : Class -> Class -> Class
  55.  Char.Class.printable : Class
  56.  Char.Class.punctuation : Class
  57.  Char.Class.range : Char -> Char -> Class
  58.  Char.Class.separator : Class
  59.  Char.Class.symbol : Class
  60.  Char.Class.upper : Class
  61.  Char.Class.whitespace : Class
  62.  Char.fromNat : Nat -> Char
  63.  Char.toNat : Char -> Nat
  64.  Char.toText : Char -> Text
  65.  builtin type Code
  66.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  67.  Code.dependencies : Code -> [Term]
  68.  Code.deserialize : Bytes -> Either Text Code
  69.  Code.display : Text -> Code -> Text
  70.  Code.isMissing : Term ->{IO} Boolean
  71.  Code.lookup : Term ->{IO} Optional Code
  72.  Code.serialize : Code -> Bytes
  73.  Code.validate : [(Term, Code)] ->{IO} Optional Failure
  74.  crypto.hash : HashAlgorithm -> a -> Bytes
  75.  builtin type crypto.HashAlgorithm
  76.  crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  77.  crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  78.  crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  79.  crypto.HashAlgorithm.Sha1 : HashAlgorithm
  80.  crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  81.  crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  82.  crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  83.  crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  84.  crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  85.  crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  86.  crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  87.  Debug.toText : a -> Optional (Either Text Text)
  88.  Debug.trace : Text -> a -> ()
  89.  Debug.watch : Text -> a -> a
  90.  unique type Doc
  91.  Doc.Blob : Text -> Doc
  92.  Doc.Evaluate : Term -> Doc
  93.  Doc.Join : [Doc] -> Doc
  94.  Doc.Link : Link -> Doc
  95.  Doc.Signature : Term -> Doc
  96.  Doc.Source : Link -> Doc
  97.  structural type Either a b
  98.  Either.Left : a -> Either a b
  99.  Either.Right : b -> Either a b
  100. structural ability Exception
  101. Exception.raise : Failure ->{Exception} x
  102. builtin type Float
  103. Float.* : Float -> Float -> Float
  104. Float.+ : Float -> Float -> Float
  105. Float.- : Float -> Float -> Float
  106. Float./ : Float -> Float -> Float
  107. Float.abs : Float -> Float
  108. Float.acos : Float -> Float
  109. Float.acosh : Float -> Float
  110. Float.asin : Float -> Float
  111. Float.asinh : Float -> Float
  112. Float.atan : Float -> Float
  113. Float.atan2 : Float -> Float -> Float
  114. Float.atanh : Float -> Float
  115. Float.ceiling : Float -> Int
  116. Float.cos : Float -> Float
  117. Float.cosh : Float -> Float
  118. Float.eq : Float -> Float -> Boolean
  119. Float.exp : Float -> Float
  120. Float.floor : Float -> Int
  121. Float.fromRepresentation : Nat -> Float
  122. Float.fromText : Text -> Optional Float
  123. Float.gt : Float -> Float -> Boolean
  124. Float.gteq : Float -> Float -> Boolean
  125. Float.log : Float -> Float
  126. Float.logBase : Float -> Float -> Float
  127. Float.lt : Float -> Float -> Boolean
  128. Float.lteq : Float -> Float -> Boolean
  129. Float.max : Float -> Float -> Float
  130. Float.min : Float -> Float -> Float
  131. Float.pow : Float -> Float -> Float
  132. Float.round : Float -> Int
  133. Float.sin : Float -> Float
  134. Float.sinh : Float -> Float
  135. Float.sqrt : Float -> Float
  136. Float.tan : Float -> Float
  137. Float.tanh : Float -> Float
  138. Float.toRepresentation : Float -> Nat
  139. Float.toText : Float -> Text
  140. Float.truncate : Float -> Int
  141. Handle.toText : Handle -> Text
  142. builtin type ImmutableArray
  143. ImmutableArray.copyTo! : MutableArray g a
                                -> Nat
                                -> ImmutableArray a
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
  144. ImmutableArray.read : ImmutableArray a
                             -> Nat
                             ->{Exception} a
  145. ImmutableArray.size : ImmutableArray a -> Nat
  146. builtin type ImmutableByteArray
  147. ImmutableByteArray.copyTo! : MutableByteArray g
                                    -> Nat
                                    -> ImmutableByteArray
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  148. ImmutableByteArray.read16be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  149. ImmutableByteArray.read24be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  150. ImmutableByteArray.read32be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  151. ImmutableByteArray.read40be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  152. ImmutableByteArray.read64be : ImmutableByteArray
                                     -> Nat
                                     ->{Exception} Nat
  153. ImmutableByteArray.read8 : ImmutableByteArray
                                  -> Nat
                                  ->{Exception} Nat
  154. ImmutableByteArray.size : ImmutableByteArray -> Nat
  155. builtin type Int
  156. Int.* : Int -> Int -> Int
  157. Int.+ : Int -> Int -> Int
  158. Int.- : Int -> Int -> Int
  159. Int./ : Int -> Int -> Int
  160. Int.and : Int -> Int -> Int
  161. Int.complement : Int -> Int
  162. Int.eq : Int -> Int -> Boolean
  163. Int.fromRepresentation : Nat -> Int
  164. Int.fromText : Text -> Optional Int
  165. Int.gt : Int -> Int -> Boolean
  166. Int.gteq : Int -> Int -> Boolean
  167. Int.increment : Int -> Int
  168. Int.isEven : Int -> Boolean
  169. Int.isOdd : Int -> Boolean
  170. Int.leadingZeros : Int -> Nat
  171. Int.lt : Int -> Int -> Boolean
  172. Int.lteq : Int -> Int -> Boolean
  173. Int.mod : Int -> Int -> Int
  174. Int.negate : Int -> Int
  175. Int.or : Int -> Int -> Int
  176. Int.popCount : Int -> Nat
  177. Int.pow : Int -> Nat -> Int
  178. Int.shiftLeft : Int -> Nat -> Int
  179. Int.shiftRight : Int -> Nat -> Int
  180. Int.signum : Int -> Int
  181. Int.toFloat : Int -> Float
  182. Int.toRepresentation : Int -> Nat
  183. Int.toText : Int -> Text
  184. Int.trailingZeros : Int -> Nat
  185. Int.truncate0 : Int -> Nat
  186. Int.xor : Int -> Int -> Int
  187. unique type io2.ArithmeticFailure
  188. unique type io2.ArrayFailure
  189. unique type io2.BufferMode
  190. io2.BufferMode.BlockBuffering : BufferMode
  191. io2.BufferMode.LineBuffering : BufferMode
  192. io2.BufferMode.NoBuffering : BufferMode
  193. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  194. io2.Clock.internals.monotonic : '{IO} Either
                                         Failure TimeSpec
  195. io2.Clock.internals.nsec : TimeSpec -> Nat
  196. io2.Clock.internals.processCPUTime : '{IO} Either
                                              Failure TimeSpec
  197. io2.Clock.internals.realtime : '{IO} Either
                                        Failure TimeSpec
  198. io2.Clock.internals.sec : TimeSpec -> Int
  199. io2.Clock.internals.threadCPUTime : '{IO} Either
                                             Failure TimeSpec
  200. builtin type io2.Clock.internals.TimeSpec
  201. unique type io2.Failure
  202. io2.Failure.Failure : Type -> Text -> Any -> Failure
  203. unique type io2.FileMode
  204. io2.FileMode.Append : FileMode
  205. io2.FileMode.Read : FileMode
  206. io2.FileMode.ReadWrite : FileMode
  207. io2.FileMode.Write : FileMode
  208. builtin type io2.Handle
  209. builtin type io2.IO
  210. io2.IO.array : Nat ->{IO} MutableArray {IO} a
  211. io2.IO.arrayOf : a -> Nat ->{IO} MutableArray {IO} a
  212. io2.IO.bytearray : Nat ->{IO} MutableByteArray {IO}
  213. io2.IO.bytearrayOf : Nat
                            -> Nat
                            ->{IO} MutableByteArray {IO}
  214. io2.IO.clientSocket.impl : Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  215. io2.IO.closeFile.impl : Handle ->{IO} Either Failure ()
  216. io2.IO.closeSocket.impl : Socket ->{IO} Either Failure ()
  217. io2.IO.createDirectory.impl : Text
                                     ->{IO} Either Failure ()
  218. io2.IO.createTempDirectory.impl : Text
                                         ->{IO} Either
                                           Failure Text
  219. io2.IO.delay.impl : Nat ->{IO} Either Failure ()
  220. io2.IO.directoryContents.impl : Text
                                       ->{IO} Either
                                         Failure [Text]
  221. io2.IO.fileExists.impl : Text
                                ->{IO} Either Failure Boolean
  222. io2.IO.forkComp : '{IO} a ->{IO} ThreadId
  223. io2.IO.getArgs.impl : '{IO} Either Failure [Text]
  224. io2.IO.getBuffering.impl : Handle
                                  ->{IO} Either
                                    Failure BufferMode
  225. io2.IO.getBytes.impl : Handle
                              -> Nat
                              ->{IO} Either Failure Bytes
  226. io2.IO.getChar.impl : Handle ->{IO} Either Failure Char
  227. io2.IO.getCurrentDirectory.impl : '{IO} Either
                                           Failure Text
  228. io2.IO.getEcho.impl : Handle
                             ->{IO} Either Failure Boolean
  229. io2.IO.getEnv.impl : Text ->{IO} Either Failure Text
  230. io2.IO.getFileSize.impl : Text ->{IO} Either Failure Nat
  231. io2.IO.getFileTimestamp.impl : Text
                                      ->{IO} Either Failure Nat
  232. io2.IO.getLine.impl : Handle ->{IO} Either Failure Text
  233. io2.IO.getSomeBytes.impl : Handle
                                  -> Nat
                                  ->{IO} Either Failure Bytes
  234. io2.IO.getTempDirectory.impl : '{IO} Either Failure Text
  235. io2.IO.handlePosition.impl : Handle
                                    ->{IO} Either Failure Nat
  236. io2.IO.isDirectory.impl : Text
                                 ->{IO} Either Failure Boolean
  237. io2.IO.isFileEOF.impl : Handle
                               ->{IO} Either Failure Boolean
  238. io2.IO.isFileOpen.impl : Handle
                                ->{IO} Either Failure Boolean
  239. io2.IO.isSeekable.impl : Handle
                                ->{IO} Either Failure Boolean
  240. io2.IO.kill.impl : ThreadId ->{IO} Either Failure ()
  241. io2.IO.listen.impl : Socket ->{IO} Either Failure ()
  242. io2.IO.openFile.impl : Text
                              -> FileMode
                              ->{IO} Either Failure Handle
  243. io2.IO.process.call : Text -> [Text] ->{IO} Nat
  244. io2.IO.process.exitCode : ProcessHandle
                                 ->{IO} Optional Nat
  245. io2.IO.process.kill : ProcessHandle ->{IO} ()
  246. io2.IO.process.start : Text
                              -> [Text]
                              ->{IO} ( Handle,
                                Handle,
                                Handle,
                                ProcessHandle)
  247. io2.IO.process.wait : ProcessHandle ->{IO} Nat
  248. io2.IO.putBytes.impl : Handle
                              -> Bytes
                              ->{IO} Either Failure ()
  249. io2.IO.ready.impl : Handle ->{IO} Either Failure Boolean
  250. io2.IO.ref : a ->{IO} Ref {IO} a
  251. io2.IO.removeDirectory.impl : Text
                                     ->{IO} Either Failure ()
  252. io2.IO.removeFile.impl : Text ->{IO} Either Failure ()
  253. io2.IO.renameDirectory.impl : Text
                                     -> Text
                                     ->{IO} Either Failure ()
  254. io2.IO.renameFile.impl : Text
                                -> Text
                                ->{IO} Either Failure ()
  255. io2.IO.seekHandle.impl : Handle
                                -> SeekMode
                                -> Int
                                ->{IO} Either Failure ()
  256. io2.IO.serverSocket.impl : Optional Text
                                  -> Text
                                  ->{IO} Either Failure Socket
  257. io2.IO.setBuffering.impl : Handle
                                  -> BufferMode
                                  ->{IO} Either Failure ()
  258. io2.IO.setCurrentDirectory.impl : Text
                                         ->{IO} Either
                                           Failure ()
  259. io2.IO.setEcho.impl : Handle
                             -> Boolean
                             ->{IO} Either Failure ()
  260. io2.IO.socketAccept.impl : Socket
                                  ->{IO} Either Failure Socket
  261. io2.IO.socketPort.impl : Socket ->{IO} Either Failure Nat
  262. io2.IO.socketReceive.impl : Socket
                                   -> Nat
                                   ->{IO} Either Failure Bytes
  263. io2.IO.socketSend.impl : Socket
                                -> Bytes
                                ->{IO} Either Failure ()
  264. io2.IO.stdHandle : StdHandle -> Handle
  265. io2.IO.systemTime.impl : '{IO} Either Failure Nat
  266. io2.IO.systemTimeMicroseconds : '{IO} Int
  267. io2.IO.tryEval : '{IO} a ->{IO, Exception} a
  268. unique type io2.IOError
  269. io2.IOError.AlreadyExists : IOError
  270. io2.IOError.EOF : IOError
  271. io2.IOError.IllegalOperation : IOError
  272. io2.IOError.NoSuchThing : IOError
  273. io2.IOError.PermissionDenied : IOError
  274. io2.IOError.ResourceBusy : IOError
  275. io2.IOError.ResourceExhausted : IOError
  276. io2.IOError.UserError : IOError
  277. unique type io2.IOFailure
  278. unique type io2.MiscFailure
  279. builtin type io2.MVar
  280. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  281. io2.MVar.new : a ->{IO} MVar a
  282. io2.MVar.newEmpty : '{IO} MVar a
  283. io2.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
  284. io2.MVar.read.impl : MVar a ->{IO} Either Failure a
  285. io2.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
  286. io2.MVar.take.impl : MVar a ->{IO} Either Failure a
  287. io2.MVar.tryPut.impl : MVar a
                              -> a
                              ->{IO} Either Failure Boolean
  288. io2.MVar.tryRead.impl : MVar a
                               ->{IO} Either
                                 Failure (Optional a)
  289. io2.MVar.tryTake : MVar a ->{IO} Optional a
  290. builtin type io2.ProcessHandle
  291. builtin type io2.Promise
  292. io2.Promise.new : '{IO} Promise a
  293. io2.Promise.read : Promise a ->{IO} a
  294. io2.Promise.tryRead : Promise a ->{IO} Optional a
  295. io2.Promise.write : Promise a -> a ->{IO} Boolean
  296. io2.Ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
  297. io2.Ref.readForCas : Ref {IO} a ->{IO} Ticket a
  298. builtin type io2.Ref.Ticket
  299. io2.Ref.Ticket.read : Ticket a -> a
  300. unique type io2.RuntimeFailure
  301. unique type io2.SeekMode
  302. io2.SeekMode.AbsoluteSeek : SeekMode
  303. io2.SeekMode.RelativeSeek : SeekMode
  304. io2.SeekMode.SeekFromEnd : SeekMode
  305. builtin type io2.Socket
  306. unique type io2.StdHandle
  307. io2.StdHandle.StdErr : StdHandle
  308. io2.StdHandle.StdIn : StdHandle
  309. io2.StdHandle.StdOut : StdHandle
  310. builtin type io2.STM
  311. io2.STM.atomically : '{STM} a ->{IO} a
  312. io2.STM.retry : '{STM} a
  313. unique type io2.STMFailure
  314. builtin type io2.ThreadId
  315. builtin type io2.Tls
  316. builtin type io2.Tls.Cipher
  317. builtin type io2.Tls.ClientConfig
  318. io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                               -> ClientConfig
                                               -> ClientConfig
  319. io2.TLS.ClientConfig.ciphers.set : [Cipher]
                                          -> ClientConfig
                                          -> ClientConfig
  320. io2.Tls.ClientConfig.default : Text
                                      -> Bytes
                                      -> ClientConfig
  321. io2.Tls.ClientConfig.versions.set : [Version]
                                           -> ClientConfig
                                           -> ClientConfig
  322. io2.Tls.decodeCert.impl : Bytes
                                 -> Either Failure SignedCert
  323. io2.Tls.decodePrivateKey : Bytes -> [PrivateKey]
  324. io2.Tls.encodeCert : SignedCert -> Bytes
  325. io2.Tls.encodePrivateKey : PrivateKey -> Bytes
  326. io2.Tls.handshake.impl : Tls ->{IO} Either Failure ()
  327. io2.Tls.newClient.impl : ClientConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  328. io2.Tls.newServer.impl : ServerConfig
                                -> Socket
                                ->{IO} Either Failure Tls
  329. builtin type io2.Tls.PrivateKey
  330. io2.Tls.receive.impl : Tls ->{IO} Either Failure Bytes
  331. io2.Tls.send.impl : Tls -> Bytes ->{IO} Either Failure ()
  332. builtin type io2.Tls.ServerConfig
  333. io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                               -> ServerConfig
                                               -> ServerConfig
  334. io2.Tls.ServerConfig.ciphers.set : [Cipher]
                                          -> ServerConfig
                                          -> ServerConfig
  335. io2.Tls.ServerConfig.default : [SignedCert]
                                      -> PrivateKey
                                      -> ServerConfig
  336. io2.Tls.ServerConfig.versions.set : [Version]
                                           -> ServerConfig
                                           -> ServerConfig
  337. builtin type io2.Tls.SignedCert
  338. io2.Tls.terminate.impl : Tls ->{IO} Either Failure ()
  339. builtin type io2.Tls.Version
  340. unique type io2.TlsFailure
  341. builtin type io2.TVar
  342. io2.TVar.new : a ->{STM} TVar a
  343. io2.TVar.newIO : a ->{IO} TVar a
  344. io2.TVar.read : TVar a ->{STM} a
  345. io2.TVar.readIO : TVar a ->{IO} a
  346. io2.TVar.swap : TVar a -> a ->{STM} a
  347. io2.TVar.write : TVar a -> a ->{STM} ()
  348. io2.validateSandboxed : [Term] -> a -> Boolean
  349. unique type IsPropagated
  350. IsPropagated.IsPropagated : IsPropagated
  351. unique type IsTest
  352. IsTest.IsTest : IsTest
  353. unique type Link
  354. builtin type Link.Term
  355. Link.Term : Term -> Link
  356. Link.Term.toText : Term -> Text
  357. builtin type Link.Type
  358. Link.Type : Type -> Link
  359. builtin type List
  360. List.++ : [a] -> [a] -> [a]
  361. List.+: : a -> [a] -> [a]
  362. List.:+ : [a] -> a -> [a]
  363. List.at : Nat -> [a] -> Optional a
  364. List.cons : a -> [a] -> [a]
  365. List.drop : Nat -> [a] -> [a]
  366. List.empty : [a]
  367. List.size : [a] -> Nat
  368. List.snoc : [a] -> a -> [a]
  369. List.take : Nat -> [a] -> [a]
  370. metadata.isPropagated : IsPropagated
  371. metadata.isTest : IsTest
  372. builtin type MutableArray
  373. MutableArray.copyTo! : MutableArray g a
                              -> Nat
                              -> MutableArray g a
                              -> Nat
                              -> Nat
                              ->{g, Exception} ()
  374. MutableArray.freeze : MutableArray g a
                             -> Nat
                             -> Nat
                             ->{g} ImmutableArray a
  375. MutableArray.freeze! : MutableArray g a
                              ->{g} ImmutableArray a
  376. MutableArray.read : MutableArray g a
                           -> Nat
                           ->{g, Exception} a
  377. MutableArray.size : MutableArray g a -> Nat
  378. MutableArray.write : MutableArray g a
                            -> Nat
                            -> a
                            ->{g, Exception} ()
  379. builtin type MutableByteArray
  380. MutableByteArray.copyTo! : MutableByteArray g
                                  -> Nat
                                  -> MutableByteArray g
                                  -> Nat
                                  -> Nat
                                  ->{g, Exception} ()
  381. MutableByteArray.freeze : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g} ImmutableByteArray
  382. MutableByteArray.freeze! : MutableByteArray g
                                  ->{g} ImmutableByteArray
  383. MutableByteArray.read16be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  384. MutableByteArray.read24be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  385. MutableByteArray.read32be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  386. MutableByteArray.read40be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  387. MutableByteArray.read64be : MutableByteArray g
                                   -> Nat
                                   ->{g, Exception} Nat
  388. MutableByteArray.read8 : MutableByteArray g
                                -> Nat
                                ->{g, Exception} Nat
  389. MutableByteArray.size : MutableByteArray g -> Nat
  390. MutableByteArray.write16be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  391. MutableByteArray.write32be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  392. MutableByteArray.write64be : MutableByteArray g
                                    -> Nat
                                    -> Nat
                                    ->{g, Exception} ()
  393. MutableByteArray.write8 : MutableByteArray g
                                 -> Nat
                                 -> Nat
                                 ->{g, Exception} ()
  394. builtin type Nat
  395. Nat.* : Nat -> Nat -> Nat
  396. Nat.+ : Nat -> Nat -> Nat
  397. Nat./ : Nat -> Nat -> Nat
  398. Nat.and : Nat -> Nat -> Nat
  399. Nat.complement : Nat -> Nat
  400. Nat.drop : Nat -> Nat -> Nat
  401. Nat.eq : Nat -> Nat -> Boolean
  402. Nat.fromText : Text -> Optional Nat
  403. Nat.gt : Nat -> Nat -> Boolean
  404. Nat.gteq : Nat -> Nat -> Boolean
  405. Nat.increment : Nat -> Nat
  406. Nat.isEven : Nat -> Boolean
  407. Nat.isOdd : Nat -> Boolean
  408. Nat.leadingZeros : Nat -> Nat
  409. Nat.lt : Nat -> Nat -> Boolean
  410. Nat.lteq : Nat -> Nat -> Boolean
  411. Nat.mod : Nat -> Nat -> Nat
  412. Nat.or : Nat -> Nat -> Nat
  413. Nat.popCount : Nat -> Nat
  414. Nat.pow : Nat -> Nat -> Nat
  415. Nat.shiftLeft : Nat -> Nat -> Nat
  416. Nat.shiftRight : Nat -> Nat -> Nat
  417. Nat.sub : Nat -> Nat -> Int
  418. Nat.toFloat : Nat -> Float
  419. Nat.toInt : Nat -> Int
  420. Nat.toText : Nat -> Text
  421. Nat.trailingZeros : Nat -> Nat
  422. Nat.xor : Nat -> Nat -> Nat
  423. structural type Optional a
  424. Optional.None : Optional a
  425. Optional.Some : a -> Optional a
  426. builtin type Pattern
  427. Pattern.capture : Pattern a -> Pattern a
  428. Pattern.isMatch : Pattern a -> a -> Boolean
  429. Pattern.join : [Pattern a] -> Pattern a
  430. Pattern.many : Pattern a -> Pattern a
  431. Pattern.or : Pattern a -> Pattern a -> Pattern a
  432. Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
  433. Pattern.run : Pattern a -> a -> Optional ([a], a)
  434. builtin type Ref
  435. Ref.read : Ref g a ->{g} a
  436. Ref.write : Ref g a -> a ->{g} ()
  437. builtin type Request
  438. builtin type Scope
  439. Scope.array : Nat ->{Scope s} MutableArray (Scope s) a
  440. Scope.arrayOf : a
                       -> Nat
                       ->{Scope s} MutableArray (Scope s) a
  441. Scope.bytearray : Nat
                         ->{Scope s} MutableByteArray (Scope s)
  442. Scope.bytearrayOf : Nat
                           -> Nat
                           ->{Scope s} MutableByteArray
                             (Scope s)
  443. Scope.ref : a ->{Scope s} Ref {Scope s} a
  444. Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
  445. structural type SeqView a b
  446. SeqView.VElem : a -> b -> SeqView a b
  447. SeqView.VEmpty : SeqView a b
  448. Socket.toText : Socket -> Text
  449. unique type Test.Result
  450. Test.Result.Fail : Text -> Result
  451. Test.Result.Ok : Text -> Result
  452. builtin type Text
  453. Text.!= : Text -> Text -> Boolean
  454. Text.++ : Text -> Text -> Text
  455. Text.drop : Nat -> Text -> Text
  456. Text.empty : Text
  457. Text.eq : Text -> Text -> Boolean
  458. Text.fromCharList : [Char] -> Text
  459. Text.fromUtf8.impl : Bytes -> Either Failure Text
  460. Text.gt : Text -> Text -> Boolean
  461. Text.gteq : Text -> Text -> Boolean
  462. Text.lt : Text -> Text -> Boolean
  463. Text.lteq : Text -> Text -> Boolean
  464. Text.patterns.anyChar : Pattern Text
  465. Text.patterns.char : Class -> Pattern Text
  466. Text.patterns.charIn : [Char] -> Pattern Text
  467. Text.patterns.charRange : Char -> Char -> Pattern Text
  468. Text.patterns.digit : Pattern Text
  469. Text.patterns.eof : Pattern Text
  470. Text.patterns.letter : Pattern Text
  471. Text.patterns.literal : Text -> Pattern Text
  472. Text.patterns.notCharIn : [Char] -> Pattern Text
  473. Text.patterns.notCharRange : Char -> Char -> Pattern Text
  474. Text.patterns.punctuation : Pattern Text
  475. Text.patterns.space : Pattern Text
  476. Text.repeat : Nat -> Text -> Text
  477. Text.reverse : Text -> Text
  478. Text.size : Text -> Nat
  479. Text.take : Nat -> Text -> Text
  480. Text.toCharList : Text -> [Char]
  481. Text.toLowercase : Text -> Text
  482. Text.toUppercase : Text -> Text
  483. Text.toUtf8 : Text -> Bytes
  484. Text.uncons : Text -> Optional (Char, Text)
  485. Text.unsnoc : Text -> Optional (Text, Char)
  486. ThreadId.toText : ThreadId -> Text
  487. todo : a -> b
  488. structural type Tuple a b
  489. Tuple.Cons : a -> b -> Tuple a b
  490. structural type Unit
  491. Unit.Unit : ()
  492. Universal.< : a -> a -> Boolean
  493. Universal.<= : a -> a -> Boolean
  494. Universal.== : a -> a -> Boolean
  495. Universal.> : a -> a -> Boolean
  496. Universal.>= : a -> a -> Boolean
  497. Universal.compare : a -> a -> Int
  498. Universal.murmurHash : a -> Nat
  499. unsafe.coerceAbilities : (a ->{e1} b) -> a ->{e2} b
  500. builtin type Value
  501. Value.dependencies : Value -> [Term]
  502. Value.deserialize : Bytes -> Either Text Value
  503. Value.load : Value ->{IO} Either [Term] a
  504. Value.serialize : Value -> Bytes
  505. Value.value : a -> Value
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  structural type Either a b
    2.  structural ability Exception
    3.  builtin type Float
    4.  Either.Left     : a -> Either a b
    5.  Doc.Link        : Link -> Doc
    6.  Either.Right    : b -> Either a b
    7.  Doc.Signature   : Term -> Doc
    8.  Doc.Source      : Link -> Doc
    9.  Exception.raise : Failure ->{Exception} x
    10. Float.*         : Float -> Float -> Float
    11. Float.+         : Float -> Float -> Float
  
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

  1.  Doc.Link : Link -> Doc
  2.  Doc.Signature : Term -> Doc
  3.  Doc.Source : Link -> Doc
  4.  structural type Either a b
  5.  Either.Left : a -> Either a b
  6.  Either.Right : b -> Either a b
  7.  structural ability Exception
  8.  Exception.raise : Failure ->{Exception} x
  9.  builtin type Float
  10. Float.* : Float -> Float -> Float
  11. Float.+ : Float -> Float -> Float
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
