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

  1.   builtin type Boolean
  2.   Boolean.not : Boolean -> Boolean
  3.   builtin type Bytes
  4.   Bytes.++ : Bytes -> Bytes -> Bytes
  5.   Bytes.at : Nat -> Bytes -> Optional Nat
  6.   Bytes.drop : Nat -> Bytes -> Bytes
  7.   Bytes.empty : Bytes
  8.   Bytes.flatten : Bytes -> Bytes
  9.   Bytes.fromBase16 : Bytes -> Either Text Bytes
  10.  Bytes.fromBase32 : Bytes -> Either Text Bytes
  11.  Bytes.fromBase64 : Bytes -> Either Text Bytes
  12.  Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
  13.  Bytes.fromList : [Nat] -> Bytes
  14.  Bytes.size : Bytes -> Nat
  15.  Bytes.take : Nat -> Bytes -> Bytes
  16.  Bytes.toBase16 : Bytes -> Bytes
  17.  Bytes.toBase32 : Bytes -> Bytes
  18.  Bytes.toBase64 : Bytes -> Bytes
  19.  Bytes.toBase64UrlUnpadded : Bytes -> Bytes
  20.  Bytes.toList : Bytes -> [Nat]
  21.  builtin type Char
  22.  Char.fromNat : Nat -> Char
  23.  Char.toNat : Char -> Nat
<<<<<<< HEAD
  24.  Debug.watch : Text -> a -> a
  25.  unique type Doc
  26.  Doc.Blob : Text -> Doc
  27.  Doc.Evaluate : Term -> Doc
  28.  Doc.Join : [Doc] -> Doc
  29.  Doc.Link : Link -> Doc
  30.  Doc.Signature : Term -> Doc
  31.  Doc.Source : Link -> Doc
  32.  type Either a b
  33.  Either.Left : a -> Either a b
  34.  Either.Right : b -> Either a b
  35.  builtin type Float
  36.  Float.* : Float -> Float -> Float
  37.  Float.+ : Float -> Float -> Float
  38.  Float.- : Float -> Float -> Float
  39.  Float./ : Float -> Float -> Float
  40.  Float.abs : Float -> Float
  41.  Float.acos : Float -> Float
  42.  Float.acosh : Float -> Float
  43.  Float.asin : Float -> Float
  44.  Float.asinh : Float -> Float
  45.  Float.atan : Float -> Float
  46.  Float.atan2 : Float -> Float -> Float
  47.  Float.atanh : Float -> Float
  48.  Float.ceiling : Float -> Int
  49.  Float.cos : Float -> Float
  50.  Float.cosh : Float -> Float
  51.  Float.eq : Float -> Float -> Boolean
  52.  Float.exp : Float -> Float
  53.  Float.floor : Float -> Int
  54.  Float.fromText : Text -> Optional Float
  55.  Float.gt : Float -> Float -> Boolean
  56.  Float.gteq : Float -> Float -> Boolean
  57.  Float.log : Float -> Float
  58.  Float.logBase : Float -> Float -> Float
  59.  Float.lt : Float -> Float -> Boolean
  60.  Float.lteq : Float -> Float -> Boolean
  61.  Float.max : Float -> Float -> Float
  62.  Float.min : Float -> Float -> Float
  63.  Float.pow : Float -> Float -> Float
  64.  Float.round : Float -> Int
  65.  Float.sin : Float -> Float
  66.  Float.sinh : Float -> Float
  67.  Float.sqrt : Float -> Float
  68.  Float.tan : Float -> Float
  69.  Float.tanh : Float -> Float
  70.  Float.toText : Float -> Text
  71.  Float.truncate : Float -> Int
  72.  builtin type Int
  73.  Int.* : Int -> Int -> Int
  74.  Int.+ : Int -> Int -> Int
  75.  Int.- : Int -> Int -> Int
  76.  Int./ : Int -> Int -> Int
  77.  Int.and : Int -> Int -> Int
  78.  Int.complement : Int -> Int
  79.  Int.eq : Int -> Int -> Boolean
  80.  Int.fromText : Text -> Optional Int
  81.  Int.gt : Int -> Int -> Boolean
  82.  Int.gteq : Int -> Int -> Boolean
  83.  Int.increment : Int -> Int
  84.  Int.isEven : Int -> Boolean
  85.  Int.isOdd : Int -> Boolean
  86.  Int.leadingZeros : Int -> Nat
  87.  Int.lt : Int -> Int -> Boolean
  88.  Int.lteq : Int -> Int -> Boolean
  89.  Int.mod : Int -> Int -> Int
  90.  Int.negate : Int -> Int
  91.  Int.or : Int -> Int -> Int
  92.  Int.popCount : Int -> Nat
  93.  Int.pow : Int -> Nat -> Int
  94.  Int.shiftLeft : Int -> Nat -> Int
  95.  Int.shiftRight : Int -> Nat -> Int
  96.  Int.signum : Int -> Int
  97.  Int.toFloat : Int -> Float
  98.  Int.toText : Int -> Text
  99.  Int.trailingZeros : Int -> Nat
  100. Int.truncate0 : Int -> Nat
  101. Int.xor : Int -> Int -> Int
  102. unique type Link
  103. builtin type Link.Term
  104. Link.Term : Term -> Link
  105. builtin type Link.Type
  106. Link.Type : Type -> Link
  107. builtin type List
  108. List.++ : [a] -> [a] -> [a]
  109. List.+: : a -> [a] -> [a]
  110. List.:+ : [a] -> a -> [a]
  111. List.at : Nat -> [a] -> Optional a
  112. List.cons : a -> [a] -> [a]
  113. List.drop : Nat -> [a] -> [a]
  114. List.empty : [a]
  115. List.size : [a] -> Nat
  116. List.snoc : [a] -> a -> [a]
  117. List.take : Nat -> [a] -> [a]
  118. builtin type Nat
  119. Nat.* : Nat -> Nat -> Nat
  120. Nat.+ : Nat -> Nat -> Nat
  121. Nat./ : Nat -> Nat -> Nat
  122. Nat.and : Nat -> Nat -> Nat
  123. Nat.complement : Nat -> Nat
  124. Nat.drop : Nat -> Nat -> Nat
  125. Nat.eq : Nat -> Nat -> Boolean
  126. Nat.fromText : Text -> Optional Nat
  127. Nat.gt : Nat -> Nat -> Boolean
  128. Nat.gteq : Nat -> Nat -> Boolean
  129. Nat.increment : Nat -> Nat
  130. Nat.isEven : Nat -> Boolean
  131. Nat.isOdd : Nat -> Boolean
  132. Nat.leadingZeros : Nat -> Nat
  133. Nat.lt : Nat -> Nat -> Boolean
  134. Nat.lteq : Nat -> Nat -> Boolean
  135. Nat.mod : Nat -> Nat -> Nat
  136. Nat.or : Nat -> Nat -> Nat
  137. Nat.popCount : Nat -> Nat
  138. Nat.pow : Nat -> Nat -> Nat
  139. Nat.shiftLeft : Nat -> Nat -> Nat
  140. Nat.shiftRight : Nat -> Nat -> Nat
  141. Nat.sub : Nat -> Nat -> Int
  142. Nat.toFloat : Nat -> Float
  143. Nat.toInt : Nat -> Int
  144. Nat.toText : Nat -> Text
  145. Nat.trailingZeros : Nat -> Nat
  146. Nat.xor : Nat -> Nat -> Nat
  147. type Optional a
  148. Optional.None : Optional a
  149. Optional.Some : a -> Optional a
  150. builtin type Request
  151. type SeqView a b
  152. SeqView.VElem : a -> b -> SeqView a b
  153. SeqView.VEmpty : SeqView a b
  154. unique type Test.Result
  155. Test.Result.Fail : Text -> Result
  156. Test.Result.Ok : Text -> Result
  157. builtin type Text
  158. Text.!= : Text -> Text -> Boolean
  159. Text.++ : Text -> Text -> Text
  160. Text.drop : Nat -> Text -> Text
  161. Text.empty : Text
  162. Text.eq : Text -> Text -> Boolean
  163. Text.fromCharList : [Char] -> Text
  164. Text.fromUtf8 : Bytes -> Either Failure Text
  165. Text.gt : Text -> Text -> Boolean
  166. Text.gteq : Text -> Text -> Boolean
  167. Text.lt : Text -> Text -> Boolean
  168. Text.lteq : Text -> Text -> Boolean
  169. Text.size : Text -> Nat
  170. Text.take : Nat -> Text -> Text
  171. Text.toCharList : Text -> [Char]
  172. Text.toUtf8 : Text -> Bytes
  173. Text.uncons : Text -> Optional (Char, Text)
  174. Text.unsnoc : Text -> Optional (Text, Char)
  175. type Tuple a b
  176. Tuple.Cons : a -> b -> Tuple a b
  177. type Unit
  178. Unit.Unit : ()
  179. Universal.< : a -> a -> Boolean
  180. Universal.<= : a -> a -> Boolean
  181. Universal.== : a -> a -> Boolean
  182. Universal.> : a -> a -> Boolean
  183. Universal.>= : a -> a -> Boolean
  184. Universal.compare : a -> a -> Int
  185. bug : a -> b
  186. builtin type crypto.HashAlgorithm
  187. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  188. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  189. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  190. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  191. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  192. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  193. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  194. crypto.hash : HashAlgorithm -> a -> Bytes
  195. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  196. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  197. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  198. unique type io2.BufferMode
  199. io2.BufferMode.BlockBuffering : BufferMode
  200. io2.BufferMode.LineBuffering : BufferMode
  201. io2.BufferMode.NoBuffering : BufferMode
  202. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  203. unique type io2.Failure
  204. io2.Failure.Failure : Type -> Text -> Failure
  205. unique type io2.FileMode
  206. io2.FileMode.Append : FileMode
  207. io2.FileMode.Read : FileMode
  208. io2.FileMode.ReadWrite : FileMode
  209. io2.FileMode.Write : FileMode
  210. builtin type io2.Handle
  211. builtin type io2.IO
  212. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  213. io2.IO.closeFile : Handle ->{IO} Either Failure ()
  214. io2.IO.closeSocket : Socket ->{IO} Either Failure ()
  215. io2.IO.createDirectory : Text ->{IO} Either Failure ()
  216. io2.IO.createTempDirectory : Text
                                    ->{IO} Either Failure Text
  217. io2.IO.delay : Nat ->{IO} Either Failure ()
  218. io2.IO.fileExists : Text ->{IO} Either Failure Boolean
  219. io2.IO.forkComp : '{IO} Either Failure a ->{IO} ThreadId
  220. io2.IO.getBuffering : Handle
                             ->{IO} Either Failure BufferMode
  221. io2.IO.getBytes : Handle
                         -> Nat
                         ->{IO} Either Failure Bytes
  222. io2.IO.getCurrentDirectory : '{IO} Either Failure Text
  223. io2.IO.getFileSize : Text ->{IO} Either Failure Nat
  224. io2.IO.getFileTimestamp : Text ->{IO} Either Failure Nat
  225. io2.IO.getTempDirectory : '{IO} Either Failure Text
  226. io2.IO.handlePosition : Handle ->{IO} Either Failure Int
  227. io2.IO.isDirectory : Text ->{IO} Either Failure Boolean
  228. io2.IO.isFileEOF : Handle ->{IO} Either Failure Boolean
  229. io2.IO.isFileOpen : Handle ->{IO} Either Failure Boolean
  230. io2.IO.isSeekable : Handle ->{IO} Either Failure Boolean
  231. io2.IO.kill : ThreadId ->{IO} Either Failure ()
  232. io2.IO.listen : Socket ->{IO} Either Failure ()
  233. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either Failure Handle
  234. io2.IO.putBytes : Handle
                         -> Bytes
                         ->{IO} Either Failure ()
  235. io2.IO.removeDirectory : Text ->{IO} Either Failure ()
  236. io2.IO.removeFile : Text ->{IO} Either Failure ()
  237. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either Failure ()
  238. io2.IO.renameFile : Text -> Text ->{IO} Either Failure ()
  239. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either Failure ()
  240. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either Failure Socket
  241. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either Failure ()
  242. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either Failure ()
  243. io2.IO.socketAccept : Socket ->{IO} Either Failure Socket
  244. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either Failure Bytes
  245. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either Failure ()
  246. io2.IO.stdHandle : StdHandle -> Handle
  247. io2.IO.systemTime : '{IO} Either Failure Nat
  248. unique type io2.IOError
  249. io2.IOError.AlreadyExists : IOError
  250. io2.IOError.EOF : IOError
  251. io2.IOError.IllegalOperation : IOError
  252. io2.IOError.NoSuchThing : IOError
  253. io2.IOError.PermissionDenied : IOError
  254. io2.IOError.ResourceBusy : IOError
  255. io2.IOError.ResourceExhausted : IOError
  256. io2.IOError.UserError : IOError
  257. builtin type io2.MVar
  258. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  259. io2.MVar.new : a ->{IO} MVar a
  260. io2.MVar.newEmpty : '{IO} MVar a
  261. io2.MVar.put : MVar a -> a ->{IO} Either Failure ()
  262. io2.MVar.read : MVar a ->{IO} Either Failure a
  263. io2.MVar.swap : MVar a -> a ->{IO} Either Failure a
  264. io2.MVar.take : MVar a ->{IO} Either Failure a
  265. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  266. io2.MVar.tryRead : MVar a ->{IO} Optional a
  267. io2.MVar.tryTake : MVar a ->{IO} Optional a
  268. unique type io2.SeekMode
  269. io2.SeekMode.AbsoluteSeek : SeekMode
  270. io2.SeekMode.RelativeSeek : SeekMode
  271. io2.SeekMode.SeekFromEnd : SeekMode
  272. builtin type io2.Socket
  273. unique type io2.StdHandle
  274. io2.StdHandle.StdErr : StdHandle
  275. io2.StdHandle.StdIn : StdHandle
  276. io2.StdHandle.StdOut : StdHandle
  277. builtin type io2.ThreadId
  278. builtin type io2.Tls
  279. builtin type io2.Tls.ClientConfig
  280. io2.Tls.Config.defaultClient : Text
                                      -> Bytes
                                      -> ClientConfig
  281. io2.Tls.Config.defaultServer : ServerConfig
  282. builtin type io2.Tls.ServerConfig
  283. io2.Tls.handshake : Tls ->{IO} Either Failure ()
  284. io2.Tls.newClient : ClientConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  285. io2.Tls.newServer : ServerConfig
                           -> Socket
                           ->{IO} Either Failure Tls
  286. io2.Tls.receive : Tls ->{IO} Either Failure Bytes
  287. io2.Tls.send : Tls -> Bytes ->{IO} Either Failure ()
  288. io2.Tls.terminate : Tls ->{IO} Either Failure ()
  289. unique type io2.TlsFailure
  290. todo : a -> b
=======
  24.  builtin type Code
  25.  Code.cache_ : [(Term, Code)] ->{IO} [Term]
  26.  Code.dependencies : Code -> [Term]
  27.  Code.deserialize : Bytes -> Either Text Code
  28.  Code.isMissing : Term ->{IO} Boolean
  29.  Code.lookup : Term ->{IO} Optional Code
  30.  Code.serialize : Code -> Bytes
  31.  Debug.watch : Text -> a -> a
  32.  unique type Doc
  33.  Doc.Blob : Text -> Doc
  34.  Doc.Evaluate : Term -> Doc
  35.  Doc.Join : [Doc] -> Doc
  36.  Doc.Link : Link -> Doc
  37.  Doc.Signature : Term -> Doc
  38.  Doc.Source : Link -> Doc
  39.  type Either a b
  40.  Either.Left : a -> Either a b
  41.  Either.Right : b -> Either a b
  42.  builtin type Float
  43.  Float.* : Float -> Float -> Float
  44.  Float.+ : Float -> Float -> Float
  45.  Float.- : Float -> Float -> Float
  46.  Float./ : Float -> Float -> Float
  47.  Float.abs : Float -> Float
  48.  Float.acos : Float -> Float
  49.  Float.acosh : Float -> Float
  50.  Float.asin : Float -> Float
  51.  Float.asinh : Float -> Float
  52.  Float.atan : Float -> Float
  53.  Float.atan2 : Float -> Float -> Float
  54.  Float.atanh : Float -> Float
  55.  Float.ceiling : Float -> Int
  56.  Float.cos : Float -> Float
  57.  Float.cosh : Float -> Float
  58.  Float.eq : Float -> Float -> Boolean
  59.  Float.exp : Float -> Float
  60.  Float.floor : Float -> Int
  61.  Float.fromText : Text -> Optional Float
  62.  Float.gt : Float -> Float -> Boolean
  63.  Float.gteq : Float -> Float -> Boolean
  64.  Float.log : Float -> Float
  65.  Float.logBase : Float -> Float -> Float
  66.  Float.lt : Float -> Float -> Boolean
  67.  Float.lteq : Float -> Float -> Boolean
  68.  Float.max : Float -> Float -> Float
  69.  Float.min : Float -> Float -> Float
  70.  Float.pow : Float -> Float -> Float
  71.  Float.round : Float -> Int
  72.  Float.sin : Float -> Float
  73.  Float.sinh : Float -> Float
  74.  Float.sqrt : Float -> Float
  75.  Float.tan : Float -> Float
  76.  Float.tanh : Float -> Float
  77.  Float.toText : Float -> Text
  78.  Float.truncate : Float -> Int
  79.  builtin type Int
  80.  Int.* : Int -> Int -> Int
  81.  Int.+ : Int -> Int -> Int
  82.  Int.- : Int -> Int -> Int
  83.  Int./ : Int -> Int -> Int
  84.  Int.and : Int -> Int -> Int
  85.  Int.complement : Int -> Int
  86.  Int.eq : Int -> Int -> Boolean
  87.  Int.fromText : Text -> Optional Int
  88.  Int.gt : Int -> Int -> Boolean
  89.  Int.gteq : Int -> Int -> Boolean
  90.  Int.increment : Int -> Int
  91.  Int.isEven : Int -> Boolean
  92.  Int.isOdd : Int -> Boolean
  93.  Int.leadingZeros : Int -> Nat
  94.  Int.lt : Int -> Int -> Boolean
  95.  Int.lteq : Int -> Int -> Boolean
  96.  Int.mod : Int -> Int -> Int
  97.  Int.negate : Int -> Int
  98.  Int.or : Int -> Int -> Int
  99.  Int.popCount : Int -> Nat
  100. Int.pow : Int -> Nat -> Int
  101. Int.shiftLeft : Int -> Nat -> Int
  102. Int.shiftRight : Int -> Nat -> Int
  103. Int.signum : Int -> Int
  104. Int.toFloat : Int -> Float
  105. Int.toText : Int -> Text
  106. Int.trailingZeros : Int -> Nat
  107. Int.truncate0 : Int -> Nat
  108. Int.xor : Int -> Int -> Int
  109. unique type Link
  110. builtin type Link.Term
  111. Link.Term : Term -> Link
  112. builtin type Link.Type
  113. Link.Type : Type -> Link
  114. builtin type List
  115. List.++ : [a] -> [a] -> [a]
  116. List.+: : a -> [a] -> [a]
  117. List.:+ : [a] -> a -> [a]
  118. List.at : Nat -> [a] -> Optional a
  119. List.cons : a -> [a] -> [a]
  120. List.drop : Nat -> [a] -> [a]
  121. List.empty : [a]
  122. List.size : [a] -> Nat
  123. List.snoc : [a] -> a -> [a]
  124. List.take : Nat -> [a] -> [a]
  125. builtin type Nat
  126. Nat.* : Nat -> Nat -> Nat
  127. Nat.+ : Nat -> Nat -> Nat
  128. Nat./ : Nat -> Nat -> Nat
  129. Nat.and : Nat -> Nat -> Nat
  130. Nat.complement : Nat -> Nat
  131. Nat.drop : Nat -> Nat -> Nat
  132. Nat.eq : Nat -> Nat -> Boolean
  133. Nat.fromText : Text -> Optional Nat
  134. Nat.gt : Nat -> Nat -> Boolean
  135. Nat.gteq : Nat -> Nat -> Boolean
  136. Nat.increment : Nat -> Nat
  137. Nat.isEven : Nat -> Boolean
  138. Nat.isOdd : Nat -> Boolean
  139. Nat.leadingZeros : Nat -> Nat
  140. Nat.lt : Nat -> Nat -> Boolean
  141. Nat.lteq : Nat -> Nat -> Boolean
  142. Nat.mod : Nat -> Nat -> Nat
  143. Nat.or : Nat -> Nat -> Nat
  144. Nat.popCount : Nat -> Nat
  145. Nat.pow : Nat -> Nat -> Nat
  146. Nat.shiftLeft : Nat -> Nat -> Nat
  147. Nat.shiftRight : Nat -> Nat -> Nat
  148. Nat.sub : Nat -> Nat -> Int
  149. Nat.toFloat : Nat -> Float
  150. Nat.toInt : Nat -> Int
  151. Nat.toText : Nat -> Text
  152. Nat.trailingZeros : Nat -> Nat
  153. Nat.xor : Nat -> Nat -> Nat
  154. type Optional a
  155. Optional.None : Optional a
  156. Optional.Some : a -> Optional a
  157. builtin type Request
  158. type SeqView a b
  159. SeqView.VElem : a -> b -> SeqView a b
  160. SeqView.VEmpty : SeqView a b
  161. unique type Test.Result
  162. Test.Result.Fail : Text -> Result
  163. Test.Result.Ok : Text -> Result
  164. builtin type Text
  165. Text.!= : Text -> Text -> Boolean
  166. Text.++ : Text -> Text -> Text
  167. Text.drop : Nat -> Text -> Text
  168. Text.empty : Text
  169. Text.eq : Text -> Text -> Boolean
  170. Text.fromCharList : [Char] -> Text
  171. Text.fromUtf8 : Bytes -> Either Text Text
  172. Text.gt : Text -> Text -> Boolean
  173. Text.gteq : Text -> Text -> Boolean
  174. Text.lt : Text -> Text -> Boolean
  175. Text.lteq : Text -> Text -> Boolean
  176. Text.size : Text -> Nat
  177. Text.take : Nat -> Text -> Text
  178. Text.toCharList : Text -> [Char]
  179. Text.toUtf8 : Text -> Bytes
  180. Text.uncons : Text -> Optional (Char, Text)
  181. Text.unsnoc : Text -> Optional (Text, Char)
  182. type Tuple a b
  183. Tuple.Cons : a -> b -> Tuple a b
  184. type Unit
  185. Unit.Unit : ()
  186. Universal.< : a -> a -> Boolean
  187. Universal.<= : a -> a -> Boolean
  188. Universal.== : a -> a -> Boolean
  189. Universal.> : a -> a -> Boolean
  190. Universal.>= : a -> a -> Boolean
  191. Universal.compare : a -> a -> Int
  192. builtin type Value
  193. Value.dependencies : Value -> [Term]
  194. Value.deserialize : Bytes -> Either Text Value
  195. Value.load : Value ->{IO} Either [Term] a
  196. Value.serialize : Value -> Bytes
  197. Value.value : a -> Value
  198. bug : a -> b
  199. builtin type crypto.HashAlgorithm
  200. crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
  201. crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
  202. crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
  203. crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
  204. crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
  205. crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
  206. crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
  207. crypto.hash : HashAlgorithm -> a -> Bytes
  208. crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
  209. crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
  210. crypto.hmacBytes : HashAlgorithm
                          -> Bytes
                          -> Bytes
                          -> Bytes
  211. unique type io2.BufferMode
  212. io2.BufferMode.BlockBuffering : BufferMode
  213. io2.BufferMode.LineBuffering : BufferMode
  214. io2.BufferMode.NoBuffering : BufferMode
  215. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  216. unique type io2.FileMode
  217. io2.FileMode.Append : FileMode
  218. io2.FileMode.Read : FileMode
  219. io2.FileMode.ReadWrite : FileMode
  220. io2.FileMode.Write : FileMode
  221. builtin type io2.Handle
  222. builtin type io2.IO
  223. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  224. io2.IO.closeFile : Handle ->{IO} Either IOError ()
  225. io2.IO.closeSocket : Socket ->{IO} Either IOError ()
  226. io2.IO.createDirectory : Text ->{IO} Either IOError ()
  227. io2.IO.delay : Nat ->{IO} Either IOError ()
  228. io2.IO.fileExists : Text ->{IO} Either IOError Boolean
  229. io2.IO.forkComp : '{IO} Either IOError a ->{IO} ThreadId
  230. io2.IO.getBuffering : Handle
                             ->{IO} Either IOError BufferMode
  231. io2.IO.getBytes : Handle
                         -> Nat
                         ->{IO} Either IOError Text
  232. io2.IO.getCurrentDirectory : '{IO} Either IOError Text
  233. io2.IO.getFileSize : Text ->{IO} Either IOError Nat
  234. io2.IO.getFileTimestamp : Text ->{IO} Either IOError Nat
  235. io2.IO.getLine : Handle ->{IO} Either IOError Text
  236. io2.IO.getTempDirectory : '{IO} Either IOError Text
  237. io2.IO.handlePosition : Handle ->{IO} Either IOError Int
  238. io2.IO.isDirectory : Text ->{IO} Either IOError Boolean
  239. io2.IO.isFileEOF : Handle ->{IO} Either IOError Boolean
  240. io2.IO.isFileOpen : Handle ->{IO} Either IOError Boolean
  241. io2.IO.isSeekable : Handle ->{IO} Either IOError Boolean
  242. io2.IO.kill : ThreadId ->{IO} Either IOError ()
  243. io2.IO.listen : Socket ->{IO} Either IOError ()
  244. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either IOError Handle
  245. io2.IO.putBytes : Handle
                         -> Bytes
                         ->{IO} Either IOError ()
  246. io2.IO.removeDirectory : Text ->{IO} Either IOError ()
  247. io2.IO.removeFile : Text ->{IO} Either IOError ()
  248. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either IOError ()
  249. io2.IO.renameFile : Text -> Text ->{IO} Either IOError ()
  250. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either IOError ()
  251. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  252. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either IOError ()
  253. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either IOError ()
  254. io2.IO.socketAccept : Socket ->{IO} Either IOError Socket
  255. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either IOError Bytes
  256. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either IOError ()
  257. io2.IO.stdHandle : StdHandle -> Handle
  258. io2.IO.systemTime : '{IO} Either IOError Nat
  259. unique type io2.IOError
  260. io2.IOError.AlreadyExists : IOError
  261. io2.IOError.EOF : IOError
  262. io2.IOError.IllegalOperation : IOError
  263. io2.IOError.NoSuchThing : IOError
  264. io2.IOError.PermissionDenied : IOError
  265. io2.IOError.ResourceBusy : IOError
  266. io2.IOError.ResourceExhausted : IOError
  267. io2.IOError.UserError : IOError
  268. builtin type io2.MVar
  269. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  270. io2.MVar.new : a ->{IO} MVar a
  271. io2.MVar.newEmpty : {IO} (MVar a)
  272. io2.MVar.put : MVar a -> a ->{IO} Either IOError ()
  273. io2.MVar.read : MVar a ->{IO} Either IOError a
  274. io2.MVar.swap : MVar a -> a ->{IO} Either IOError a
  275. io2.MVar.take : MVar a ->{IO} Either IOError a
  276. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  277. io2.MVar.tryRead : MVar a ->{IO} Optional a
  278. io2.MVar.tryTake : MVar a ->{IO} Optional a
  279. unique type io2.SeekMode
  280. io2.SeekMode.AbsoluteSeek : SeekMode
  281. io2.SeekMode.RelativeSeek : SeekMode
  282. io2.SeekMode.SeekFromEnd : SeekMode
  283. builtin type io2.Socket
  284. unique type io2.StdHandle
  285. io2.StdHandle.StdErr : StdHandle
  286. io2.StdHandle.StdIn : StdHandle
  287. io2.StdHandle.StdOut : StdHandle
  288. builtin type io2.ThreadId
  289. todo : a -> b
>>>>>>> trunk
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.lt         : Int -> Int -> Boolean
    2.  Int.lteq       : Int -> Int -> Boolean
    3.  Int.mod        : Int -> Int -> Int
    4.  Int.negate     : Int -> Int
    5.  Int.or         : Int -> Int -> Int
    6.  Int.popCount   : Int -> Nat
    7.  Int.pow        : Int -> Nat -> Int
    8.  Int.shiftLeft  : Int -> Nat -> Int
    9.  Int.shiftRight : Int -> Nat -> Int
    10. Int.signum     : Int -> Int
    11. Int.toFloat    : Int -> Float
  
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

  1.  Int.lt : Int -> Int -> Boolean
  2.  Int.lteq : Int -> Int -> Boolean
  3.  Int.mod : Int -> Int -> Int
  4.  Int.negate : Int -> Int
  5.  Int.or : Int -> Int -> Int
  6.  Int.popCount : Int -> Nat
  7.  Int.pow : Int -> Nat -> Int
  8.  Int.shiftLeft : Int -> Nat -> Int
  9.  Int.shiftRight : Int -> Nat -> Int
  10. Int.signum : Int -> Int
  11. Int.toFloat : Int -> Float
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
