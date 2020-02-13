The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be copied over are the names relative to the current namespace:

```
.> help alias.many
  alias.many (or copy)
  `alias.many foo.foo bar.bar quux` creates aliases
  `quux.foo.foo` and `quux.bar.bar`.

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
  9.   Bytes.fromList : [Nat] -> Bytes
  10.  Bytes.size : Bytes -> Nat
  11.  Bytes.take : Nat -> Bytes -> Bytes
  12.  Bytes.toList : Bytes -> [Nat]
  13.  builtin type Char
  14.  Char.fromNat : Nat -> Char
  15.  Char.toNat : Char -> Nat
  16.  Debug.watch : Text -> a -> a
  17.  unique type Doc
  18.  Doc.++ : Doc -> Doc -> Doc
  19.  Doc.Blob : Text -> Doc
  20.  Doc.Evaluate : Term -> Doc
  21.  Doc.Join : [Doc] -> Doc
  22.  Doc.Link : Link -> Doc
  23.  Doc.Signature : Term -> Doc
  24.  Doc.Source : Link -> Doc
  25.  type Either a b
  26.  Either.Left : a -> Either a b
  27.  Either.Right : b -> Either a b
  28.  builtin type Float
  29.  Float.* : Float -> Float -> Float
  30.  Float.+ : Float -> Float -> Float
  31.  Float.- : Float -> Float -> Float
  32.  Float./ : Float -> Float -> Float
  33.  Float.abs : Float -> Float
  34.  Float.acos : Float -> Float
  35.  Float.acosh : Float -> Float
  36.  Float.asin : Float -> Float
  37.  Float.asinh : Float -> Float
  38.  Float.atan : Float -> Float
  39.  Float.atan2 : Float -> Float -> Float
  40.  Float.atanh : Float -> Float
  41.  Float.ceiling : Float -> Int
  42.  Float.cos : Float -> Float
  43.  Float.cosh : Float -> Float
  44.  Float.eq : Float -> Float -> Boolean
  45.  Float.exp : Float -> Float
  46.  Float.floor : Float -> Int
  47.  Float.fromText : Text -> Optional Float
  48.  Float.gt : Float -> Float -> Boolean
  49.  Float.gteq : Float -> Float -> Boolean
  50.  Float.log : Float -> Float
  51.  Float.logBase : Float -> Float -> Float
  52.  Float.lt : Float -> Float -> Boolean
  53.  Float.lteq : Float -> Float -> Boolean
  54.  Float.max : Float -> Float -> Float
  55.  Float.min : Float -> Float -> Float
  56.  Float.pow : Float -> Float -> Float
  57.  Float.round : Float -> Int
  58.  Float.sin : Float -> Float
  59.  Float.sinh : Float -> Float
  60.  Float.sqrt : Float -> Float
  61.  Float.tan : Float -> Float
  62.  Float.tanh : Float -> Float
  63.  Float.toText : Float -> Text
  64.  Float.truncate : Float -> Int
  65.  builtin type Int
  66.  Int.* : Int -> Int -> Int
  67.  Int.+ : Int -> Int -> Int
  68.  Int.- : Int -> Int -> Int
  69.  Int./ : Int -> Int -> Int
  70.  Int.eq : Int -> Int -> Boolean
  71.  Int.fromText : Text -> Optional Int
  72.  Int.gt : Int -> Int -> Boolean
  73.  Int.gteq : Int -> Int -> Boolean
  74.  Int.increment : Int -> Int
  75.  Int.isEven : Int -> Boolean
  76.  Int.isOdd : Int -> Boolean
  77.  Int.lt : Int -> Int -> Boolean
  78.  Int.lteq : Int -> Int -> Boolean
  79.  Int.mod : Int -> Int -> Int
  80.  Int.negate : Int -> Int
  81.  Int.signum : Int -> Int
  82.  Int.toFloat : Int -> Float
  83.  Int.toText : Int -> Text
  84.  Int.truncate0 : Int -> Nat
  85.  unique type IsPropagated
  86.  IsPropagated.IsPropagated : IsPropagated
  87.  unique type IsTest
  88.  IsTest.IsTest : IsTest
  89.  unique type Link
  90.  builtin type Link.Term
  91.  Link.Term : Term -> Link
  92.  builtin type Link.Type
  93.  Link.Type : Type -> Link
  94.  builtin type List
  95.  List.++ : [a] -> [a] -> [a]
  96.  List.+: : a -> [a] -> [a]
  97.  List.:+ : [a] -> a -> [a]
  98.  List.at : Nat -> [a] -> Optional a
  99.  List.cons : a -> [a] -> [a]
  100. List.drop : Nat -> [a] -> [a]
  101. List.empty : [a]
  102. List.size : [a] -> Nat
  103. List.snoc : [a] -> a -> [a]
  104. List.take : Nat -> [a] -> [a]
  105. builtin type Nat
  106. Nat.* : Nat -> Nat -> Nat
  107. Nat.+ : Nat -> Nat -> Nat
  108. Nat./ : Nat -> Nat -> Nat
  109. Nat.drop : Nat -> Nat -> Nat
  110. Nat.eq : Nat -> Nat -> Boolean
  111. Nat.fromText : Text -> Optional Nat
  112. Nat.gt : Nat -> Nat -> Boolean
  113. Nat.gteq : Nat -> Nat -> Boolean
  114. Nat.increment : Nat -> Nat
  115. Nat.isEven : Nat -> Boolean
  116. Nat.isOdd : Nat -> Boolean
  117. Nat.lt : Nat -> Nat -> Boolean
  118. Nat.lteq : Nat -> Nat -> Boolean
  119. Nat.mod : Nat -> Nat -> Nat
  120. Nat.sub : Nat -> Nat -> Int
  121. Nat.toFloat : Nat -> Float
  122. Nat.toInt : Nat -> Int
  123. Nat.toText : Nat -> Text
  124. type Optional a
  125. Optional.None : Optional a
  126. Optional.Some : a -> Optional a
  127. builtin type Request
  128. unique type Test.Result
  129. Test.Result.Fail : Text -> Result
  130. Test.Result.Ok : Text -> Result
  131. builtin type Text
  132. Text.!= : Text -> Text -> Boolean
  133. Text.++ : Text -> Text -> Text
  134. Text.drop : Nat -> Text -> Text
  135. Text.empty : Text
  136. Text.eq : Text -> Text -> Boolean
  137. Text.fromCharList : [Char] -> Text
  138. Text.gt : Text -> Text -> Boolean
  139. Text.gteq : Text -> Text -> Boolean
  140. Text.lt : Text -> Text -> Boolean
  141. Text.lteq : Text -> Text -> Boolean
  142. Text.size : Text -> Nat
  143. Text.take : Nat -> Text -> Text
  144. Text.toCharList : Text -> [Char]
  145. Text.uncons : Text -> Optional (Char, Text)
  146. Text.unsnoc : Text -> Optional (Text, Char)
  147. type Tuple a b
  148. Tuple.Cons : a -> b -> Tuple a b
  149. type Unit
  150. Unit.Unit : ()
  151. Universal.< : a -> a -> Boolean
  152. Universal.<= : a -> a -> Boolean
  153. Universal.== : a -> a -> Boolean
  154. Universal.> : a -> a -> Boolean
  155. Universal.>= : a -> a -> Boolean
  156. Universal.compare : a -> a -> Int
  157. bug : a -> b
  158. unique type io.BufferMode
  159. io.BufferMode.Block : Optional Nat -> BufferMode
  160. io.BufferMode.Line : BufferMode
  161. unique type io.EpochTime
  162. io.EpochTime.EpochTime : Nat -> EpochTime
  163. type io.Error
  164. io.Error.Error : ErrorType -> Text -> Error
  165. unique type io.ErrorDescription
  166. io.ErrorDescription.ErrorDescription : Text
                                              -> ErrorDescription
  167. unique type io.ErrorLocation
  168. io.ErrorLocation.ErrorLocation : Text -> ErrorLocation
  169. unique type io.ErrorType
  170. io.ErrorType.AlreadyExists : ErrorType
  171. io.ErrorType.EOF : ErrorType
  172. io.ErrorType.IllegalOperation : ErrorType
  173. io.ErrorType.NoSuchThing : ErrorType
  174. io.ErrorType.PermissionDenied : ErrorType
  175. io.ErrorType.ResourceBusy : ErrorType
  176. io.ErrorType.ResourceExhausted : ErrorType
  177. io.ErrorType.UserError : ErrorType
  178. unique type io.FilePath
  179. io.FilePath.FilePath : Text -> FilePath
  180. unique type io.Handle
  181. io.Handle.Handle : Text -> Handle
  182. unique type io.HostName
  183. io.HostName.HostName : Text -> HostName
  184. ability io.IO
  185. io.IO.accept_ : Socket ->{IO} Either Error Socket
  186. io.IO.bracket_ : '{IO} a
                        -> (a ->{IO} b)
                        -> (a ->{IO} c)
                        ->{IO} Either Error c
  187. io.IO.clientSocket_ : HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  188. io.IO.closeFile_ : Handle ->{IO} Either Error ()
  189. io.IO.closeSocket_ : Socket ->{IO} Either Error ()
  190. io.IO.createDirectory_ : FilePath ->{IO} Either Error ()
  191. io.IO.delay_ : Nat ->{IO} Either Error ()
  192. io.IO.directoryContents_ : FilePath
                                  ->{IO} Either Error [FilePath]
  193. io.IO.fileExists_ : FilePath ->{IO} Either Error Boolean
  194. io.IO.fork_ : '{IO} a ->{IO} Either Error ThreadId
  195. io.IO.getBuffering_ : Handle
                             ->{IO} Either
                               Error (Optional BufferMode)
  196. io.IO.getCurrentDirectory_ : {IO} (Either Error FilePath)
  197. io.IO.getFileSize_ : FilePath ->{IO} Either Error Nat
  198. io.IO.getFileTimestamp_ : FilePath
                                 ->{IO} Either Error EpochTime
  199. io.IO.getLine_ : Handle ->{IO} Either Error Text
  200. io.IO.getTemporaryDirectory_ : {IO} (Either
                                        Error FilePath)
  201. io.IO.getText_ : Handle ->{IO} Either Error Text
  202. io.IO.isDirectory_ : FilePath ->{IO} Either Error Boolean
  203. io.IO.isFileEOF_ : Handle ->{IO} Either Error Boolean
  204. io.IO.isFileOpen_ : Handle ->{IO} Either Error Boolean
  205. io.IO.isSeekable_ : Handle ->{IO} Either Error Boolean
  206. io.IO.kill_ : ThreadId ->{IO} Either Error ()
  207. io.IO.listen_ : Socket ->{IO} Either Error ()
  208. io.IO.openFile_ : FilePath
                         -> Mode
                         ->{IO} Either Error Handle
  209. io.IO.position_ : Handle ->{IO} Either Error Int
  210. io.IO.putText_ : Handle -> Text ->{IO} Either Error ()
  211. io.IO.receive_ : Socket
                        -> Nat
                        ->{IO} Either Error (Optional Bytes)
  212. io.IO.removeDirectory_ : FilePath ->{IO} Either Error ()
  213. io.IO.removeFile_ : FilePath ->{IO} Either Error ()
  214. io.IO.renameDirectory_ : FilePath
                                -> FilePath
                                ->{IO} Either Error ()
  215. io.IO.renameFile_ : FilePath
                           -> FilePath
                           ->{IO} Either Error ()
  216. io.IO.seek_ : Handle
                     -> SeekMode
                     -> Int
                     ->{IO} Either Error ()
  217. io.IO.send_ : Socket -> Bytes ->{IO} Either Error ()
  218. io.IO.serverSocket_ : Optional HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  219. io.IO.setBuffering_ : Handle
                             -> Optional BufferMode
                             ->{IO} Either Error ()
  220. io.IO.setCurrentDirectory_ : FilePath
                                    ->{IO} Either Error ()
  221. io.IO.systemTime_ : {IO} (Either Error EpochTime)
  222. io.IO.throw : Error ->{IO} a
  223. type io.Mode
  224. io.Mode.Append : Mode
  225. io.Mode.Read : Mode
  226. io.Mode.ReadWrite : Mode
  227. io.Mode.Write : Mode
  228. unique type io.SeekMode
  229. io.SeekMode.Absolute : SeekMode
  230. io.SeekMode.FromEnd : SeekMode
  231. io.SeekMode.Relative : SeekMode
  232. unique type io.ServiceName
  233. io.ServiceName.ServiceName : Text -> ServiceName
  234. unique type io.Socket
  235. io.Socket.Socket : Text -> Socket
  236. unique type io.ThreadId
  237. io.ThreadId.ThreadId : Text -> ThreadId
  238. io.accept : Socket ->{IO} Socket
  239. io.bracket : '{IO} a
                    -> (a ->{IO} b)
                    -> (a ->{IO} c)
                    ->{IO} c
  240. io.clientSocket : HostName -> ServiceName ->{IO} Socket
  241. io.closeFile : Handle ->{IO} ()
  242. io.closeSocket : Socket ->{IO} ()
  243. io.createDirectory : FilePath ->{IO} ()
  244. io.delay : Nat ->{IO} ()
  245. io.directoryContents : FilePath ->{IO} [FilePath]
  246. io.fileExists : FilePath ->{IO} Boolean
  247. io.fork : '{IO} a ->{IO} ThreadId
  248. io.getBuffering : Handle ->{IO} Optional BufferMode
  249. io.getCurrentDirectory : '{IO} FilePath
  250. io.getFileSize : FilePath ->{IO} Nat
  251. io.getFileTimestamp : FilePath ->{IO} EpochTime
  252. io.getLine : Handle ->{IO} Text
  253. io.getTemporaryDirectory : '{IO} FilePath
  254. io.getText : Handle ->{IO} Text
  255. io.isDirectory : FilePath ->{IO} Boolean
  256. io.isFileEOF : Handle ->{IO} Boolean
  257. io.isFileOpen : Handle ->{IO} Boolean
  258. io.isSeekable : Handle ->{IO} Boolean
  259. io.kill : ThreadId ->{IO} ()
  260. io.listen : Socket ->{IO} ()
  261. io.openFile : FilePath -> Mode ->{IO} Handle
  262. io.position : Handle ->{IO} Int
  263. io.printLine : Text ->{IO} ()
  264. io.putText : Handle -> Text ->{IO} ()
  265. io.readLine : '{IO} Text
  266. io.receive : Socket -> Nat ->{IO} Optional Bytes
  267. io.removeDirectory : FilePath ->{IO} ()
  268. io.removeFile : FilePath ->{IO} ()
  269. io.renameDirectory : FilePath -> FilePath ->{IO} ()
  270. io.renameFile : FilePath -> FilePath ->{IO} ()
  271. io.rethrow : Either Error a ->{IO} a
  272. io.seek : Handle -> SeekMode -> Int ->{IO} ()
  273. io.send : Socket -> Bytes ->{IO} ()
  274. io.serverSocket : Optional HostName
                         -> ServiceName
                         ->{IO} Socket
  275. io.setBuffering : Handle -> Optional BufferMode ->{IO} ()
  276. io.setCurrentDirectory : FilePath ->{IO} ()
  277. io.stderr : Handle
  278. io.stdin : Handle
  279. io.stdout : Handle
  280. io.systemTime : '{IO} EpochTime
  281. metadata.isPropagated : IsPropagated
  282. metadata.isTest : IsTest
  283. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type List
    2.  List.++     : [a] -> [a] -> [a]
    3.  ┌ List.+:   : a -> [a] -> [a]
    4.  └ List.cons : a -> [a] -> [a]
    5.  ┌ List.:+   : [a] -> a -> [a]
    6.  └ List.snoc : [a] -> a -> [a]
    7.  List.at     : Nat -> [a] -> Optional a
    8.  List.drop   : Nat -> [a] -> [a]
    9.  List.empty  : [a]
    10. List.size   : [a] -> Nat
    11. List.take   : Nat -> [a] -> [a]
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar

.runar> find

  1.  List.adjacentPairs : [a] -> [(a, a)]
  2.  List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  3.  List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  4.  List.chunk : Nat -> [a] -> [[a]]
  5.  List.chunksOf : Nat -> [a] -> [[a]]
  6.  List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
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
    2.  List.all           : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} Boolean
    3.  List.any           : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} Boolean
    4.  List.chunk         : Nat -> [a] -> [[a]]
    5.  List.chunksOf      : Nat -> [a] -> [[a]]
    6.  List.dropWhile     : (a ->{𝕖} Boolean)
                           ->{𝕖} [a]
                           ->{𝕖} [a]
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

  1.  builtin type List
  2.  List.++ : [a] -> [a] -> [a]
  3.  List.+: : a -> [a] -> [a]
  4.  List.:+ : [a] -> a -> [a]
  5.  List.adjacentPairs : [a] -> [(a, a)]
  6.  List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  7.  List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  8.  List.at : Nat -> [a] -> Optional a
  9.  List.chunk : Nat -> [a] -> [[a]]
  10. List.chunksOf : Nat -> [a] -> [[a]]
  11. List.cons : a -> [a] -> [a]
  12. List.drop : Nat -> [a] -> [a]
  13. List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
  14. List.empty : [a]
  15. List.first : [a] -> Optional a
  16. List.init : [a] -> Optional [a]
  17. List.intersperse : a -> [a] -> [a]
  18. List.isEmpty : [a] -> Boolean
  19. List.last : [a] -> Optional a
  20. List.replicate : Nat -> a -> [a]
  21. List.size : [a] -> Nat
  22. List.snoc : [a] -> a -> [a]
  23. List.splitAt : Nat -> [a] -> ([a], [a])
  24. List.tail : [a] -> Optional [a]
  25. List.take : Nat -> [a] -> [a]
  26. List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  

```
Thanks, `alias.many!
