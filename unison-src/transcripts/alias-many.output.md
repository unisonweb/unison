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

  1.   unique type Author
  2.   Author.Author : GUID -> Text -> Author
  3.   Author.guid : Author -> GUID
  4.   Author.guid.modify : (GUID ->{𝕖} GUID)
                            -> Author
                            ->{𝕖} Author
  5.   Author.guid.set : GUID -> Author -> Author
  6.   Author.name : Author -> Text
  7.   Author.name.modify : (Text ->{𝕖} Text)
                            -> Author
                            ->{𝕖} Author
  8.   Author.name.set : Text -> Author -> Author
  9.   builtin type Boolean
  10.  Boolean.not : Boolean -> Boolean
  11.  builtin type Bytes
  12.  Bytes.++ : Bytes -> Bytes -> Bytes
  13.  Bytes.at : Nat -> Bytes -> Optional Nat
  14.  Bytes.drop : Nat -> Bytes -> Bytes
  15.  Bytes.empty : Bytes
  16.  Bytes.flatten : Bytes -> Bytes
  17.  Bytes.fromList : [Nat] -> Bytes
  18.  Bytes.size : Bytes -> Nat
  19.  Bytes.take : Nat -> Bytes -> Bytes
  20.  Bytes.toList : Bytes -> [Nat]
  21.  builtin type Char
  22.  Char.fromNat : Nat -> Char
  23.  Char.toNat : Char -> Nat
  24.  unique type CopyrightHolder
  25.  CopyrightHolder.CopyrightHolder : GUID
                                         -> Text
                                         -> CopyrightHolder
  26.  CopyrightHolder.guid : CopyrightHolder -> GUID
  27.  CopyrightHolder.guid.modify : (GUID ->{𝕖} GUID)
                                     -> CopyrightHolder
                                     ->{𝕖} CopyrightHolder
  28.  CopyrightHolder.guid.set : GUID
                                  -> CopyrightHolder
                                  -> CopyrightHolder
  29.  CopyrightHolder.name : CopyrightHolder -> Text
  30.  CopyrightHolder.name.modify : (Text ->{𝕖} Text)
                                     -> CopyrightHolder
                                     ->{𝕖} CopyrightHolder
  31.  CopyrightHolder.name.set : Text
                                  -> CopyrightHolder
                                  -> CopyrightHolder
  32.  Debug.watch : Text -> a -> a
  33.  unique type Doc
  34.  Doc.++ : Doc -> Doc -> Doc
  35.  Doc.Blob : Text -> Doc
  36.  Doc.Evaluate : Term -> Doc
  37.  Doc.Join : [Doc] -> Doc
  38.  Doc.Link : Link -> Doc
  39.  Doc.Signature : Term -> Doc
  40.  Doc.Source : Link -> Doc
  41.  type Either a b
  42.  Either.Left : a -> Either a b
  43.  Either.Right : b -> Either a b
  44.  builtin type Float
  45.  Float.* : Float -> Float -> Float
  46.  Float.+ : Float -> Float -> Float
  47.  Float.- : Float -> Float -> Float
  48.  Float./ : Float -> Float -> Float
  49.  Float.abs : Float -> Float
  50.  Float.acos : Float -> Float
  51.  Float.acosh : Float -> Float
  52.  Float.asin : Float -> Float
  53.  Float.asinh : Float -> Float
  54.  Float.atan : Float -> Float
  55.  Float.atan2 : Float -> Float -> Float
  56.  Float.atanh : Float -> Float
  57.  Float.ceiling : Float -> Int
  58.  Float.cos : Float -> Float
  59.  Float.cosh : Float -> Float
  60.  Float.eq : Float -> Float -> Boolean
  61.  Float.exp : Float -> Float
  62.  Float.floor : Float -> Int
  63.  Float.fromText : Text -> Optional Float
  64.  Float.gt : Float -> Float -> Boolean
  65.  Float.gteq : Float -> Float -> Boolean
  66.  Float.log : Float -> Float
  67.  Float.logBase : Float -> Float -> Float
  68.  Float.lt : Float -> Float -> Boolean
  69.  Float.lteq : Float -> Float -> Boolean
  70.  Float.max : Float -> Float -> Float
  71.  Float.min : Float -> Float -> Float
  72.  Float.pow : Float -> Float -> Float
  73.  Float.round : Float -> Int
  74.  Float.sin : Float -> Float
  75.  Float.sinh : Float -> Float
  76.  Float.sqrt : Float -> Float
  77.  Float.tan : Float -> Float
  78.  Float.tanh : Float -> Float
  79.  Float.toText : Float -> Text
  80.  Float.truncate : Float -> Int
  81.  unique type GUID
  82.  GUID.GUID : Bytes -> GUID
  83.  builtin type Int
  84.  Int.* : Int -> Int -> Int
  85.  Int.+ : Int -> Int -> Int
  86.  Int.- : Int -> Int -> Int
  87.  Int./ : Int -> Int -> Int
  88.  Int.eq : Int -> Int -> Boolean
  89.  Int.fromText : Text -> Optional Int
  90.  Int.gt : Int -> Int -> Boolean
  91.  Int.gteq : Int -> Int -> Boolean
  92.  Int.increment : Int -> Int
  93.  Int.isEven : Int -> Boolean
  94.  Int.isOdd : Int -> Boolean
  95.  Int.lt : Int -> Int -> Boolean
  96.  Int.lteq : Int -> Int -> Boolean
  97.  Int.mod : Int -> Int -> Int
  98.  Int.negate : Int -> Int
  99.  Int.signum : Int -> Int
  100. Int.toFloat : Int -> Float
  101. Int.toText : Int -> Text
  102. Int.truncate0 : Int -> Nat
  103. unique type IsPropagated
  104. IsPropagated.IsPropagated : IsPropagated
  105. unique type IsTest
  106. IsTest.IsTest : IsTest
  107. unique type License
  108. License.License : [CopyrightHolder]
                         -> [Year]
                         -> LicenseType
                         -> License
  109. License.copyrightHolders : License -> [CopyrightHolder]
  110. License.copyrightHolders.modify : ([CopyrightHolder]
                                         ->{𝕖} [CopyrightHolder])
                                         -> License
                                         ->{𝕖} License
  111. License.copyrightHolders.set : [CopyrightHolder]
                                      -> License
                                      -> License
  112. License.licenseType : License -> LicenseType
  113. License.licenseType.modify : (LicenseType
                                    ->{𝕖} LicenseType)
                                    -> License
                                    ->{𝕖} License
  114. License.licenseType.set : LicenseType
                                 -> License
                                 -> License
  115. License.years : License -> [Year]
  116. License.years.modify : ([Year] ->{𝕖} [Year])
                              -> License
                              ->{𝕖} License
  117. License.years.set : [Year] -> License -> License
  118. unique type LicenseType
  119. LicenseType.LicenseType : Doc -> LicenseType
  120. unique type Link
  121. builtin type Link.Term
  122. Link.Term : Term -> Link
  123. builtin type Link.Type
  124. Link.Type : Type -> Link
  125. builtin type List
  126. List.++ : [a] -> [a] -> [a]
  127. List.+: : a -> [a] -> [a]
  128. List.:+ : [a] -> a -> [a]
  129. List.at : Nat -> [a] -> Optional a
  130. List.cons : a -> [a] -> [a]
  131. List.drop : Nat -> [a] -> [a]
  132. List.empty : [a]
  133. List.size : [a] -> Nat
  134. List.snoc : [a] -> a -> [a]
  135. List.take : Nat -> [a] -> [a]
  136. builtin type Nat
  137. Nat.* : Nat -> Nat -> Nat
  138. Nat.+ : Nat -> Nat -> Nat
  139. Nat./ : Nat -> Nat -> Nat
  140. Nat.drop : Nat -> Nat -> Nat
  141. Nat.eq : Nat -> Nat -> Boolean
  142. Nat.fromText : Text -> Optional Nat
  143. Nat.gt : Nat -> Nat -> Boolean
  144. Nat.gteq : Nat -> Nat -> Boolean
  145. Nat.increment : Nat -> Nat
  146. Nat.isEven : Nat -> Boolean
  147. Nat.isOdd : Nat -> Boolean
  148. Nat.lt : Nat -> Nat -> Boolean
  149. Nat.lteq : Nat -> Nat -> Boolean
  150. Nat.mod : Nat -> Nat -> Nat
  151. Nat.sub : Nat -> Nat -> Int
  152. Nat.toFloat : Nat -> Float
  153. Nat.toInt : Nat -> Int
  154. Nat.toText : Nat -> Text
  155. type Optional a
  156. Optional.None : Optional a
  157. Optional.Some : a -> Optional a
  158. builtin type Request
  159. unique type Test.Result
  160. Test.Result.Fail : Text -> Result
  161. Test.Result.Ok : Text -> Result
  162. builtin type Text
  163. Text.!= : Text -> Text -> Boolean
  164. Text.++ : Text -> Text -> Text
  165. Text.drop : Nat -> Text -> Text
  166. Text.empty : Text
  167. Text.eq : Text -> Text -> Boolean
  168. Text.fromCharList : [Char] -> Text
  169. Text.gt : Text -> Text -> Boolean
  170. Text.gteq : Text -> Text -> Boolean
  171. Text.lt : Text -> Text -> Boolean
  172. Text.lteq : Text -> Text -> Boolean
  173. Text.size : Text -> Nat
  174. Text.take : Nat -> Text -> Text
  175. Text.toCharList : Text -> [Char]
  176. Text.uncons : Text -> Optional (Char, Text)
  177. Text.unsnoc : Text -> Optional (Text, Char)
  178. type Tuple a b
  179. Tuple.Cons : a -> b -> Tuple a b
  180. type Unit
  181. Unit.Unit : ()
  182. Universal.< : a -> a -> Boolean
  183. Universal.<= : a -> a -> Boolean
  184. Universal.== : a -> a -> Boolean
  185. Universal.> : a -> a -> Boolean
  186. Universal.>= : a -> a -> Boolean
  187. Universal.compare : a -> a -> Int
  188. unique type Year
  189. Year.Year : Nat -> Year
  190. bug : a -> b
  191. unique type io.BufferMode
  192. io.BufferMode.Block : Optional Nat -> BufferMode
  193. io.BufferMode.Line : BufferMode
  194. unique type io.EpochTime
  195. io.EpochTime.EpochTime : Nat -> EpochTime
  196. type io.Error
  197. io.Error.Error : ErrorType -> Text -> Error
  198. unique type io.ErrorDescription
  199. io.ErrorDescription.ErrorDescription : Text
                                              -> ErrorDescription
  200. unique type io.ErrorLocation
  201. io.ErrorLocation.ErrorLocation : Text -> ErrorLocation
  202. unique type io.ErrorType
  203. io.ErrorType.AlreadyExists : ErrorType
  204. io.ErrorType.EOF : ErrorType
  205. io.ErrorType.IllegalOperation : ErrorType
  206. io.ErrorType.NoSuchThing : ErrorType
  207. io.ErrorType.PermissionDenied : ErrorType
  208. io.ErrorType.ResourceBusy : ErrorType
  209. io.ErrorType.ResourceExhausted : ErrorType
  210. io.ErrorType.UserError : ErrorType
  211. unique type io.FilePath
  212. io.FilePath.FilePath : Text -> FilePath
  213. unique type io.Handle
  214. io.Handle.Handle : Text -> Handle
  215. unique type io.HostName
  216. io.HostName.HostName : Text -> HostName
  217. ability io.IO
  218. io.IO.accept_ : Socket ->{IO} Either Error Socket
  219. io.IO.bracket_ : '{IO} a
                        -> (a ->{IO} b)
                        -> (a ->{IO} c)
                        ->{IO} Either Error c
  220. io.IO.clientSocket_ : HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  221. io.IO.closeFile_ : Handle ->{IO} Either Error ()
  222. io.IO.closeSocket_ : Socket ->{IO} Either Error ()
  223. io.IO.createDirectory_ : FilePath ->{IO} Either Error ()
  224. io.IO.delay_ : Nat ->{IO} Either Error ()
  225. io.IO.directoryContents_ : FilePath
                                  ->{IO} Either Error [FilePath]
  226. io.IO.fileExists_ : FilePath ->{IO} Either Error Boolean
  227. io.IO.fork_ : '{IO} a ->{IO} Either Error ThreadId
  228. io.IO.getBuffering_ : Handle
                             ->{IO} Either
                               Error (Optional BufferMode)
  229. io.IO.getCurrentDirectory_ : {IO} (Either Error FilePath)
  230. io.IO.getFileSize_ : FilePath ->{IO} Either Error Nat
  231. io.IO.getFileTimestamp_ : FilePath
                                 ->{IO} Either Error EpochTime
  232. io.IO.getLine_ : Handle ->{IO} Either Error Text
  233. io.IO.getTemporaryDirectory_ : {IO} (Either
                                        Error FilePath)
  234. io.IO.getText_ : Handle ->{IO} Either Error Text
  235. io.IO.isDirectory_ : FilePath ->{IO} Either Error Boolean
  236. io.IO.isFileEOF_ : Handle ->{IO} Either Error Boolean
  237. io.IO.isFileOpen_ : Handle ->{IO} Either Error Boolean
  238. io.IO.isSeekable_ : Handle ->{IO} Either Error Boolean
  239. io.IO.kill_ : ThreadId ->{IO} Either Error ()
  240. io.IO.listen_ : Socket ->{IO} Either Error ()
  241. io.IO.openFile_ : FilePath
                         -> Mode
                         ->{IO} Either Error Handle
  242. io.IO.position_ : Handle ->{IO} Either Error Int
  243. io.IO.putText_ : Handle -> Text ->{IO} Either Error ()
  244. io.IO.receive_ : Socket
                        -> Nat
                        ->{IO} Either Error (Optional Bytes)
  245. io.IO.removeDirectory_ : FilePath ->{IO} Either Error ()
  246. io.IO.removeFile_ : FilePath ->{IO} Either Error ()
  247. io.IO.renameDirectory_ : FilePath
                                -> FilePath
                                ->{IO} Either Error ()
  248. io.IO.renameFile_ : FilePath
                           -> FilePath
                           ->{IO} Either Error ()
  249. io.IO.seek_ : Handle
                     -> SeekMode
                     -> Int
                     ->{IO} Either Error ()
  250. io.IO.send_ : Socket -> Bytes ->{IO} Either Error ()
  251. io.IO.serverSocket_ : Optional HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  252. io.IO.setBuffering_ : Handle
                             -> Optional BufferMode
                             ->{IO} Either Error ()
  253. io.IO.setCurrentDirectory_ : FilePath
                                    ->{IO} Either Error ()
  254. io.IO.systemTime_ : {IO} (Either Error EpochTime)
  255. io.IO.throw : Error ->{IO} a
  256. type io.Mode
  257. io.Mode.Append : Mode
  258. io.Mode.Read : Mode
  259. io.Mode.ReadWrite : Mode
  260. io.Mode.Write : Mode
  261. unique type io.SeekMode
  262. io.SeekMode.Absolute : SeekMode
  263. io.SeekMode.FromEnd : SeekMode
  264. io.SeekMode.Relative : SeekMode
  265. unique type io.ServiceName
  266. io.ServiceName.ServiceName : Text -> ServiceName
  267. unique type io.Socket
  268. io.Socket.Socket : Text -> Socket
  269. unique type io.ThreadId
  270. io.ThreadId.ThreadId : Text -> ThreadId
  271. io.accept : Socket ->{IO} Socket
  272. io.bracket : '{IO} a
                    -> (a ->{IO} b)
                    -> (a ->{IO} c)
                    ->{IO} c
  273. io.clientSocket : HostName -> ServiceName ->{IO} Socket
  274. io.closeFile : Handle ->{IO} ()
  275. io.closeSocket : Socket ->{IO} ()
  276. io.createDirectory : FilePath ->{IO} ()
  277. io.delay : Nat ->{IO} ()
  278. io.directoryContents : FilePath ->{IO} [FilePath]
  279. io.fileExists : FilePath ->{IO} Boolean
  280. io.fork : '{IO} a ->{IO} ThreadId
  281. io.getBuffering : Handle ->{IO} Optional BufferMode
  282. io.getCurrentDirectory : '{IO} FilePath
  283. io.getFileSize : FilePath ->{IO} Nat
  284. io.getFileTimestamp : FilePath ->{IO} EpochTime
  285. io.getLine : Handle ->{IO} Text
  286. io.getTemporaryDirectory : '{IO} FilePath
  287. io.getText : Handle ->{IO} Text
  288. io.isDirectory : FilePath ->{IO} Boolean
  289. io.isFileEOF : Handle ->{IO} Boolean
  290. io.isFileOpen : Handle ->{IO} Boolean
  291. io.isSeekable : Handle ->{IO} Boolean
  292. io.kill : ThreadId ->{IO} ()
  293. io.listen : Socket ->{IO} ()
  294. io.openFile : FilePath -> Mode ->{IO} Handle
  295. io.position : Handle ->{IO} Int
  296. io.printLine : Text ->{IO} ()
  297. io.putText : Handle -> Text ->{IO} ()
  298. io.readLine : '{IO} Text
  299. io.receive : Socket -> Nat ->{IO} Optional Bytes
  300. io.removeDirectory : FilePath ->{IO} ()
  301. io.removeFile : FilePath ->{IO} ()
  302. io.renameDirectory : FilePath -> FilePath ->{IO} ()
  303. io.renameFile : FilePath -> FilePath ->{IO} ()
  304. io.rethrow : Either Error a ->{IO} a
  305. io.seek : Handle -> SeekMode -> Int ->{IO} ()
  306. io.send : Socket -> Bytes ->{IO} ()
  307. io.serverSocket : Optional HostName
                         -> ServiceName
                         ->{IO} Socket
  308. io.setBuffering : Handle -> Optional BufferMode ->{IO} ()
  309. io.setCurrentDirectory : FilePath ->{IO} ()
  310. io.stderr : Handle
  311. io.stdin : Handle
  312. io.stdout : Handle
  313. io.systemTime : '{IO} EpochTime
  314. metadata.isPropagated : IsPropagated
  315. metadata.isTest : IsTest
  316. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  unique type IsPropagated
    2.  IsPropagated.IsPropagated : IsPropagated
    3.  Int.isOdd                 : Int -> Boolean
    4.  Int.lt                    : Int -> Int -> Boolean
    5.  Int.lteq                  : Int -> Int -> Boolean
    6.  Int.mod                   : Int -> Int -> Int
    7.  Int.negate                : Int -> Int
    8.  Int.signum                : Int -> Int
    9.  Int.toFloat               : Int -> Float
    10. Int.toText                : Int -> Text
    11. Int.truncate0             : Int -> Nat
  
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

  1.  Int.isOdd : Int -> Boolean
  2.  Int.lt : Int -> Int -> Boolean
  3.  Int.lteq : Int -> Int -> Boolean
  4.  Int.mod : Int -> Int -> Int
  5.  Int.negate : Int -> Int
  6.  Int.signum : Int -> Int
  7.  Int.toFloat : Int -> Float
  8.  Int.toText : Int -> Text
  9.  Int.truncate0 : Int -> Nat
  10. unique type IsPropagated
  11. IsPropagated.IsPropagated : IsPropagated
  12. List.adjacentPairs : [a] -> [(a, a)]
  13. List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  14. List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  15. List.chunk : Nat -> [a] -> [[a]]
  16. List.chunksOf : Nat -> [a] -> [[a]]
  17. List.dropWhile : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} [a]
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
