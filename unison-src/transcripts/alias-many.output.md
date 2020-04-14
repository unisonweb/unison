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
  99.  Int.pow : Int -> Nat -> Int
  100. Int.shiftLeft : Int -> Nat -> Int
  101. Int.shiftRight : Int -> Nat -> Int
  102. Int.signum : Int -> Int
  103. Int.toFloat : Int -> Float
  104. Int.toText : Int -> Text
  105. Int.truncate0 : Int -> Nat
  106. unique type IsPropagated
  107. IsPropagated.IsPropagated : IsPropagated
  108. unique type IsTest
  109. IsTest.IsTest : IsTest
  110. unique type License
  111. License.License : [CopyrightHolder]
                         -> [Year]
                         -> LicenseType
                         -> License
  112. License.copyrightHolders : License -> [CopyrightHolder]
  113. License.copyrightHolders.modify : ([CopyrightHolder]
                                         ->{𝕖} [CopyrightHolder])
                                         -> License
                                         ->{𝕖} License
  114. License.copyrightHolders.set : [CopyrightHolder]
                                      -> License
                                      -> License
  115. License.licenseType : License -> LicenseType
  116. License.licenseType.modify : (LicenseType
                                    ->{𝕖} LicenseType)
                                    -> License
                                    ->{𝕖} License
  117. License.licenseType.set : LicenseType
                                 -> License
                                 -> License
  118. License.years : License -> [Year]
  119. License.years.modify : ([Year] ->{𝕖} [Year])
                              -> License
                              ->{𝕖} License
  120. License.years.set : [Year] -> License -> License
  121. unique type LicenseType
  122. LicenseType.LicenseType : Doc -> LicenseType
  123. unique type Link
  124. builtin type Link.Term
  125. Link.Term : Term -> Link
  126. builtin type Link.Type
  127. Link.Type : Type -> Link
  128. builtin type List
  129. List.++ : [a] -> [a] -> [a]
  130. List.+: : a -> [a] -> [a]
  131. List.:+ : [a] -> a -> [a]
  132. List.at : Nat -> [a] -> Optional a
  133. List.cons : a -> [a] -> [a]
  134. List.drop : Nat -> [a] -> [a]
  135. List.empty : [a]
  136. List.size : [a] -> Nat
  137. List.snoc : [a] -> a -> [a]
  138. List.take : Nat -> [a] -> [a]
  139. builtin type Nat
  140. Nat.* : Nat -> Nat -> Nat
  141. Nat.+ : Nat -> Nat -> Nat
  142. Nat./ : Nat -> Nat -> Nat
  143. Nat.drop : Nat -> Nat -> Nat
  144. Nat.eq : Nat -> Nat -> Boolean
  145. Nat.fromText : Text -> Optional Nat
  146. Nat.gt : Nat -> Nat -> Boolean
  147. Nat.gteq : Nat -> Nat -> Boolean
  148. Nat.increment : Nat -> Nat
  149. Nat.isEven : Nat -> Boolean
  150. Nat.isOdd : Nat -> Boolean
  151. Nat.lt : Nat -> Nat -> Boolean
  152. Nat.lteq : Nat -> Nat -> Boolean
  153. Nat.mod : Nat -> Nat -> Nat
  154. Nat.pow : Nat -> Nat -> Nat
  155. Nat.shiftLeft : Nat -> Nat -> Nat
  156. Nat.shiftRight : Nat -> Nat -> Nat
  157. Nat.sub : Nat -> Nat -> Int
  158. Nat.toFloat : Nat -> Float
  159. Nat.toInt : Nat -> Int
  160. Nat.toText : Nat -> Text
  161. type Optional a
  162. Optional.None : Optional a
  163. Optional.Some : a -> Optional a
  164. builtin type Request
  165. unique type Test.Result
  166. Test.Result.Fail : Text -> Result
  167. Test.Result.Ok : Text -> Result
  168. builtin type Text
  169. Text.!= : Text -> Text -> Boolean
  170. Text.++ : Text -> Text -> Text
  171. Text.drop : Nat -> Text -> Text
  172. Text.empty : Text
  173. Text.eq : Text -> Text -> Boolean
  174. Text.fromCharList : [Char] -> Text
  175. Text.gt : Text -> Text -> Boolean
  176. Text.gteq : Text -> Text -> Boolean
  177. Text.lt : Text -> Text -> Boolean
  178. Text.lteq : Text -> Text -> Boolean
  179. Text.size : Text -> Nat
  180. Text.take : Nat -> Text -> Text
  181. Text.toCharList : Text -> [Char]
  182. Text.uncons : Text -> Optional (Char, Text)
  183. Text.unsnoc : Text -> Optional (Text, Char)
  184. type Tuple a b
  185. Tuple.Cons : a -> b -> Tuple a b
  186. type Unit
  187. Unit.Unit : ()
  188. Universal.< : a -> a -> Boolean
  189. Universal.<= : a -> a -> Boolean
  190. Universal.== : a -> a -> Boolean
  191. Universal.> : a -> a -> Boolean
  192. Universal.>= : a -> a -> Boolean
  193. Universal.compare : a -> a -> Int
  194. unique type Year
  195. Year.Year : Nat -> Year
  196. bug : a -> b
  197. unique type io.BufferMode
  198. io.BufferMode.Block : Optional Nat -> BufferMode
  199. io.BufferMode.Line : BufferMode
  200. unique type io.EpochTime
  201. io.EpochTime.EpochTime : Nat -> EpochTime
  202. type io.Error
  203. io.Error.Error : ErrorType -> Text -> Error
  204. unique type io.ErrorDescription
  205. io.ErrorDescription.ErrorDescription : Text
                                              -> ErrorDescription
  206. unique type io.ErrorLocation
  207. io.ErrorLocation.ErrorLocation : Text -> ErrorLocation
  208. unique type io.ErrorType
  209. io.ErrorType.AlreadyExists : ErrorType
  210. io.ErrorType.EOF : ErrorType
  211. io.ErrorType.IllegalOperation : ErrorType
  212. io.ErrorType.NoSuchThing : ErrorType
  213. io.ErrorType.PermissionDenied : ErrorType
  214. io.ErrorType.ResourceBusy : ErrorType
  215. io.ErrorType.ResourceExhausted : ErrorType
  216. io.ErrorType.UserError : ErrorType
  217. unique type io.FilePath
  218. io.FilePath.FilePath : Text -> FilePath
  219. unique type io.Handle
  220. io.Handle.Handle : Text -> Handle
  221. unique type io.HostName
  222. io.HostName.HostName : Text -> HostName
  223. ability io.IO
  224. io.IO.accept_ : Socket ->{IO} Either Error Socket
  225. io.IO.bracket_ : '{IO} a
                        -> (a ->{IO} b)
                        -> (a ->{IO} c)
                        ->{IO} Either Error c
  226. io.IO.clientSocket_ : HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  227. io.IO.closeFile_ : Handle ->{IO} Either Error ()
  228. io.IO.closeSocket_ : Socket ->{IO} Either Error ()
  229. io.IO.createDirectory_ : FilePath ->{IO} Either Error ()
  230. io.IO.delay_ : Nat ->{IO} Either Error ()
  231. io.IO.directoryContents_ : FilePath
                                  ->{IO} Either Error [FilePath]
  232. io.IO.fileExists_ : FilePath ->{IO} Either Error Boolean
  233. io.IO.fork_ : '{IO} a ->{IO} Either Error ThreadId
  234. io.IO.getBuffering_ : Handle
                             ->{IO} Either
                               Error (Optional BufferMode)
  235. io.IO.getCurrentDirectory_ : {IO} (Either Error FilePath)
  236. io.IO.getFileSize_ : FilePath ->{IO} Either Error Nat
  237. io.IO.getFileTimestamp_ : FilePath
                                 ->{IO} Either Error EpochTime
  238. io.IO.getLine_ : Handle ->{IO} Either Error Text
  239. io.IO.getTemporaryDirectory_ : {IO} (Either
                                        Error FilePath)
  240. io.IO.getText_ : Handle ->{IO} Either Error Text
  241. io.IO.isDirectory_ : FilePath ->{IO} Either Error Boolean
  242. io.IO.isFileEOF_ : Handle ->{IO} Either Error Boolean
  243. io.IO.isFileOpen_ : Handle ->{IO} Either Error Boolean
  244. io.IO.isSeekable_ : Handle ->{IO} Either Error Boolean
  245. io.IO.kill_ : ThreadId ->{IO} Either Error ()
  246. io.IO.listen_ : Socket ->{IO} Either Error ()
  247. io.IO.openFile_ : FilePath
                         -> Mode
                         ->{IO} Either Error Handle
  248. io.IO.position_ : Handle ->{IO} Either Error Int
  249. io.IO.putText_ : Handle -> Text ->{IO} Either Error ()
  250. io.IO.receive_ : Socket
                        -> Nat
                        ->{IO} Either Error (Optional Bytes)
  251. io.IO.removeDirectory_ : FilePath ->{IO} Either Error ()
  252. io.IO.removeFile_ : FilePath ->{IO} Either Error ()
  253. io.IO.renameDirectory_ : FilePath
                                -> FilePath
                                ->{IO} Either Error ()
  254. io.IO.renameFile_ : FilePath
                           -> FilePath
                           ->{IO} Either Error ()
  255. io.IO.seek_ : Handle
                     -> SeekMode
                     -> Int
                     ->{IO} Either Error ()
  256. io.IO.send_ : Socket -> Bytes ->{IO} Either Error ()
  257. io.IO.serverSocket_ : Optional HostName
                             -> ServiceName
                             ->{IO} Either Error Socket
  258. io.IO.setBuffering_ : Handle
                             -> Optional BufferMode
                             ->{IO} Either Error ()
  259. io.IO.setCurrentDirectory_ : FilePath
                                    ->{IO} Either Error ()
  260. io.IO.systemTime_ : {IO} (Either Error EpochTime)
  261. io.IO.throw : Error ->{IO} a
  262. type io.Mode
  263. io.Mode.Append : Mode
  264. io.Mode.Read : Mode
  265. io.Mode.ReadWrite : Mode
  266. io.Mode.Write : Mode
  267. unique type io.SeekMode
  268. io.SeekMode.Absolute : SeekMode
  269. io.SeekMode.FromEnd : SeekMode
  270. io.SeekMode.Relative : SeekMode
  271. unique type io.ServiceName
  272. io.ServiceName.ServiceName : Text -> ServiceName
  273. unique type io.Socket
  274. io.Socket.Socket : Text -> Socket
  275. unique type io.ThreadId
  276. io.ThreadId.ThreadId : Text -> ThreadId
  277. io.accept : Socket ->{IO} Socket
  278. io.bracket : '{IO} a
                    -> (a ->{IO} b)
                    -> (a ->{IO} c)
                    ->{IO} c
  279. io.clientSocket : HostName -> ServiceName ->{IO} Socket
  280. io.closeFile : Handle ->{IO} ()
  281. io.closeSocket : Socket ->{IO} ()
  282. io.createDirectory : FilePath ->{IO} ()
  283. io.delay : Nat ->{IO} ()
  284. io.directoryContents : FilePath ->{IO} [FilePath]
  285. io.fileExists : FilePath ->{IO} Boolean
  286. io.fork : '{IO} a ->{IO} ThreadId
  287. io.getBuffering : Handle ->{IO} Optional BufferMode
  288. io.getCurrentDirectory : '{IO} FilePath
  289. io.getFileSize : FilePath ->{IO} Nat
  290. io.getFileTimestamp : FilePath ->{IO} EpochTime
  291. io.getLine : Handle ->{IO} Text
  292. io.getTemporaryDirectory : '{IO} FilePath
  293. io.getText : Handle ->{IO} Text
  294. io.isDirectory : FilePath ->{IO} Boolean
  295. io.isFileEOF : Handle ->{IO} Boolean
  296. io.isFileOpen : Handle ->{IO} Boolean
  297. io.isSeekable : Handle ->{IO} Boolean
  298. io.kill : ThreadId ->{IO} ()
  299. io.listen : Socket ->{IO} ()
  300. io.openFile : FilePath -> Mode ->{IO} Handle
  301. io.position : Handle ->{IO} Int
  302. io.printLine : Text ->{IO} ()
  303. io.putText : Handle -> Text ->{IO} ()
  304. io.readLine : '{IO} Text
  305. io.receive : Socket -> Nat ->{IO} Optional Bytes
  306. io.removeDirectory : FilePath ->{IO} ()
  307. io.removeFile : FilePath ->{IO} ()
  308. io.renameDirectory : FilePath -> FilePath ->{IO} ()
  309. io.renameFile : FilePath -> FilePath ->{IO} ()
  310. io.rethrow : Either Error a ->{IO} a
  311. io.seek : Handle -> SeekMode -> Int ->{IO} ()
  312. io.send : Socket -> Bytes ->{IO} ()
  313. io.serverSocket : Optional HostName
                         -> ServiceName
                         ->{IO} Socket
  314. io.setBuffering : Handle -> Optional BufferMode ->{IO} ()
  315. io.setCurrentDirectory : FilePath ->{IO} ()
  316. io.stderr : Handle
  317. io.stdin : Handle
  318. io.stdout : Handle
  319. io.systemTime : '{IO} EpochTime
  320. metadata.isPropagated : IsPropagated
  321. metadata.isTest : IsTest
  322. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  Int.isOdd      : Int -> Boolean
    2.  Int.lt         : Int -> Int -> Boolean
    3.  Int.lteq       : Int -> Int -> Boolean
    4.  Int.mod        : Int -> Int -> Int
    5.  Int.negate     : Int -> Int
    6.  Int.pow        : Int -> Nat -> Int
    7.  Int.shiftLeft  : Int -> Nat -> Int
    8.  Int.shiftRight : Int -> Nat -> Int
    9.  Int.signum     : Int -> Int
    10. Int.toFloat    : Int -> Float
    11. Int.toText     : Int -> Text
  
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
  6.  Int.pow : Int -> Nat -> Int
  7.  Int.shiftLeft : Int -> Nat -> Int
  8.  Int.shiftRight : Int -> Nat -> Int
  9.  Int.signum : Int -> Int
  10. Int.toFloat : Int -> Float
  11. Int.toText : Int -> Text
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
