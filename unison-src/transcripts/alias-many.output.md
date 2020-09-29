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
  9.   Bytes.fromList : [Nat] -> Bytes
  10.  Bytes.size : Bytes -> Nat
  11.  Bytes.take : Nat -> Bytes -> Bytes
  12.  Bytes.toList : Bytes -> [Nat]
  13.  builtin type Char
  14.  Char.fromNat : Nat -> Char
  15.  Char.toNat : Char -> Nat
  16.  Debug.watch : Text -> a -> a
  17.  unique type Doc
  18.  Doc.Blob : Text -> Doc
  19.  Doc.Evaluate : Term -> Doc
  20.  Doc.Join : [Doc] -> Doc
  21.  Doc.Link : Link -> Doc
  22.  Doc.Signature : Term -> Doc
  23.  Doc.Source : Link -> Doc
  24.  type Either a b
  25.  Either.Left : a -> Either a b
  26.  Either.Right : b -> Either a b
  27.  builtin type Float
  28.  Float.* : Float -> Float -> Float
  29.  Float.+ : Float -> Float -> Float
  30.  Float.- : Float -> Float -> Float
  31.  Float./ : Float -> Float -> Float
  32.  Float.abs : Float -> Float
  33.  Float.acos : Float -> Float
  34.  Float.acosh : Float -> Float
  35.  Float.asin : Float -> Float
  36.  Float.asinh : Float -> Float
  37.  Float.atan : Float -> Float
  38.  Float.atan2 : Float -> Float -> Float
  39.  Float.atanh : Float -> Float
  40.  Float.ceiling : Float -> Int
  41.  Float.cos : Float -> Float
  42.  Float.cosh : Float -> Float
  43.  Float.eq : Float -> Float -> Boolean
  44.  Float.exp : Float -> Float
  45.  Float.floor : Float -> Int
  46.  Float.fromText : Text -> Optional Float
  47.  Float.gt : Float -> Float -> Boolean
  48.  Float.gteq : Float -> Float -> Boolean
  49.  Float.log : Float -> Float
  50.  Float.logBase : Float -> Float -> Float
  51.  Float.lt : Float -> Float -> Boolean
  52.  Float.lteq : Float -> Float -> Boolean
  53.  Float.max : Float -> Float -> Float
  54.  Float.min : Float -> Float -> Float
  55.  Float.pow : Float -> Float -> Float
  56.  Float.round : Float -> Int
  57.  Float.sin : Float -> Float
  58.  Float.sinh : Float -> Float
  59.  Float.sqrt : Float -> Float
  60.  Float.tan : Float -> Float
  61.  Float.tanh : Float -> Float
  62.  Float.toText : Float -> Text
  63.  Float.truncate : Float -> Int
  64.  builtin type Int
  65.  Int.* : Int -> Int -> Int
  66.  Int.+ : Int -> Int -> Int
  67.  Int.- : Int -> Int -> Int
  68.  Int./ : Int -> Int -> Int
  69.  Int.and : Int -> Int -> Int
  70.  Int.complement : Int -> Int
  71.  Int.eq : Int -> Int -> Boolean
  72.  Int.fromText : Text -> Optional Int
  73.  Int.gt : Int -> Int -> Boolean
  74.  Int.gteq : Int -> Int -> Boolean
  75.  Int.increment : Int -> Int
  76.  Int.isEven : Int -> Boolean
  77.  Int.isOdd : Int -> Boolean
  78.  Int.leadingZeros : Int -> Nat
  79.  Int.lt : Int -> Int -> Boolean
  80.  Int.lteq : Int -> Int -> Boolean
  81.  Int.mod : Int -> Int -> Int
  82.  Int.negate : Int -> Int
  83.  Int.or : Int -> Int -> Int
  84.  Int.pow : Int -> Nat -> Int
  85.  Int.shiftLeft : Int -> Nat -> Int
  86.  Int.shiftRight : Int -> Nat -> Int
  87.  Int.signum : Int -> Int
  88.  Int.toFloat : Int -> Float
  89.  Int.toText : Int -> Text
  90.  Int.trailingZeros : Int -> Nat
  91.  Int.truncate0 : Int -> Nat
  92.  Int.xor : Int -> Int -> Int
  93.  unique type Link
  94.  builtin type Link.Term
  95.  Link.Term : Term -> Link
  96.  builtin type Link.Type
  97.  Link.Type : Type -> Link
  98.  builtin type List
  99.  List.++ : [a] -> [a] -> [a]
  100. List.+: : a -> [a] -> [a]
  101. List.:+ : [a] -> a -> [a]
  102. List.at : Nat -> [a] -> Optional a
  103. List.cons : a -> [a] -> [a]
  104. List.drop : Nat -> [a] -> [a]
  105. List.empty : [a]
  106. List.size : [a] -> Nat
  107. List.snoc : [a] -> a -> [a]
  108. List.take : Nat -> [a] -> [a]
  109. builtin type Nat
  110. Nat.* : Nat -> Nat -> Nat
  111. Nat.+ : Nat -> Nat -> Nat
  112. Nat./ : Nat -> Nat -> Nat
  113. Nat.and : Nat -> Nat -> Nat
  114. Nat.complement : Nat -> Nat
  115. Nat.drop : Nat -> Nat -> Nat
  116. Nat.eq : Nat -> Nat -> Boolean
  117. Nat.fromText : Text -> Optional Nat
  118. Nat.gt : Nat -> Nat -> Boolean
  119. Nat.gteq : Nat -> Nat -> Boolean
  120. Nat.increment : Nat -> Nat
  121. Nat.isEven : Nat -> Boolean
  122. Nat.isOdd : Nat -> Boolean
  123. Nat.leadingZeros : Nat -> Nat
  124. Nat.lt : Nat -> Nat -> Boolean
  125. Nat.lteq : Nat -> Nat -> Boolean
  126. Nat.mod : Nat -> Nat -> Nat
  127. Nat.or : Nat -> Nat -> Nat
  128. Nat.pow : Nat -> Nat -> Nat
  129. Nat.shiftLeft : Nat -> Nat -> Nat
  130. Nat.shiftRight : Nat -> Nat -> Nat
  131. Nat.sub : Nat -> Nat -> Int
  132. Nat.toFloat : Nat -> Float
  133. Nat.toInt : Nat -> Int
  134. Nat.toText : Nat -> Text
  135. Nat.trailingZeros : Nat -> Nat
  136. Nat.xor : Nat -> Nat -> Nat
  137. type Optional a
  138. Optional.None : Optional a
  139. Optional.Some : a -> Optional a
  140. builtin type Request
  141. type SeqView a b
  142. SeqView.VElem : a -> b -> SeqView a b
  143. SeqView.VEmpty : SeqView a b
  144. unique type Test.Result
  145. Test.Result.Fail : Text -> Result
  146. Test.Result.Ok : Text -> Result
  147. builtin type Text
  148. Text.!= : Text -> Text -> Boolean
  149. Text.++ : Text -> Text -> Text
  150. Text.drop : Nat -> Text -> Text
  151. Text.empty : Text
  152. Text.eq : Text -> Text -> Boolean
  153. Text.fromCharList : [Char] -> Text
  154. Text.gt : Text -> Text -> Boolean
  155. Text.gteq : Text -> Text -> Boolean
  156. Text.lt : Text -> Text -> Boolean
  157. Text.lteq : Text -> Text -> Boolean
  158. Text.size : Text -> Nat
  159. Text.take : Nat -> Text -> Text
  160. Text.toCharList : Text -> [Char]
  161. Text.uncons : Text -> Optional (Char, Text)
  162. Text.unsnoc : Text -> Optional (Text, Char)
  163. type Tuple a b
  164. Tuple.Cons : a -> b -> Tuple a b
  165. type Unit
  166. Unit.Unit : ()
  167. Universal.< : a -> a -> Boolean
  168. Universal.<= : a -> a -> Boolean
  169. Universal.== : a -> a -> Boolean
  170. Universal.> : a -> a -> Boolean
  171. Universal.>= : a -> a -> Boolean
  172. Universal.compare : a -> a -> Int
  173. bug : a -> b
  174. unique type io2.BufferMode
  175. io2.BufferMode.BlockBuffering : BufferMode
  176. io2.BufferMode.LineBuffering : BufferMode
  177. io2.BufferMode.NoBuffering : BufferMode
  178. io2.BufferMode.SizedBlockBuffering : Nat -> BufferMode
  179. unique type io2.FileMode
  180. io2.FileMode.Append : FileMode
  181. io2.FileMode.Read : FileMode
  182. io2.FileMode.ReadWrite : FileMode
  183. io2.FileMode.Write : FileMode
  184. builtin type io2.Handle
  185. builtin type io2.IO
  186. io2.IO.clientSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  187. io2.IO.closeFile : Handle ->{IO} Either IOError ()
  188. io2.IO.closeSocket : Socket ->{IO} Either IOError ()
  189. io2.IO.createDirectory : Text ->{IO} Either IOError ()
  190. io2.IO.delay : Nat ->{IO} Either IOError ()
  191. io2.IO.fileExists : Text ->{IO} Either IOError Boolean
  192. io2.IO.forkComp : '{IO} Either IOError a ->{IO} ThreadId
  193. io2.IO.getBuffering : Handle
                             ->{IO} Either IOError BufferMode
  194. io2.IO.getCurrentDirectory : '{IO} Either IOError Text
  195. io2.IO.getFileSize : Text ->{IO} Either IOError Nat
  196. io2.IO.getFileTimestamp : Text ->{IO} Either IOError Nat
  197. io2.IO.getLine : Handle ->{IO} Either IOError Text
  198. io2.IO.getTempDirectory : '{IO} Either IOError Text
  199. io2.IO.getText : Handle ->{IO} Either IOError Text
  200. io2.IO.handlePosition : Handle ->{IO} Either IOError Int
  201. io2.IO.isDirectory : Text ->{IO} Either IOError Boolean
  202. io2.IO.isFileEOF : Handle ->{IO} Either IOError Boolean
  203. io2.IO.isFileOpen : Handle ->{IO} Either IOError Boolean
  204. io2.IO.isSeekable : Handle ->{IO} Either IOError Boolean
  205. io2.IO.kill : ThreadId ->{IO} Either IOError ()
  206. io2.IO.listen : Socket ->{IO} Either IOError ()
  207. io2.IO.openFile : Text
                         -> FileMode
                         ->{IO} Either IOError Handle
  208. io2.IO.putText : Handle -> Text ->{IO} Either IOError ()
  209. io2.IO.removeDirectory : Text ->{IO} Either IOError ()
  210. io2.IO.removeFile : Text ->{IO} Either IOError ()
  211. io2.IO.renameDirectory : Text
                                -> Text
                                ->{IO} Either IOError ()
  212. io2.IO.renameFile : Text -> Text ->{IO} Either IOError ()
  213. io2.IO.seekHandle : Handle
                           -> SeekMode
                           -> Int
                           ->{IO} Either IOError ()
  214. io2.IO.serverSocket : Text
                             -> Text
                             ->{IO} Either IOError Socket
  215. io2.IO.setBuffering : Handle
                             -> BufferMode
                             ->{IO} Either IOError ()
  216. io2.IO.setCurrentDirectory : Text
                                    ->{IO} Either IOError ()
  217. io2.IO.socketAccept : Socket ->{IO} Either IOError Socket
  218. io2.IO.socketReceive : Socket
                              -> Nat
                              ->{IO} Either IOError Bytes
  219. io2.IO.socketSend : Socket
                           -> Bytes
                           ->{IO} Either IOError ()
  220. io2.IO.stdHandle : StdHandle -> Handle
  221. io2.IO.systemTime : '{IO} Either IOError Nat
  222. unique type io2.IOError
  223. io2.IOError.AlreadyExists : IOError
  224. io2.IOError.EOF : IOError
  225. io2.IOError.IllegalOperation : IOError
  226. io2.IOError.NoSuchThing : IOError
  227. io2.IOError.PermissionDenied : IOError
  228. io2.IOError.ResourceBusy : IOError
  229. io2.IOError.ResourceExhausted : IOError
  230. io2.IOError.UserError : IOError
  231. builtin type io2.MVar
  232. io2.MVar.isEmpty : MVar a ->{IO} Boolean
  233. io2.MVar.new : a ->{IO} MVar a
  234. io2.MVar.newEmpty : {IO} (MVar a)
  235. io2.MVar.put : MVar a -> a ->{IO} Either IOError ()
  236. io2.MVar.read : MVar a ->{IO} Either IOError a
  237. io2.MVar.swap : MVar a -> a ->{IO} Either IOError a
  238. io2.MVar.take : MVar a ->{IO} Either IOError a
  239. io2.MVar.tryPut : MVar a -> a ->{IO} Boolean
  240. io2.MVar.tryRead : MVar a ->{IO} Optional a
  241. io2.MVar.tryTake : MVar a ->{IO} Optional a
  242. unique type io2.SeekMode
  243. io2.SeekMode.AbsoluteSeek : SeekMode
  244. io2.SeekMode.RelativeSeek : SeekMode
  245. io2.SeekMode.SeekFromEnd : SeekMode
  246. builtin type io2.Socket
  247. unique type io2.StdHandle
  248. io2.StdHandle.StdErr : StdHandle
  249. io2.StdHandle.StdIn : StdHandle
  250. io2.StdHandle.StdOut : StdHandle
  251. builtin type io2.ThreadId
  252. todo : a -> b
  

.builtin> alias.many 94-104 .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  builtin type Link.Term
    2.  builtin type Link.Type
    3.  builtin type List
    4.  Link.Term   : Term -> Link
    5.  Link.Type   : Type -> Link
    6.  List.++     : [a] -> [a] -> [a]
    7.  ┌ List.+:   : a -> [a] -> [a]
    8.  └ List.cons : a -> [a] -> [a]
    9.  List.:+     : [a] -> a -> [a]
    10. List.at     : Nat -> [a] -> Optional a
    11. List.drop   : Nat -> [a] -> [a]
  
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

  1.  builtin type Link.Term
  2.  Link.Term : Term -> Link
  3.  builtin type Link.Type
  4.  Link.Type : Type -> Link
  5.  builtin type List
  6.  List.++ : [a] -> [a] -> [a]
  7.  List.+: : a -> [a] -> [a]
  8.  List.:+ : [a] -> a -> [a]
  9.  List.adjacentPairs : [a] -> [(a, a)]
  10. List.all : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  11. List.any : (a ->{𝕖} Boolean) ->{𝕖} [a] ->{𝕖} Boolean
  12. List.at : Nat -> [a] -> Optional a
  13. List.chunk : Nat -> [a] -> [[a]]
  14. List.chunksOf : Nat -> [a] -> [[a]]
  15. List.cons : a -> [a] -> [a]
  16. List.drop : Nat -> [a] -> [a]
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
