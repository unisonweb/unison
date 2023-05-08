This transcript shows how the pretty-printer picks names for a hash when multiple are available. The algorithm is:

1. Names that are "name-only" come before names that are hash qualified. So `List.map` comes before `List.map#2384a` and also `aaaa#xyz`.
2. Shorter names (in terms of segment count) come before longer ones, for instance `base.List.map` comes before `somelibrary.external.base.List.map`.
3. Otherwise if there are multiple names with a minimal number of segments, compare the names alphabetically.

```unison
a = b + 1
b = 0 + 1
```

Will add `a` and `b` to the codebase and give `b` a longer (in terms of segment length alias), and show that it isn't used when viewing `a`:

```ucm
  ☝️  The namespace .a is empty.

.a> add

  ⍟ I've added these definitions:
  
    a : Nat
    b : Nat

.a> alias.term b aaa.but.more.segments

  Done.

.a> view a

  a : Nat
  a = b + 1

.> cd .

```
Next let's introduce a conflicting symbol and show that its hash qualified name isn't used when it has an unconflicted name:

```
.> fork a a2
.> fork a a3

```

```unison
c = 1
d = c + 10
```

```ucm
.a2> add

  ⍟ I've added these definitions:
  
    c : Nat
    d : Nat

.a2> alias.term c long.name.but.shortest.suffixification

  Done.

```
```unison
c = 2
d = c + 10
```

```ucm
  ☝️  The namespace .a3 is empty.

.a3> add

  ⍟ I've added these definitions:
  
    c : Nat
    d : Nat

.a3> merge .a2 .a3

  Here's what's changed in .a3 after the merge:
  
  New name conflicts:
  
    1.   c#dcgdua2lj6 : Nat
         ↓
    2.   ┌ c#dcgdua2lj6 : Nat
    3.   └ c#gjmq673r1v : Nat
    
    4.   d#9ivhgvhthc : Nat
         ↓
    5.   ┌ d#9ivhgvhthc : Nat
    6.   └ d#ve16e6jmf6 : Nat
  
  Added definitions:
  
    7.   builtin type builtin.Any
    8.   unique type builtin.io2.ArithmeticFailure
    9.   unique type builtin.io2.ArrayFailure
    10.  builtin type builtin.Boolean
    11.  unique type builtin.io2.BufferMode
    12.  builtin type builtin.Bytes
    13.  builtin type builtin.Char
    14.  builtin type builtin.io2.Tls.Cipher
    15.  builtin type builtin.Char.Class
    16.  builtin type builtin.io2.Tls.ClientConfig
    17.  builtin type builtin.Code
    18.  unique type builtin.Doc
    19.  structural type builtin.Either a b
    20.  structural ability builtin.Exception
    21.  unique type builtin.io2.Failure
    22.  unique type builtin.io2.FileMode
    23.  builtin type builtin.Float
    24.  builtin type builtin.io2.Handle
    25.  builtin type builtin.crypto.HashAlgorithm
    26.  builtin ability builtin.io2.IO
    27.  unique type builtin.io2.IOError
    28.  unique type builtin.io2.IOFailure
    29.  builtin type builtin.ImmutableArray
    30.  builtin type builtin.ImmutableByteArray
    31.  builtin type builtin.Int
    32.  unique type builtin.IsPropagated
    33.  unique type builtin.IsTest
    34.  unique type builtin.Link
    35.  builtin type builtin.List
    36.  builtin type builtin.io2.MVar
    37.  unique type builtin.io2.MiscFailure
    38.  builtin type builtin.MutableArray
    39.  builtin type builtin.MutableByteArray
    40.  builtin type builtin.Nat
    41.  structural type builtin.Optional a
    42.  builtin type builtin.Pattern
    43.  builtin type builtin.io2.Tls.PrivateKey
    44.  builtin type builtin.io2.ProcessHandle
    45.  builtin type builtin.io2.Promise
    46.  builtin type builtin.Ref
    47.  builtin type builtin.Request
    48.  unique type builtin.Test.Result
    49.  unique type builtin.io2.RuntimeFailure
    50.  builtin ability builtin.io2.STM
    51.  unique type builtin.io2.STMFailure
    52.  builtin ability builtin.Scope
    53.  unique type builtin.io2.SeekMode
    54.  structural type builtin.SeqView a b
    55.  builtin type builtin.io2.Tls.ServerConfig
    56.  builtin type builtin.io2.Tls.SignedCert
    57.  builtin type builtin.io2.Socket
    58.  unique type builtin.io2.StdHandle
    59.  builtin type builtin.io2.TVar
    60.  builtin type builtin.Link.Term
    61.  builtin type builtin.Text
    62.  builtin type builtin.io2.ThreadId
    63.  unique type builtin.io2.ThreadKilledFailure
    64.  builtin type builtin.io2.Ref.Ticket
    65.  builtin type builtin.io2.Clock.internals.TimeSpec
    66.  builtin type builtin.io2.Tls
    67.  unique type builtin.io2.TlsFailure
    68.  structural type builtin.Tuple a b
    69.  builtin type builtin.Link.Type
    70.  structural type builtin.Unit
    71.  builtin type builtin.Value
    72.  builtin type builtin.io2.Tls.Version
    73.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    74.  builtin.io2.IOError.AlreadyExists             : IOError
    75.  builtin.io2.FileMode.Append                   : FileMode
    76.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    77.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    78.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    79.  builtin.io2.IOError.EOF                       : IOError
    80.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    81.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    82.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    83.  builtin.io2.IOError.IllegalOperation          : IOError
    84.  builtin.IsPropagated.IsPropagated             : IsPropagated
    85.  builtin.IsTest.IsTest                         : IsTest
    86.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    87.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    88.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    89.  builtin.Doc.Link                              : Link
                                                       -> Doc
    90.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    91.  builtin.io2.IOError.NoSuchThing               : IOError
    92.  builtin.Optional.None                         : Optional
                                                         a
    93.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    94.  builtin.io2.IOError.PermissionDenied          : IOError
    95.  builtin.io2.FileMode.Read                     : FileMode
    96.  builtin.io2.FileMode.ReadWrite                : FileMode
    97.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    98.  builtin.io2.IOError.ResourceBusy              : IOError
    99.  builtin.io2.IOError.ResourceExhausted         : IOError
    100. builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    101. builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    102. builtin.Doc.Signature                         : Term
                                                       -> Doc
    103. builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    104. builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    105. builtin.Doc.Source                            : Link
                                                       -> Doc
    106. builtin.io2.StdHandle.StdErr                  : StdHandle
    107. builtin.io2.StdHandle.StdIn                   : StdHandle
    108. builtin.io2.StdHandle.StdOut                  : StdHandle
    109. builtin.Link.Term                             : Term
                                                       -> Link
    110. builtin.Link.Type                             : Type
                                                       -> Link
    111. builtin.Unit.Unit                             : ()
    112. builtin.io2.IOError.UserError                 : IOError
    113. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    114. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    115. builtin.io2.FileMode.Write                    : FileMode
    116. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    117. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    118. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    119. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    120. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    121. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    122. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    123. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    124. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    125. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    126. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    127. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    128. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    129. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    130. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    131. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    132. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    133. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    134. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    135. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    136. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    137. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    138. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    139. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    140. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    141. builtin.Any.Any                               : a
                                                       -> Any
    142. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    143. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    144. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    145. builtin.crypto.HashAlgorithm.Md5              : HashAlgorithm
    146. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    147. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    148. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    149. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    150. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    151. builtin.Float.abs                             : Float
                                                       -> Float
    152. builtin.Float.acos                            : Float
                                                       -> Float
    153. builtin.Float.acosh                           : Float
                                                       -> Float
    154. builtin.Char.Class.alphanumeric               : Class
    155. builtin.Char.Class.and                        : Class
                                                       -> Class
                                                       -> Class
    156. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    157. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    158. builtin.Char.Class.any                        : Class
    159. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    160. builtin.Char.Class.anyOf                      : [Char]
                                                       -> Class
    161. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    162. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    163. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    164. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    165. builtin.Float.asin                            : Float
                                                       -> Float
    166. builtin.Float.asinh                           : Float
                                                       -> Float
    167. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    168. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    169. builtin.Float.atan                            : Float
                                                       -> Float
    170. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    171. builtin.Float.atanh                           : Float
                                                       -> Float
    172. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    173. builtin.bug                                   : a -> b
    174. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    175. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    176. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    177. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    178. ┌ c#gjmq673r1v                                : Nat
    179. └ long.name.but.shortest.suffixification      : Nat
    180. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    181. builtin.io2.IO.process.call                   : Text
                                                       -> [Text]
                                                       ->{IO} Nat
    182. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    183. builtin.Pattern.captureAs                     : a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    184. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    185. builtin.Float.ceiling                         : Float
                                                       -> Int
    186. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    187. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    188. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    189. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    190. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    191. builtin.Int.complement                        : Int
                                                       -> Int
    192. builtin.Nat.complement                        : Nat
                                                       -> Nat
    193. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    194. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    195. builtin.Char.Class.control                    : Class
    196. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    197. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    198. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    199. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    200. builtin.Float.cos                             : Float
                                                       -> Float
    201. builtin.Float.cosh                            : Float
                                                       -> Float
    202. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    203. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    204. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    205. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    206. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    207. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    208. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    209. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    210. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    211. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    212. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    213. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    214. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    215. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    216. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    217. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    218. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    219. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    220. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    221. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    222. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    223. builtin.Bytes.empty                           : Bytes
    224. builtin.List.empty                            : [a]
    225. builtin.Text.empty                            : Text
    226. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    227. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    228. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    229. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    230. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    231. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    232. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    233. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    234. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    235. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    236. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    237. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    238. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    239. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    240. builtin.Float.exp                             : Float
                                                       -> Float
    241. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    242. builtin.Float.floor                           : Float
                                                       -> Int
    243. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    244. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    245. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    246. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    247. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    248. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    249. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    250. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    251. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    252. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    253. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    254. builtin.Char.fromNat                          : Nat
                                                       -> Char
    255. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    256. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    257. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    258. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    259. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    260. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    261. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    262. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    263. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    264. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    265. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    266. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    267. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    268. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    269. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    270. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    271. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    272. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    273. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    275. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    276. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    277. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    278. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    279. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    280. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    281. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    282. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    283. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    284. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    285. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    286. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    287. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    288. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    289. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    290. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    291. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    292. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    293. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    294. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    295. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    296. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    297. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    298. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    299. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    300. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    301. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    302. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    303. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    304. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    305. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    306. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    307. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    308. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    309. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    310. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    311. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    312. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    313. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    315. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    316. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    317. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    318. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    319. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    320. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    321. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    322. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    323. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    324. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    325. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    326. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    327. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    328. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    329. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    330. builtin.Int.increment                         : Int
                                                       -> Int
    331. builtin.Nat.increment                         : Nat
                                                       -> Nat
    332. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    333. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    334. builtin.Int.isEven                            : Int
                                                       -> Boolean
    335. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    336. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    337. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    338. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    339. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    340. builtin.metadata.isPropagated                 : IsPropagated
    341. builtin.metadata.isTest                       : IsTest
    342. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    343. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    344. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    345. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    346. builtin.Char.Class.letter                     : Class
    347. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    348. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    349. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    350. builtin.Float.log                             : Float
                                                       -> Float
    351. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    352. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    353. builtin.Char.Class.lower                      : Class
    354. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    355. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    356. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    357. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    358. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    359. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    360. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    361. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    362. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    363. builtin.Char.Class.mark                       : Class
    364. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    365. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    366. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    367. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    368. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    369. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    370. builtin.Int.negate                            : Int
                                                       -> Int
    371. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    372. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    373. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    374. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    375. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    376. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    377. builtin.Char.Class.not                        : Class
                                                       -> Class
    378. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    379. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    380. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    381. builtin.Char.Class.number                     : Class
    382. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    383. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    384. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    385. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    386. builtin.Int.popCount                          : Int
                                                       -> Nat
    387. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    388. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    389. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    390. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    391. builtin.Char.Class.printable                  : Class
    392. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    393. builtin.Char.Class.punctuation                : Class
    394. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    395. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    396. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    397. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    398. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    399. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    400. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    401. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    402. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    403. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    404. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    405. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    406. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    407. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    408. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    409. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    410. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    411. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    412. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    413. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    414. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    415. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    416. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    417. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    418. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    419. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    420. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    421. builtin.io2.STM.retry                         : '{STM} a
    422. builtin.Text.reverse                          : Text
                                                       -> Text
    423. builtin.Float.round                           : Float
                                                       -> Int
    424. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    425. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    426. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    427. builtin.Char.Class.separator                  : Class
    428. builtin.Code.serialize                        : Code
                                                       -> Bytes
    429. builtin.Value.serialize                       : Value
                                                       -> Bytes
    430. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    431. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    432. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    433. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    434. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    435. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    436. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    437. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    438. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    439. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    440. builtin.Int.signum                            : Int
                                                       -> Int
    441. builtin.Float.sin                             : Float
                                                       -> Float
    442. builtin.Float.sinh                            : Float
                                                       -> Float
    443. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    444. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    445. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    446. builtin.List.size                             : [a]
                                                       -> Nat
    447. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    448. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    449. builtin.Text.size                             : Text
                                                       -> Nat
    450. builtin.Text.patterns.space                   : Pattern
                                                         Text
    451. builtin.Float.sqrt                            : Float
                                                       -> Float
    452. builtin.io2.IO.process.start                  : Text
                                                       -> [Text]
                                                       ->{IO} ( Handle,
                                                         Handle,
                                                         Handle,
                                                         ProcessHandle)
    453. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    454. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    455. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    456. builtin.Char.Class.symbol                     : Class
    457. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    458. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    459. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    460. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    461. builtin.Float.tan                             : Float
                                                       -> Float
    462. builtin.Float.tanh                            : Float
                                                       -> Float
    463. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    464. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    465. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    466. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    467. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    468. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    469. builtin.Int.toFloat                           : Int
                                                       -> Float
    470. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    471. builtin.Nat.toInt                             : Nat
                                                       -> Int
    472. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    473. builtin.Text.toLowercase                      : Text
                                                       -> Text
    474. builtin.Char.toNat                            : Char
                                                       -> Nat
    475. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    476. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    477. builtin.Char.toText                           : Char
                                                       -> Text
    478. builtin.Debug.toText                          : a
                                                       -> Optional
                                                         (Either
                                                           Text
                                                           Text)
    479. builtin.Float.toText                          : Float
                                                       -> Text
    480. builtin.Handle.toText                         : Handle
                                                       -> Text
    481. builtin.Int.toText                            : Int
                                                       -> Text
    482. builtin.Nat.toText                            : Nat
                                                       -> Text
    483. builtin.Socket.toText                         : Socket
                                                       -> Text
    484. builtin.Link.Term.toText                      : Term
                                                       -> Text
    485. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    486. builtin.Text.toUppercase                      : Text
                                                       -> Text
    487. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    488. builtin.todo                                  : a -> b
    489. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    490. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    491. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    492. builtin.Float.truncate                        : Float
                                                       -> Int
    493. builtin.Int.truncate0                         : Int
                                                       -> Nat
    494. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    495. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    496. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    497. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    498. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    499. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    500. builtin.Char.Class.upper                      : Class
    501. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    502. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    503. builtin.Value.value                           : a
                                                       -> Value
    504. builtin.io2.IO.process.wait                   : ProcessHandle
                                                       ->{IO} Nat
    505. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    506. builtin.Char.Class.whitespace                 : Class
    507. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    508. builtin.io2.Promise.write                     : Promise
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    509. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    510. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    511. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    512. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    513. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    514. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    515. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    516. builtin.Nat.xor                               : Nat
                                                       -> Nat
                                                       -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized. 
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope, 
`a2.c` is chosen because although the suffixified version has fewer segments, its fully-qualified name has the fewest segments.

```ucm
.> view a b c d

  a.a : Nat
  a.a =
    use Nat +
    b + 1
  
  a.b : Nat
  a.b =
    use Nat +
    0 + 1
  
  a2.c : Nat
  a2.c = 1
  
  a2.d : Nat
  a2.d =
    use Nat +
    a2.c + 10
  
  a3.c#dcgdua2lj6 : Nat
  a3.c#dcgdua2lj6 = 2
  
  a3.d#9ivhgvhthc : Nat
  a3.d#9ivhgvhthc =
    use Nat +
    c#dcgdua2lj6 + 10

```
## Name biasing

```unison
deeply.nested.term = 
  a + 1

deeply.nested.value = 10

a = 10
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a                   : Nat
      deeply.nested.term  : Nat
      deeply.nested.value : Nat

```
```ucm
  ☝️  The namespace .biasing is empty.

.biasing> add

  ⍟ I've added these definitions:
  
    a                   : Nat
    deeply.nested.term  : Nat
    deeply.nested.value : Nat

-- Despite being saved with name `a`, 
-- the pretty printer should prefer the suffixified 'deeply.nested.value name' over the shallow 'a'.
-- It's closer to the term being printed.
.biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    value + 1

```
Add another term with `value` suffix to force longer suffixification of `deeply.nested.value`

```unison
other.value = 20
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      other.value : Nat

```
```ucm
.biasing> add

  ⍟ I've added these definitions:
  
    other.value : Nat

-- nested.value should be preferred over the shorter name `a` due to biasing
-- because `deeply.nested.value` is nearby to the term being viewed.
.biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    nested.value + 1

```
