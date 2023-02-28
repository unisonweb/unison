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
    145. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    146. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    147. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    148. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    149. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    150. builtin.Float.abs                             : Float
                                                       -> Float
    151. builtin.Float.acos                            : Float
                                                       -> Float
    152. builtin.Float.acosh                           : Float
                                                       -> Float
    153. builtin.Char.Class.alphanumeric               : Class
    154. builtin.Char.Class.and                        : Class
                                                       -> Class
                                                       -> Class
    155. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    156. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    157. builtin.Char.Class.any                        : Class
    158. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    159. builtin.Char.Class.anyOf                      : [Char]
                                                       -> Class
    160. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    161. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    162. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    163. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    164. builtin.Float.asin                            : Float
                                                       -> Float
    165. builtin.Float.asinh                           : Float
                                                       -> Float
    166. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    167. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    168. builtin.Float.atan                            : Float
                                                       -> Float
    169. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    170. builtin.Float.atanh                           : Float
                                                       -> Float
    171. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    172. builtin.bug                                   : a -> b
    173. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    174. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    175. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    176. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    177. ┌ c#gjmq673r1v                                : Nat
    178. └ long.name.but.shortest.suffixification      : Nat
    179. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    180. builtin.io2.IO.process.call                   : Text
                                                       -> [Text]
                                                       ->{IO} Nat
    181. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    182. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    183. builtin.Float.ceiling                         : Float
                                                       -> Int
    184. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    185. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    186. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    187. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    188. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    189. builtin.Int.complement                        : Int
                                                       -> Int
    190. builtin.Nat.complement                        : Nat
                                                       -> Nat
    191. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    192. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    193. builtin.Char.Class.control                    : Class
    194. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    195. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    196. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    197. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    198. builtin.Float.cos                             : Float
                                                       -> Float
    199. builtin.Float.cosh                            : Float
                                                       -> Float
    200. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    201. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    202. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    203. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    204. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    205. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    206. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    207. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    208. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    209. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    210. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    211. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    212. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    213. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    214. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    215. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    216. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    217. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    218. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    219. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    220. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    221. builtin.Bytes.empty                           : Bytes
    222. builtin.List.empty                            : [a]
    223. builtin.Text.empty                            : Text
    224. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    225. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    226. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    227. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    228. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    229. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    230. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    231. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    232. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    233. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    234. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    235. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    236. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    237. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    238. builtin.Float.exp                             : Float
                                                       -> Float
    239. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    240. builtin.Float.floor                           : Float
                                                       -> Int
    241. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    242. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    243. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    244. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    245. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    246. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    247. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    248. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    249. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    250. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    251. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    252. builtin.Char.fromNat                          : Nat
                                                       -> Char
    253. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    254. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    255. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    256. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    257. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    258. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    259. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    260. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    261. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    262. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    263. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    264. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    265. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    266. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    267. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    268. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    269. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    270. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    271. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    272. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    273. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    275. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    276. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    277. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    278. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    279. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    280. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    281. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    282. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    283. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    284. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    285. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    286. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    287. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    288. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    289. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    290. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    291. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    292. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    293. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    294. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    295. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    296. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    297. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    298. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    299. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    300. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    301. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    302. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    303. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    305. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    306. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    307. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    308. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    309. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    310. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    311. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    312. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    313. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    315. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    316. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    317. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    318. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    319. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    320. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    321. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    322. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    323. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    324. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    325. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    326. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    327. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    328. builtin.Int.increment                         : Int
                                                       -> Int
    329. builtin.Nat.increment                         : Nat
                                                       -> Nat
    330. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    331. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    332. builtin.Int.isEven                            : Int
                                                       -> Boolean
    333. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    334. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    335. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    336. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    337. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    338. builtin.metadata.isPropagated                 : IsPropagated
    339. builtin.metadata.isTest                       : IsTest
    340. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    341. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    342. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    343. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    344. builtin.Char.Class.letter                     : Class
    345. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    346. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    347. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    348. builtin.Float.log                             : Float
                                                       -> Float
    349. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    350. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    351. builtin.Char.Class.lower                      : Class
    352. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    353. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    354. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    355. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    356. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    357. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    358. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    359. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    360. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    361. builtin.Char.Class.mark                       : Class
    362. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    363. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    364. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    365. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    366. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    367. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    368. builtin.Int.negate                            : Int
                                                       -> Int
    369. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    370. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    371. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    372. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    373. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    374. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    375. builtin.Char.Class.not                        : Class
                                                       -> Class
    376. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    377. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    378. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    379. builtin.Char.Class.number                     : Class
    380. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    381. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    382. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    383. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    384. builtin.Int.popCount                          : Int
                                                       -> Nat
    385. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    386. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    387. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    388. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    389. builtin.Char.Class.printable                  : Class
    390. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    391. builtin.Char.Class.punctuation                : Class
    392. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    393. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    394. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    395. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    396. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    397. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    398. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    399. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    400. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    401. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    402. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    403. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    404. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    405. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    406. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    407. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    408. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    409. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    410. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    411. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    412. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    413. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    414. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    415. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    416. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    417. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    418. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    419. builtin.io2.STM.retry                         : '{STM} a
    420. builtin.Text.reverse                          : Text
                                                       -> Text
    421. builtin.Float.round                           : Float
                                                       -> Int
    422. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    423. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    424. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    425. builtin.Char.Class.separator                  : Class
    426. builtin.Code.serialize                        : Code
                                                       -> Bytes
    427. builtin.Value.serialize                       : Value
                                                       -> Bytes
    428. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    429. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    430. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    431. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    432. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    433. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    434. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    435. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    436. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    437. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    438. builtin.Int.signum                            : Int
                                                       -> Int
    439. builtin.Float.sin                             : Float
                                                       -> Float
    440. builtin.Float.sinh                            : Float
                                                       -> Float
    441. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    442. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    443. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    444. builtin.List.size                             : [a]
                                                       -> Nat
    445. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    446. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    447. builtin.Text.size                             : Text
                                                       -> Nat
    448. builtin.Code.serialize.small                  : Code
                                                       -> Bytes
    449. builtin.Value.serialize.small                 : Value
                                                       -> Bytes
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
