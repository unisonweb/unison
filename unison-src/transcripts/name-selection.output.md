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
    183. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    184. builtin.Float.ceiling                         : Float
                                                       -> Int
    185. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    186. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    187. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    188. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    189. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    190. builtin.Int.complement                        : Int
                                                       -> Int
    191. builtin.Nat.complement                        : Nat
                                                       -> Nat
    192. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    193. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    194. builtin.Char.Class.control                    : Class
    195. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    196. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    197. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    198. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    199. builtin.Float.cos                             : Float
                                                       -> Float
    200. builtin.Float.cosh                            : Float
                                                       -> Float
    201. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    202. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    203. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    204. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    205. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    206. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    207. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    208. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    209. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    210. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    211. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    212. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    213. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    214. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    215. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    216. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    217. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    218. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    219. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    220. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    221. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    222. builtin.Bytes.empty                           : Bytes
    223. builtin.List.empty                            : [a]
    224. builtin.Text.empty                            : Text
    225. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    226. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    227. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    228. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    229. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    230. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    231. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    232. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    233. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    234. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    235. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    236. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    237. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    238. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    239. builtin.Float.exp                             : Float
                                                       -> Float
    240. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    241. builtin.Float.floor                           : Float
                                                       -> Int
    242. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    243. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    244. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    245. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    246. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    247. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    248. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    249. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    250. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    251. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    252. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    253. builtin.Char.fromNat                          : Nat
                                                       -> Char
    254. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    255. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    256. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    257. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    258. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    259. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    260. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    261. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    262. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    263. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    264. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    265. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    266. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    267. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    268. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    269. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    270. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    271. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    272. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    273. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    275. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    276. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    277. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    278. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    279. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    280. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    281. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    282. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    283. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    284. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    285. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    286. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    287. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    288. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    289. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    290. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    291. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    292. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    293. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    294. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    295. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    296. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    297. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    298. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    299. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    300. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    301. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    302. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    303. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    304. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    305. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    306. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    307. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    308. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    309. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    310. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    311. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    312. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    313. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    315. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    316. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    317. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    318. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    319. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    320. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    321. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    322. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    323. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    324. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    325. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    326. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    327. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    328. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    329. builtin.Int.increment                         : Int
                                                       -> Int
    330. builtin.Nat.increment                         : Nat
                                                       -> Nat
    331. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    332. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    333. builtin.Int.isEven                            : Int
                                                       -> Boolean
    334. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    335. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    336. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    337. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    338. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    339. builtin.metadata.isPropagated                 : IsPropagated
    340. builtin.metadata.isTest                       : IsTest
    341. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    342. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    343. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    344. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    345. builtin.Char.Class.letter                     : Class
    346. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    347. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    348. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    349. builtin.Float.log                             : Float
                                                       -> Float
    350. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    351. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    352. builtin.Char.Class.lower                      : Class
    353. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    354. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    355. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    356. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    357. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    358. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    359. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    360. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    361. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    362. builtin.Char.Class.mark                       : Class
    363. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    364. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    365. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    366. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    367. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    368. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    369. builtin.Int.negate                            : Int
                                                       -> Int
    370. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    371. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    372. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    373. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    374. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    375. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    376. builtin.Char.Class.not                        : Class
                                                       -> Class
    377. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    378. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    379. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    380. builtin.Char.Class.number                     : Class
    381. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    382. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    383. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    384. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    385. builtin.Int.popCount                          : Int
                                                       -> Nat
    386. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    387. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    388. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    389. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    390. builtin.Char.Class.printable                  : Class
    391. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    392. builtin.Char.Class.punctuation                : Class
    393. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    394. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    395. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    396. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    397. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    398. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    399. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    400. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    401. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    402. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    403. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    404. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    405. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    406. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    407. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    408. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    409. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    410. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    411. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    412. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    413. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    414. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    415. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    416. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    417. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    418. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    419. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    420. builtin.io2.STM.retry                         : '{STM} a
    421. builtin.Text.reverse                          : Text
                                                       -> Text
    422. builtin.Float.round                           : Float
                                                       -> Int
    423. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    424. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    425. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    426. builtin.Char.Class.separator                  : Class
    427. builtin.Code.serialize                        : Code
                                                       -> Bytes
    428. builtin.Value.serialize                       : Value
                                                       -> Bytes
    429. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    430. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    431. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    432. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    433. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    434. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    435. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    436. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    437. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    438. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    439. builtin.Int.signum                            : Int
                                                       -> Int
    440. builtin.Float.sin                             : Float
                                                       -> Float
    441. builtin.Float.sinh                            : Float
                                                       -> Float
    442. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    443. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    444. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    445. builtin.List.size                             : [a]
                                                       -> Nat
    446. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    447. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    448. builtin.Text.size                             : Text
                                                       -> Nat
    449. builtin.Text.patterns.space                   : Pattern
                                                         Text
    450. builtin.Float.sqrt                            : Float
                                                       -> Float
    451. builtin.io2.IO.process.start                  : Text
                                                       -> [Text]
                                                       ->{IO} ( Handle,
                                                         Handle,
                                                         Handle,
                                                         ProcessHandle)
    452. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    453. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    454. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    455. builtin.Char.Class.symbol                     : Class
    456. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    457. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    458. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    459. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    460. builtin.Float.tan                             : Float
                                                       -> Float
    461. builtin.Float.tanh                            : Float
                                                       -> Float
    462. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    463. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    464. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    465. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    466. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    467. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    468. builtin.Int.toFloat                           : Int
                                                       -> Float
    469. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    470. builtin.Nat.toInt                             : Nat
                                                       -> Int
    471. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    472. builtin.Text.toLowercase                      : Text
                                                       -> Text
    473. builtin.Char.toNat                            : Char
                                                       -> Nat
    474. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    475. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    476. builtin.Char.toText                           : Char
                                                       -> Text
    477. builtin.Debug.toText                          : a
                                                       -> Optional
                                                         (Either
                                                           Text
                                                           Text)
    478. builtin.Float.toText                          : Float
                                                       -> Text
    479. builtin.Handle.toText                         : Handle
                                                       -> Text
    480. builtin.Int.toText                            : Int
                                                       -> Text
    481. builtin.Nat.toText                            : Nat
                                                       -> Text
    482. builtin.Socket.toText                         : Socket
                                                       -> Text
    483. builtin.Link.Term.toText                      : Term
                                                       -> Text
    484. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    485. builtin.Text.toUppercase                      : Text
                                                       -> Text
    486. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    487. builtin.todo                                  : a -> b
    488. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    489. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    490. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    491. builtin.Float.truncate                        : Float
                                                       -> Int
    492. builtin.Int.truncate0                         : Int
                                                       -> Nat
    493. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    494. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    495. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    496. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    497. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    498. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    499. builtin.Char.Class.upper                      : Class
    500. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    501. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    502. builtin.Value.value                           : a
                                                       -> Value
    503. builtin.io2.IO.process.wait                   : ProcessHandle
                                                       ->{IO} Nat
    504. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    505. builtin.Char.Class.whitespace                 : Class
    506. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    507. builtin.io2.Promise.write                     : Promise
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    508. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    509. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    510. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    511. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    512. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    513. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    514. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    515. builtin.Nat.xor                               : Nat
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
