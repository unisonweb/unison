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
    63.  builtin type builtin.io2.Ref.Ticket
    64.  builtin type builtin.io2.Clock.internals.TimeSpec
    65.  builtin type builtin.io2.Tls
    66.  unique type builtin.io2.TlsFailure
    67.  structural type builtin.Tuple a b
    68.  builtin type builtin.Link.Type
    69.  structural type builtin.Unit
    70.  builtin type builtin.Value
    71.  builtin type builtin.io2.Tls.Version
    72.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    73.  builtin.io2.IOError.AlreadyExists             : IOError
    74.  builtin.io2.FileMode.Append                   : FileMode
    75.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    76.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    77.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    78.  builtin.io2.IOError.EOF                       : IOError
    79.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    80.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    81.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    82.  builtin.io2.IOError.IllegalOperation          : IOError
    83.  builtin.IsPropagated.IsPropagated             : IsPropagated
    84.  builtin.IsTest.IsTest                         : IsTest
    85.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    86.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    87.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    88.  builtin.Doc.Link                              : Link
                                                       -> Doc
    89.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    90.  builtin.io2.IOError.NoSuchThing               : IOError
    91.  builtin.Optional.None                         : Optional
                                                         a
    92.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    93.  builtin.io2.IOError.PermissionDenied          : IOError
    94.  builtin.io2.FileMode.Read                     : FileMode
    95.  builtin.io2.FileMode.ReadWrite                : FileMode
    96.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    97.  builtin.io2.IOError.ResourceBusy              : IOError
    98.  builtin.io2.IOError.ResourceExhausted         : IOError
    99.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    100. builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    101. builtin.Doc.Signature                         : Term
                                                       -> Doc
    102. builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    103. builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    104. builtin.Doc.Source                            : Link
                                                       -> Doc
    105. builtin.io2.StdHandle.StdErr                  : StdHandle
    106. builtin.io2.StdHandle.StdIn                   : StdHandle
    107. builtin.io2.StdHandle.StdOut                  : StdHandle
    108. builtin.Link.Term                             : Term
                                                       -> Link
    109. builtin.Link.Type                             : Type
                                                       -> Link
    110. builtin.Unit.Unit                             : ()
    111. builtin.io2.IOError.UserError                 : IOError
    112. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    113. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    114. builtin.io2.FileMode.Write                    : FileMode
    115. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    116. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    117. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    118. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    119. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    120. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    121. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    122. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    123. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    124. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    125. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    126. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    127. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    128. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    129. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    130. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    131. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    132. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    133. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    134. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    135. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    136. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    137. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    138. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    139. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    140. builtin.Any.Any                               : a
                                                       -> Any
    141. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    142. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    143. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    144. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    145. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    146. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    147. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    148. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    149. builtin.Float.abs                             : Float
                                                       -> Float
    150. builtin.Float.acos                            : Float
                                                       -> Float
    151. builtin.Float.acosh                           : Float
                                                       -> Float
    152. builtin.Char.Class.alphanumeric               : Class
    153. builtin.Char.Class.and                        : Class
                                                       -> Class
                                                       -> Class
    154. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    155. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    156. builtin.Char.Class.any                        : Class
    157. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    158. builtin.Char.Class.anyOf                      : [Char]
                                                       -> Class
    159. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    160. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    161. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    162. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    163. builtin.Float.asin                            : Float
                                                       -> Float
    164. builtin.Float.asinh                           : Float
                                                       -> Float
    165. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    166. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    167. builtin.Float.atan                            : Float
                                                       -> Float
    168. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    169. builtin.Float.atanh                           : Float
                                                       -> Float
    170. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    171. builtin.bug                                   : a -> b
    172. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    173. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    174. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    175. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    176. ┌ c#gjmq673r1v                                : Nat
    177. └ long.name.but.shortest.suffixification      : Nat
    178. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    179. builtin.io2.IO.process.call                   : Text
                                                       -> [Text]
                                                       ->{IO} Nat
    180. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    181. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    182. builtin.Float.ceiling                         : Float
                                                       -> Int
    183. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    184. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    185. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    186. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    187. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    188. builtin.Int.complement                        : Int
                                                       -> Int
    189. builtin.Nat.complement                        : Nat
                                                       -> Nat
    190. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    191. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    192. builtin.Char.Class.control                    : Class
    193. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    194. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    195. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    196. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    197. builtin.Float.cos                             : Float
                                                       -> Float
    198. builtin.Float.cosh                            : Float
                                                       -> Float
    199. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    200. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    201. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    202. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    203. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    204. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    205. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    206. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    207. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    208. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    209. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    210. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    211. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    212. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    213. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    214. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    215. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    216. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    217. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    218. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    219. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    220. builtin.Bytes.empty                           : Bytes
    221. builtin.List.empty                            : [a]
    222. builtin.Text.empty                            : Text
    223. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    224. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    225. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    226. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    227. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    228. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    229. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    230. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    231. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    232. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    233. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    234. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    235. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    236. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    237. builtin.Float.exp                             : Float
                                                       -> Float
    238. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    239. builtin.Float.floor                           : Float
                                                       -> Int
    240. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    241. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    242. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    243. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    244. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    245. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    246. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    247. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    248. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    249. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    250. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    251. builtin.Char.fromNat                          : Nat
                                                       -> Char
    252. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    253. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    254. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    255. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    256. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    257. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    258. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    259. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    260. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    261. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    262. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    263. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    264. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    265. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    266. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    267. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    268. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    269. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    270. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    271. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    272. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    273. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    274. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    275. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    276. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    277. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    278. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    279. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    280. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    281. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    282. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    283. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    284. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    285. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    286. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    287. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    288. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    289. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    290. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    291. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    292. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    293. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    294. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    295. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    296. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    297. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    298. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    299. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    300. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    301. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    302. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    303. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    305. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    306. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    307. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    308. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    309. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    310. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    311. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    312. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    313. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    314. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    315. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    316. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    317. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    318. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    319. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    320. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    321. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    322. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    323. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    324. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    325. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    326. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    327. builtin.Int.increment                         : Int
                                                       -> Int
    328. builtin.Nat.increment                         : Nat
                                                       -> Nat
    329. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    330. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    331. builtin.Int.isEven                            : Int
                                                       -> Boolean
    332. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    333. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    334. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    335. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    336. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    337. builtin.metadata.isPropagated                 : IsPropagated
    338. builtin.metadata.isTest                       : IsTest
    339. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    340. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    341. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    342. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    343. builtin.Char.Class.letter                     : Class
    344. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    345. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    346. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    347. builtin.Float.log                             : Float
                                                       -> Float
    348. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    349. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    350. builtin.Char.Class.lower                      : Class
    351. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    352. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    353. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    354. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    355. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    356. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    357. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    358. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    359. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    360. builtin.Char.Class.mark                       : Class
    361. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    362. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    363. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    364. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    365. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    366. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    367. builtin.Int.negate                            : Int
                                                       -> Int
    368. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    369. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    370. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    371. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    372. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    373. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    374. builtin.Char.Class.not                        : Class
                                                       -> Class
    375. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    376. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    377. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    378. builtin.Char.Class.number                     : Class
    379. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    380. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    381. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    382. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    383. builtin.Int.popCount                          : Int
                                                       -> Nat
    384. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    385. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    386. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    387. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    388. builtin.Char.Class.printable                  : Class
    389. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    390. builtin.Char.Class.punctuation                : Class
    391. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    392. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    393. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    394. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    395. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    396. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    397. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    398. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    399. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    400. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    401. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    402. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    403. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    404. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    405. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    406. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    407. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    408. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    409. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    410. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    411. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    412. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    413. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    414. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    415. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    416. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    417. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    418. builtin.io2.STM.retry                         : '{STM} a
    419. builtin.Text.reverse                          : Text
                                                       -> Text
    420. builtin.Float.round                           : Float
                                                       -> Int
    421. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    422. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    423. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    424. builtin.Char.Class.separator                  : Class
    425. builtin.Code.serialize                        : Code
                                                       -> Bytes
    426. builtin.Value.serialize                       : Value
                                                       -> Bytes
    427. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    428. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    429. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    430. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    431. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    432. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    433. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    434. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    435. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    436. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    437. builtin.Int.signum                            : Int
                                                       -> Int
    438. builtin.Float.sin                             : Float
                                                       -> Float
    439. builtin.Float.sinh                            : Float
                                                       -> Float
    440. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    441. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    442. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    443. builtin.List.size                             : [a]
                                                       -> Nat
    444. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    445. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    446. builtin.Text.size                             : Text
                                                       -> Nat
    447. builtin.Code.serialize.small                  : Code
                                                       -> Bytes
    448. builtin.Value.serialize.small                 : Value
                                                       -> Bytes
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
