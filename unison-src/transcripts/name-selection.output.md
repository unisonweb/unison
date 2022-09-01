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
    15.  builtin type builtin.io2.Tls.ClientConfig
    16.  builtin type builtin.Code
    17.  unique type builtin.Doc
    18.  structural type builtin.Either a b
    19.  structural ability builtin.Exception
    20.  unique type builtin.io2.Failure
    21.  unique type builtin.io2.FileMode
    22.  builtin type builtin.Float
    23.  builtin type builtin.io2.Handle
    24.  builtin type builtin.crypto.HashAlgorithm
    25.  builtin ability builtin.io2.IO
    26.  unique type builtin.io2.IOError
    27.  unique type builtin.io2.IOFailure
    28.  builtin type builtin.ImmutableArray
    29.  builtin type builtin.ImmutableByteArray
    30.  builtin type builtin.Int
    31.  unique type builtin.IsPropagated
    32.  unique type builtin.IsTest
    33.  unique type builtin.Link
    34.  builtin type builtin.List
    35.  builtin type builtin.io2.MVar
    36.  unique type builtin.io2.MiscFailure
    37.  builtin type builtin.MutableArray
    38.  builtin type builtin.MutableByteArray
    39.  builtin type builtin.Nat
    40.  structural type builtin.Optional a
    41.  builtin type builtin.Pattern
    42.  builtin type builtin.io2.Tls.PrivateKey
    43.  builtin type builtin.Ref
    44.  builtin type builtin.Request
    45.  unique type builtin.Test.Result
    46.  unique type builtin.io2.RuntimeFailure
    47.  builtin ability builtin.io2.STM
    48.  unique type builtin.io2.STMFailure
    49.  builtin ability builtin.Scope
    50.  unique type builtin.io2.SeekMode
    51.  structural type builtin.SeqView a b
    52.  builtin type builtin.io2.Tls.ServerConfig
    53.  builtin type builtin.io2.Tls.SignedCert
    54.  builtin type builtin.io2.Socket
    55.  unique type builtin.io2.StdHandle
    56.  builtin type builtin.io2.TVar
    57.  builtin type builtin.Link.Term
    58.  builtin type builtin.Text
    59.  builtin type builtin.io2.ThreadId
    60.  builtin type builtin.io2.Clock.internals.TimeSpec
    61.  builtin type builtin.io2.Tls
    62.  unique type builtin.io2.TlsFailure
    63.  structural type builtin.Tuple a b
    64.  builtin type builtin.Link.Type
    65.  structural type builtin.Unit
    66.  builtin type builtin.Value
    67.  builtin type builtin.io2.Tls.Version
    68.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    69.  builtin.io2.IOError.AlreadyExists             : IOError
    70.  builtin.io2.FileMode.Append                   : FileMode
    71.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    72.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    73.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    74.  builtin.io2.IOError.EOF                       : IOError
    75.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    76.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    77.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    78.  builtin.io2.IOError.IllegalOperation          : IOError
    79.  builtin.IsPropagated.IsPropagated             : IsPropagated
    80.  builtin.IsTest.IsTest                         : IsTest
    81.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    82.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    83.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    84.  builtin.Doc.Link                              : Link
                                                       -> Doc
    85.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    86.  builtin.io2.IOError.NoSuchThing               : IOError
    87.  builtin.Optional.None                         : Optional
                                                         a
    88.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    89.  builtin.io2.IOError.PermissionDenied          : IOError
    90.  builtin.io2.FileMode.Read                     : FileMode
    91.  builtin.io2.FileMode.ReadWrite                : FileMode
    92.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    93.  builtin.io2.IOError.ResourceBusy              : IOError
    94.  builtin.io2.IOError.ResourceExhausted         : IOError
    95.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    96.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    97.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    98.  builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    99.  builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    100. builtin.Doc.Source                            : Link
                                                       -> Doc
    101. builtin.io2.StdHandle.StdErr                  : StdHandle
    102. builtin.io2.StdHandle.StdIn                   : StdHandle
    103. builtin.io2.StdHandle.StdOut                  : StdHandle
    104. builtin.Link.Term                             : Term
                                                       -> Link
    105. builtin.Link.Type                             : Type
                                                       -> Link
    106. builtin.Unit.Unit                             : ()
    107. builtin.io2.IOError.UserError                 : IOError
    108. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    109. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    110. builtin.io2.FileMode.Write                    : FileMode
    111. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    112. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    113. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    114. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    115. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    116. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    117. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    118. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    119. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    120. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    121. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    122. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    123. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    124. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    125. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    126. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    127. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    128. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    129. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    130. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    131. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    132. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    133. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    134. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    135. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    136. builtin.Any.Any                               : a
                                                       -> Any
    137. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    138. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    139. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    140. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    141. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    142. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    143. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    144. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    145. builtin.Float.abs                             : Float
                                                       -> Float
    146. builtin.Float.acos                            : Float
                                                       -> Float
    147. builtin.Float.acosh                           : Float
                                                       -> Float
    148. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    149. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    150. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    151. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    152. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    153. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    154. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    155. builtin.Float.asin                            : Float
                                                       -> Float
    156. builtin.Float.asinh                           : Float
                                                       -> Float
    157. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    158. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    159. builtin.Float.atan                            : Float
                                                       -> Float
    160. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    161. builtin.Float.atanh                           : Float
                                                       -> Float
    162. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    163. builtin.bug                                   : a -> b
    164. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    165. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    166. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    167. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    168. ┌ c#gjmq673r1v                                : Nat
    169. └ long.name.but.shortest.suffixification      : Nat
    170. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    171. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    172. builtin.Float.ceiling                         : Float
                                                       -> Int
    173. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    174. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    175. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    176. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    177. builtin.Int.complement                        : Int
                                                       -> Int
    178. builtin.Nat.complement                        : Nat
                                                       -> Nat
    179. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    180. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    181. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    182. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    183. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    184. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    185. builtin.Float.cos                             : Float
                                                       -> Float
    186. builtin.Float.cosh                            : Float
                                                       -> Float
    187. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    188. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    189. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    190. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    191. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    192. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    193. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    194. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    195. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    196. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    197. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    198. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    199. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    200. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    201. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    202. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    203. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    204. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    205. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    206. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    207. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    208. builtin.Bytes.empty                           : Bytes
    209. builtin.List.empty                            : [a]
    210. builtin.Text.empty                            : Text
    211. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    212. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    213. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    214. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    215. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    216. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    217. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    218. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    219. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    220. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    221. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    222. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    223. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    224. builtin.Float.exp                             : Float
                                                       -> Float
    225. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    226. builtin.Float.floor                           : Float
                                                       -> Int
    227. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    228. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    229. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    230. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    231. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    232. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    233. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    234. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    235. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    236. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    237. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    238. builtin.Char.fromNat                          : Nat
                                                       -> Char
    239. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    240. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    241. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    242. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    243. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    244. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    245. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    246. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    247. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    248. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    249. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    250. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    251. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    252. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    253. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    254. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    255. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    256. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    257. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    258. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    259. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    260. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    261. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    262. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    264. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    265. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    266. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    267. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    268. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    269. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    270. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    271. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    272. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    273. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    274. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    275. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    276. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    277. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    278. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    279. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    280. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    281. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    282. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    285. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    286. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    287. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    289. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    290. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    291. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    292. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    293. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    294. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    295. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    296. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    297. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    298. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    299. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    300. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    301. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    302. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    303. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    305. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    306. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    307. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    308. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    309. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    310. builtin.Int.increment                         : Int
                                                       -> Int
    311. builtin.Nat.increment                         : Nat
                                                       -> Nat
    312. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    313. builtin.Int.isEven                            : Int
                                                       -> Boolean
    314. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    315. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    316. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    317. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    318. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    319. builtin.metadata.isPropagated                 : IsPropagated
    320. builtin.metadata.isTest                       : IsTest
    321. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    322. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    323. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    324. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    325. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    326. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    327. builtin.Float.log                             : Float
                                                       -> Float
    328. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    329. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    330. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    331. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    332. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    333. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    334. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    335. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    336. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    337. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    338. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    339. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    340. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    341. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    342. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    343. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    344. builtin.Int.negate                            : Int
                                                       -> Int
    345. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    346. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    347. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    348. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    349. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    350. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    351. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    352. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    353. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    354. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    355. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    356. builtin.Int.popCount                          : Int
                                                       -> Nat
    357. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    358. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    359. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    360. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    361. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    362. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    363. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    364. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    365. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    366. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    367. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    368. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    369. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    370. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    371. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    372. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    373. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    374. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    375. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    376. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    377. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    378. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    379. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    380. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    381. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    382. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    383. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    384. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    385. builtin.io2.STM.retry                         : '{STM} a
    386. builtin.Text.reverse                          : Text
                                                       -> Text
    387. builtin.Float.round                           : Float
                                                       -> Int
    388. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    389. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    390. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    391. builtin.Code.serialize                        : Code
                                                       -> Bytes
    392. builtin.Value.serialize                       : Value
                                                       -> Bytes
    393. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    394. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    395. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    396. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    397. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    398. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    399. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    400. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    401. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    402. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    403. builtin.Int.signum                            : Int
                                                       -> Int
    404. builtin.Float.sin                             : Float
                                                       -> Float
    405. builtin.Float.sinh                            : Float
                                                       -> Float
    406. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    407. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    408. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    409. builtin.List.size                             : [a]
                                                       -> Nat
    410. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    411. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    412. builtin.Text.size                             : Text
                                                       -> Nat
    413. builtin.Text.patterns.space                   : Pattern
                                                         Text
    414. builtin.Float.sqrt                            : Float
                                                       -> Float
    415. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    416. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    417. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    418. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    419. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    420. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    421. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    422. builtin.Float.tan                             : Float
                                                       -> Float
    423. builtin.Float.tanh                            : Float
                                                       -> Float
    424. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    425. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    426. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    427. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    428. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    429. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    430. builtin.Int.toFloat                           : Int
                                                       -> Float
    431. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    432. builtin.Nat.toInt                             : Nat
                                                       -> Int
    433. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    434. builtin.Text.toLowercase                      : Text
                                                       -> Text
    435. builtin.Char.toNat                            : Char
                                                       -> Nat
    436. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    437. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    438. builtin.Char.toText                           : Char
                                                       -> Text
    439. builtin.Float.toText                          : Float
                                                       -> Text
    440. builtin.Handle.toText                         : Handle
                                                       -> Text
    441. builtin.Int.toText                            : Int
                                                       -> Text
    442. builtin.Nat.toText                            : Nat
                                                       -> Text
    443. builtin.Socket.toText                         : Socket
                                                       -> Text
    444. builtin.Link.Term.toText                      : Term
                                                       -> Text
    445. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    446. builtin.Text.toUppercase                      : Text
                                                       -> Text
    447. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    448. builtin.todo                                  : a -> b
    449. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    450. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    451. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    452. builtin.Float.truncate                        : Float
                                                       -> Int
    453. builtin.Int.truncate0                         : Int
                                                       -> Nat
    454. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    455. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    456. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    457. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    458. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    459. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    460. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    461. builtin.Value.value                           : a
                                                       -> Value
    462. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    463. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    464. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    465. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    466. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    467. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    468. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    469. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    470. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    471. builtin.Nat.xor                               : Nat
                                                       -> Nat
                                                       -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized. 
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope, 
`long.name.but.shortest.suffixification` is chosen because its suffixified version has the fewest segments.

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
    suffixification + 10
  
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

-- nested.value should still be preferred even if the suffixification requires more segments than `a`
.biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    nested.value + 1

```
