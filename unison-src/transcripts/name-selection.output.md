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

.a2> alias.term c aaaa.tooManySegments

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
    8.   unique type builtin.io2.ArrayFailure
    9.   builtin type builtin.Boolean
    10.  unique type builtin.io2.BufferMode
    11.  builtin type builtin.Bytes
    12.  builtin type builtin.Char
    13.  builtin type builtin.io2.Tls.Cipher
    14.  builtin type builtin.io2.Tls.ClientConfig
    15.  builtin type builtin.Code
    16.  unique type builtin.Doc
    17.  structural type builtin.Either a b
    18.  structural ability builtin.Exception
    19.  unique type builtin.io2.Failure
    20.  unique type builtin.io2.FileMode
    21.  builtin type builtin.Float
    22.  builtin type builtin.io2.Handle
    23.  builtin type builtin.crypto.HashAlgorithm
    24.  builtin ability builtin.io2.IO
    25.  unique type builtin.io2.IOError
    26.  unique type builtin.io2.IOFailure
    27.  builtin type builtin.ImmutableArray
    28.  builtin type builtin.ImmutableByteArray
    29.  builtin type builtin.Int
    30.  unique type builtin.IsPropagated
    31.  unique type builtin.IsTest
    32.  unique type builtin.Link
    33.  builtin type builtin.List
    34.  builtin type builtin.io2.MVar
    35.  builtin type builtin.MutableArray
    36.  builtin type builtin.MutableByteArray
    37.  builtin type builtin.Nat
    38.  structural type builtin.Optional a
    39.  builtin type builtin.io2.Tls.PrivateKey
    40.  builtin type builtin.Ref
    41.  builtin type builtin.Request
    42.  unique type builtin.Test.Result
    43.  builtin ability builtin.io2.STM
    44.  builtin ability builtin.Scope
    45.  unique type builtin.io2.SeekMode
    46.  structural type builtin.SeqView a b
    47.  builtin type builtin.io2.Tls.ServerConfig
    48.  builtin type builtin.io2.Tls.SignedCert
    49.  builtin type builtin.io2.Socket
    50.  unique type builtin.io2.StdHandle
    51.  builtin type builtin.io2.TVar
    52.  builtin type builtin.Link.Term
    53.  builtin type builtin.Text
    54.  builtin type builtin.io2.ThreadId
    55.  builtin type builtin.io2.Clock.internals.TimeSpec
    56.  builtin type builtin.io2.Tls
    57.  unique type builtin.io2.TlsFailure
    58.  structural type builtin.Tuple a b
    59.  builtin type builtin.Link.Type
    60.  structural type builtin.Unit
    61.  builtin type builtin.Value
    62.  builtin type builtin.io2.Tls.Version
    63.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    64.  builtin.io2.IOError.AlreadyExists             : IOError
    65.  builtin.io2.FileMode.Append                   : FileMode
    66.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    67.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    68.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    69.  builtin.io2.IOError.EOF                       : IOError
    70.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    71.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    72.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    73.  builtin.io2.IOError.IllegalOperation          : IOError
    74.  builtin.IsPropagated.IsPropagated             : IsPropagated
    75.  builtin.IsTest.IsTest                         : IsTest
    76.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    77.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    78.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    79.  builtin.Doc.Link                              : Link
                                                       -> Doc
    80.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    81.  builtin.io2.IOError.NoSuchThing               : IOError
    82.  builtin.Optional.None                         : Optional
                                                         a
    83.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    84.  builtin.io2.IOError.PermissionDenied          : IOError
    85.  builtin.io2.FileMode.Read                     : FileMode
    86.  builtin.io2.FileMode.ReadWrite                : FileMode
    87.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    88.  builtin.io2.IOError.ResourceBusy              : IOError
    89.  builtin.io2.IOError.ResourceExhausted         : IOError
    90.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    91.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    92.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    93.  builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    94.  builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    95.  builtin.Doc.Source                            : Link
                                                       -> Doc
    96.  builtin.io2.StdHandle.StdErr                  : StdHandle
    97.  builtin.io2.StdHandle.StdIn                   : StdHandle
    98.  builtin.io2.StdHandle.StdOut                  : StdHandle
    99.  builtin.Link.Term                             : Term
                                                       -> Link
    100. builtin.Link.Type                             : Type
                                                       -> Link
    101. builtin.Unit.Unit                             : ()
    102. builtin.io2.IOError.UserError                 : IOError
    103. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    104. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    105. builtin.io2.FileMode.Write                    : FileMode
    106. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    107. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    108. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    109. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    110. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    111. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    112. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    113. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    114. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    115. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    116. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    117. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    118. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    119. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    120. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    121. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    122. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    123. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    124. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    125. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    126. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    127. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    128. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    129. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    130. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    131. builtin.Any.Any                               : a
                                                       -> Any
    132. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    133. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    134. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    135. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    136. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    137. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    138. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    139. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    140. builtin.Float.abs                             : Float
                                                       -> Float
    141. builtin.Float.acos                            : Float
                                                       -> Float
    142. builtin.Float.acosh                           : Float
                                                       -> Float
    143. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    144. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    145. builtin.Text.patterns.anyChar                 : ##Pattern
                                                         Text
    146. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    147. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    148. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    149. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    150. builtin.Float.asin                            : Float
                                                       -> Float
    151. builtin.Float.asinh                           : Float
                                                       -> Float
    152. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    153. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    154. builtin.Float.atan                            : Float
                                                       -> Float
    155. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    156. builtin.Float.atanh                           : Float
                                                       -> Float
    157. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    158. builtin.bug                                   : a -> b
    159. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    160. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    161. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    162. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    163. ┌ c#gjmq673r1v                                : Nat
    164. └ aaaa.tooManySegments                        : Nat
    165. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    166. builtin.Pattern.capture                       : ##Pattern
                                                         a
                                                       -> ##Pattern
                                                         a
    167. builtin.Float.ceiling                         : Float
                                                       -> Int
    168. builtin.Text.patterns.charIn                  : [Char]
                                                       -> ##Pattern
                                                         Text
    169. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> ##Pattern
                                                         Text
    170. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    171. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    172. builtin.Int.complement                        : Int
                                                       -> Int
    173. builtin.Nat.complement                        : Nat
                                                       -> Nat
    174. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    175. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    176. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    177. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    178. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    179. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    180. builtin.Float.cos                             : Float
                                                       -> Float
    181. builtin.Float.cosh                            : Float
                                                       -> Float
    182. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    183. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    184. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    185. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    186. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    187. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    188. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    189. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    190. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    191. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    192. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    193. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    194. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    195. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    196. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    197. builtin.Text.patterns.digit                   : ##Pattern
                                                         Text
    198. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    199. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    200. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    201. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    202. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    203. builtin.Bytes.empty                           : Bytes
    204. builtin.List.empty                            : [a]
    205. builtin.Text.empty                            : Text
    206. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    207. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    208. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    209. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    210. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    211. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    212. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    213. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    214. builtin.Text.patterns.eof                     : ##Pattern
                                                         Text
    215. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    216. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    217. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    218. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    219. builtin.Float.exp                             : Float
                                                       -> Float
    220. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    221. builtin.Float.floor                           : Float
                                                       -> Int
    222. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    223. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    224. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    225. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    226. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    227. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    228. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    229. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    230. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    231. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    232. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    233. builtin.Char.fromNat                          : Nat
                                                       -> Char
    234. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    235. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    236. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    237. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    238. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    239. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    240. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    241. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    242. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    243. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    244. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    245. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    246. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    247. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    248. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    249. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    250. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    251. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    252. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    253. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    254. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    255. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    256. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    257. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    258. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    259. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    260. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    261. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    262. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    263. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    264. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    265. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    266. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    267. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    268. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    269. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    270. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    271. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    272. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    273. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    274. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    275. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    276. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    277. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    278. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    279. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    280. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    281. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    282. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    285. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    286. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    287. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    289. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    290. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    291. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    292. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    293. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    294. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    295. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    296. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    297. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    298. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    299. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    300. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    301. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    302. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    303. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    304. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    305. builtin.Int.increment                         : Int
                                                       -> Int
    306. builtin.Nat.increment                         : Nat
                                                       -> Nat
    307. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    308. builtin.Int.isEven                            : Int
                                                       -> Boolean
    309. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    310. builtin.Pattern.isMatch                       : ##Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    311. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    312. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    313. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    314. builtin.metadata.isPropagated                 : IsPropagated
    315. builtin.metadata.isTest                       : IsTest
    316. builtin.Pattern.join                          : [##Pattern
                                                         a]
                                                       -> ##Pattern
                                                         a
    317. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    318. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    319. builtin.Text.patterns.letter                  : ##Pattern
                                                         Text
    320. builtin.Text.patterns.literal                 : Text
                                                       -> ##Pattern
                                                         Text
    321. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    322. builtin.Float.log                             : Float
                                                       -> Float
    323. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    324. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    325. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    326. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    327. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    328. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    329. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    330. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    331. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    332. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    333. builtin.Pattern.many                          : ##Pattern
                                                         a
                                                       -> ##Pattern
                                                         a
    334. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    335. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    336. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    337. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    338. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    339. builtin.Int.negate                            : Int
                                                       -> Int
    340. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    341. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    342. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    343. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    344. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    345. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> ##Pattern
                                                         Text
    346. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> ##Pattern
                                                         Text
    347. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    348. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    349. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    350. builtin.Pattern.or                            : ##Pattern
                                                         a
                                                       -> ##Pattern
                                                         a
                                                       -> ##Pattern
                                                         a
    351. builtin.Int.popCount                          : Int
                                                       -> Nat
    352. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    353. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    354. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    355. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    356. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    357. builtin.Text.patterns.punctuation             : ##Pattern
                                                         Text
    358. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    359. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    360. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    361. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    362. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    363. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    364. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    365. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    366. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    367. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    368. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    369. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    370. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    371. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    372. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    373. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    374. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    375. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    376. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    377. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    378. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    379. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> ##Pattern
                                                         a
                                                       -> ##Pattern
                                                         a
    380. builtin.io2.STM.retry                         : '{STM} a
    381. builtin.Text.reverse                          : Text
                                                       -> Text
    382. builtin.Float.round                           : Float
                                                       -> Int
    383. builtin.Pattern.run                           : ##Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    384. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    385. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    386. builtin.Code.serialize                        : Code
                                                       -> Bytes
    387. builtin.Value.serialize                       : Value
                                                       -> Bytes
    388. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    389. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    390. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    391. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    392. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    393. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    394. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    395. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    396. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    397. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    398. builtin.Int.signum                            : Int
                                                       -> Int
    399. builtin.Float.sin                             : Float
                                                       -> Float
    400. builtin.Float.sinh                            : Float
                                                       -> Float
    401. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    402. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    403. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    404. builtin.List.size                             : [a]
                                                       -> Nat
    405. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    406. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    407. builtin.Text.size                             : Text
                                                       -> Nat
    408. builtin.Text.patterns.space                   : ##Pattern
                                                         Text
    409. builtin.Float.sqrt                            : Float
                                                       -> Float
    410. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    411. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    412. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    413. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    414. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    415. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    416. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    417. builtin.Float.tan                             : Float
                                                       -> Float
    418. builtin.Float.tanh                            : Float
                                                       -> Float
    419. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    420. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    421. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    422. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    423. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    424. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    425. builtin.Int.toFloat                           : Int
                                                       -> Float
    426. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    427. builtin.Nat.toInt                             : Nat
                                                       -> Int
    428. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    429. builtin.Text.toLowercase                      : Text
                                                       -> Text
    430. builtin.Char.toNat                            : Char
                                                       -> Nat
    431. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    432. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    433. builtin.Char.toText                           : Char
                                                       -> Text
    434. builtin.Float.toText                          : Float
                                                       -> Text
    435. builtin.Handle.toText                         : Handle
                                                       -> Text
    436. builtin.Int.toText                            : Int
                                                       -> Text
    437. builtin.Nat.toText                            : Nat
                                                       -> Text
    438. builtin.Socket.toText                         : Socket
                                                       -> Text
    439. builtin.Link.Term.toText                      : Term
                                                       -> Text
    440. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    441. builtin.Text.toUppercase                      : Text
                                                       -> Text
    442. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    443. builtin.todo                                  : a -> b
    444. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    445. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    446. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    447. builtin.Float.truncate                        : Float
                                                       -> Int
    448. builtin.Int.truncate0                         : Int
                                                       -> Nat
    449. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    450. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    451. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    452. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    453. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    454. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    455. builtin.Value.value                           : a
                                                       -> Value
    456. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    457. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    458. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    459. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    460. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    461. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    462. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    463. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    464. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    465. builtin.Nat.xor                               : Nat
                                                       -> Nat
                                                       -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
At this point, `a3` is conflicted for symbols `c` and `d`, but the original `a2` namespace has an unconflicted definition for `c` and `d`, so those are preferred.

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
