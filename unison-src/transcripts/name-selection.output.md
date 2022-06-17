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
    135. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    136. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    137. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    138. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    139. builtin.Float.abs                             : Float
                                                       -> Float
    140. builtin.Float.acos                            : Float
                                                       -> Float
    141. builtin.Float.acosh                           : Float
                                                       -> Float
    142. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    143. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    144. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    145. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    146. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    147. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    148. builtin.Float.asin                            : Float
                                                       -> Float
    149. builtin.Float.asinh                           : Float
                                                       -> Float
    150. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    151. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    152. builtin.Float.atan                            : Float
                                                       -> Float
    153. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    154. builtin.Float.atanh                           : Float
                                                       -> Float
    155. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    156. builtin.bug                                   : a -> b
    157. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    158. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    159. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    160. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    161. ┌ c#gjmq673r1v                                : Nat
    162. └ aaaa.tooManySegments                        : Nat
    163. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    164. builtin.Float.ceiling                         : Float
                                                       -> Int
    165. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    166. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    167. builtin.Int.complement                        : Int
                                                       -> Int
    168. builtin.Nat.complement                        : Nat
                                                       -> Nat
    169. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    170. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    171. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    172. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    173. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    174. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    175. builtin.Float.cos                             : Float
                                                       -> Float
    176. builtin.Float.cosh                            : Float
                                                       -> Float
    177. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    178. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    179. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    180. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    181. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    182. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    183. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    184. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    185. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    186. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    187. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    188. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    189. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    190. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    191. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    192. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    193. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    194. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    195. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    196. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    197. builtin.Bytes.empty                           : Bytes
    198. builtin.List.empty                            : [a]
    199. builtin.Text.empty                            : Text
    200. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    201. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    202. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    203. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    204. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    205. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    206. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    207. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    208. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    209. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    210. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    211. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    212. builtin.Float.exp                             : Float
                                                       -> Float
    213. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    214. builtin.Float.floor                           : Float
                                                       -> Int
    215. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    216. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    217. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    218. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    219. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    220. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    221. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    222. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    223. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    224. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    225. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    226. builtin.Char.fromNat                          : Nat
                                                       -> Char
    227. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    228. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    229. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    230. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    231. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    232. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    233. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    234. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    235. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    236. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    237. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    238. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    239. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    240. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    241. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    242. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    243. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    244. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    245. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    246. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    247. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    248. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    249. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    250. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    251. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    252. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    253. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    254. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    255. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    256. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    257. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    258. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    259. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    260. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    261. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    262. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    263. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    264. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    265. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    266. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    267. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    268. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    269. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    270. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    271. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    272. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    273. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    274. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    275. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    276. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    277. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    278. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    279. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    280. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    281. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    282. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    285. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    286. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    287. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    289. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    290. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    291. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    292. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    293. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    294. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    295. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    296. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    297. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    298. builtin.Int.increment                         : Int
                                                       -> Int
    299. builtin.Nat.increment                         : Nat
                                                       -> Nat
    300. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    301. builtin.Int.isEven                            : Int
                                                       -> Boolean
    302. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    303. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    304. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    305. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    306. builtin.metadata.isPropagated                 : IsPropagated
    307. builtin.metadata.isTest                       : IsTest
    308. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    309. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    310. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    311. builtin.Float.log                             : Float
                                                       -> Float
    312. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    313. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    314. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    315. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    316. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    317. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    318. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    319. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    320. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    321. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    322. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    323. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    324. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    325. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    326. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    327. builtin.Int.negate                            : Int
                                                       -> Int
    328. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    329. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    330. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    331. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    332. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    333. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    334. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    335. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    336. builtin.Int.popCount                          : Int
                                                       -> Nat
    337. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    338. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    339. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    340. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    341. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    342. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    343. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    344. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    345. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    346. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    347. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    348. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    349. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    350. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    351. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    352. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    353. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    354. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    355. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    356. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    357. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    358. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    359. builtin.io2.STM.retry                         : '{STM} a
    360. builtin.Float.round                           : Float
                                                       -> Int
    361. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    362. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    363. builtin.Code.serialize                        : Code
                                                       -> Bytes
    364. builtin.Value.serialize                       : Value
                                                       -> Bytes
    365. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    366. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    367. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    368. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    369. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    370. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    371. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    372. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    373. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    374. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    375. builtin.Int.signum                            : Int
                                                       -> Int
    376. builtin.Float.sin                             : Float
                                                       -> Float
    377. builtin.Float.sinh                            : Float
                                                       -> Float
    378. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    379. builtin.List.size                             : [a]
                                                       -> Nat
    380. builtin.Text.size                             : Text
                                                       -> Nat
    381. builtin.Float.sqrt                            : Float
                                                       -> Float
    382. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    383. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    384. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    385. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    386. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    387. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    388. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    389. builtin.Float.tan                             : Float
                                                       -> Float
    390. builtin.Float.tanh                            : Float
                                                       -> Float
    391. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    392. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    393. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    394. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    395. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    396. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    397. builtin.Int.toFloat                           : Int
                                                       -> Float
    398. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    399. builtin.Nat.toInt                             : Nat
                                                       -> Int
    400. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    401. builtin.Char.toNat                            : Char
                                                       -> Nat
    402. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    403. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    404. builtin.Char.toText                           : Char
                                                       -> Text
    405. builtin.Float.toText                          : Float
                                                       -> Text
    406. builtin.Handle.toText                         : Handle
                                                       -> Text
    407. builtin.Int.toText                            : Int
                                                       -> Text
    408. builtin.Nat.toText                            : Nat
                                                       -> Text
    409. builtin.Socket.toText                         : Socket
                                                       -> Text
    410. builtin.Link.Term.toText                      : Term
                                                       -> Text
    411. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    412. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    413. builtin.todo                                  : a -> b
    414. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    415. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    416. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    417. builtin.Float.truncate                        : Float
                                                       -> Int
    418. builtin.Int.truncate0                         : Int
                                                       -> Nat
    419. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    420. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    421. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    422. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    423. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    424. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    425. builtin.Value.value                           : a
                                                       -> Value
    426. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    427. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    428. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    429. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    430. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    431. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    432. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    433. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    434. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    435. builtin.Nat.xor                               : Nat
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
