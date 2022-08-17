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

  a : ##Nat
  a = ##Nat.+ b 1

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
    39.  builtin type builtin.Pattern
    40.  builtin type builtin.io2.Tls.PrivateKey
    41.  builtin type builtin.Ref
    42.  builtin type builtin.Request
    43.  unique type builtin.Test.Result
    44.  builtin ability builtin.io2.STM
    45.  builtin ability builtin.Scope
    46.  unique type builtin.io2.SeekMode
    47.  structural type builtin.SeqView a b
    48.  builtin type builtin.io2.Tls.ServerConfig
    49.  builtin type builtin.io2.Tls.SignedCert
    50.  builtin type builtin.io2.Socket
    51.  unique type builtin.io2.StdHandle
    52.  builtin type builtin.io2.TVar
    53.  builtin type builtin.Link.Term
    54.  builtin type builtin.Text
    55.  builtin type builtin.io2.ThreadId
    56.  builtin type builtin.io2.Clock.internals.TimeSpec
    57.  builtin type builtin.io2.Tls
    58.  unique type builtin.io2.TlsFailure
    59.  structural type builtin.Tuple a b
    60.  builtin type builtin.Link.Type
    61.  structural type builtin.Unit
    62.  builtin type builtin.Value
    63.  builtin type builtin.io2.Tls.Version
    64.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    65.  builtin.io2.IOError.AlreadyExists             : IOError
    66.  builtin.io2.FileMode.Append                   : FileMode
    67.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    68.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    69.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    70.  builtin.io2.IOError.EOF                       : IOError
    71.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    72.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    73.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    74.  builtin.io2.IOError.IllegalOperation          : IOError
    75.  builtin.IsPropagated.IsPropagated             : IsPropagated
    76.  builtin.IsTest.IsTest                         : IsTest
    77.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    78.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    79.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    80.  builtin.Doc.Link                              : Link
                                                       -> Doc
    81.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    82.  builtin.io2.IOError.NoSuchThing               : IOError
    83.  builtin.Optional.None                         : Optional
                                                         a
    84.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    85.  builtin.io2.IOError.PermissionDenied          : IOError
    86.  builtin.io2.FileMode.Read                     : FileMode
    87.  builtin.io2.FileMode.ReadWrite                : FileMode
    88.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    89.  builtin.io2.IOError.ResourceBusy              : IOError
    90.  builtin.io2.IOError.ResourceExhausted         : IOError
    91.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    92.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    93.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    94.  builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    95.  builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    96.  builtin.Doc.Source                            : Link
                                                       -> Doc
    97.  builtin.io2.StdHandle.StdErr                  : StdHandle
    98.  builtin.io2.StdHandle.StdIn                   : StdHandle
    99.  builtin.io2.StdHandle.StdOut                  : StdHandle
    100. builtin.Link.Term                             : Term
                                                       -> Link
    101. builtin.Link.Type                             : Type
                                                       -> Link
    102. builtin.Unit.Unit                             : ()
    103. builtin.io2.IOError.UserError                 : IOError
    104. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    105. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    106. builtin.io2.FileMode.Write                    : FileMode
    107. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    108. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    109. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    110. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    111. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    112. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    113. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    114. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    115. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    116. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    117. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    118. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    119. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    120. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    121. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    122. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    123. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    124. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    125. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    126. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    127. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    128. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    129. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    130. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    131. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    132. builtin.Any.Any                               : a
                                                       -> Any
    133. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    134. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    135. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    136. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    137. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    138. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    139. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    140. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    141. builtin.Float.abs                             : Float
                                                       -> Float
    142. builtin.Float.acos                            : Float
                                                       -> Float
    143. builtin.Float.acosh                           : Float
                                                       -> Float
    144. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    145. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    146. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    147. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    148. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    149. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    150. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    151. builtin.Float.asin                            : Float
                                                       -> Float
    152. builtin.Float.asinh                           : Float
                                                       -> Float
    153. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    154. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    155. builtin.Float.atan                            : Float
                                                       -> Float
    156. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    157. builtin.Float.atanh                           : Float
                                                       -> Float
    158. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    159. builtin.bug                                   : a -> b
    160. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    161. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    162. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    163. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    164. ┌ c#gjmq673r1v                                : Nat
    165. └ long.name.but.shortest.suffixification      : Nat
    166. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    167. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    168. builtin.Float.ceiling                         : Float
                                                       -> Int
    169. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    170. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    171. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    172. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    173. builtin.Int.complement                        : Int
                                                       -> Int
    174. builtin.Nat.complement                        : Nat
                                                       -> Nat
    175. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    176. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    177. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    178. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    179. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    180. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    181. builtin.Float.cos                             : Float
                                                       -> Float
    182. builtin.Float.cosh                            : Float
                                                       -> Float
    183. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    184. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    185. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    186. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    187. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    188. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    189. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    190. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    191. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    192. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    193. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    194. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    195. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    196. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    197. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    198. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    199. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    200. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    201. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    202. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    203. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    204. builtin.Bytes.empty                           : Bytes
    205. builtin.List.empty                            : [a]
    206. builtin.Text.empty                            : Text
    207. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    208. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    209. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    210. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    211. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    212. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    213. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    214. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    215. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    216. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    217. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    218. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    219. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    220. builtin.Float.exp                             : Float
                                                       -> Float
    221. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    222. builtin.Float.floor                           : Float
                                                       -> Int
    223. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    224. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    225. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    226. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    227. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    228. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    229. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    230. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    231. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    232. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    233. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    234. builtin.Char.fromNat                          : Nat
                                                       -> Char
    235. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    236. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    237. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    238. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    239. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    240. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    241. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    242. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    243. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    244. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    245. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    246. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    247. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    248. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    249. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    250. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    251. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    252. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    253. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    254. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    255. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    256. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    257. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    258. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    259. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    260. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    261. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    262. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    263. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    264. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    265. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    266. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    267. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    268. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    269. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    270. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    271. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    272. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    273. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    275. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    276. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    277. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    278. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    279. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    280. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    281. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    282. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    283. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    285. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    286. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    287. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    289. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    290. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    291. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    292. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    293. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    294. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    295. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    296. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    297. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    298. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    299. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    300. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    301. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    302. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    303. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    305. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    306. builtin.Int.increment                         : Int
                                                       -> Int
    307. builtin.Nat.increment                         : Nat
                                                       -> Nat
    308. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    309. builtin.Int.isEven                            : Int
                                                       -> Boolean
    310. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    311. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    312. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    313. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    314. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    315. builtin.metadata.isPropagated                 : IsPropagated
    316. builtin.metadata.isTest                       : IsTest
    317. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    318. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    319. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    320. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    321. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    322. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    323. builtin.Float.log                             : Float
                                                       -> Float
    324. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    325. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    326. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    327. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    328. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    329. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    330. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    331. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    332. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    333. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    334. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    335. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    336. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    337. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    338. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    339. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    340. builtin.Int.negate                            : Int
                                                       -> Int
    341. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    342. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    343. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    344. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    345. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    346. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    347. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    348. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    349. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    350. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    351. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    352. builtin.Int.popCount                          : Int
                                                       -> Nat
    353. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    354. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    355. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    356. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    357. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    358. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    359. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    360. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    361. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    362. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    363. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    364. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    365. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    366. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    367. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    368. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    369. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    370. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    371. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    372. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    373. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    374. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    375. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    376. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    377. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    378. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    379. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    380. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    381. builtin.io2.STM.retry                         : '{STM} a
    382. builtin.Text.reverse                          : Text
                                                       -> Text
    383. builtin.Float.round                           : Float
                                                       -> Int
    384. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    385. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    386. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    387. builtin.Code.serialize                        : Code
                                                       -> Bytes
    388. builtin.Value.serialize                       : Value
                                                       -> Bytes
    389. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    390. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    391. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    392. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    393. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    394. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    395. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    396. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    397. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    398. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    399. builtin.Int.signum                            : Int
                                                       -> Int
    400. builtin.Float.sin                             : Float
                                                       -> Float
    401. builtin.Float.sinh                            : Float
                                                       -> Float
    402. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    403. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    404. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    405. builtin.List.size                             : [a]
                                                       -> Nat
    406. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    407. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    408. builtin.Text.size                             : Text
                                                       -> Nat
    409. builtin.Text.patterns.space                   : Pattern
                                                         Text
    410. builtin.Float.sqrt                            : Float
                                                       -> Float
    411. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    412. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    413. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    414. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    415. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    416. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    417. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    418. builtin.Float.tan                             : Float
                                                       -> Float
    419. builtin.Float.tanh                            : Float
                                                       -> Float
    420. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    421. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    422. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    423. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    424. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    425. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    426. builtin.Int.toFloat                           : Int
                                                       -> Float
    427. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    428. builtin.Nat.toInt                             : Nat
                                                       -> Int
    429. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    430. builtin.Text.toLowercase                      : Text
                                                       -> Text
    431. builtin.Char.toNat                            : Char
                                                       -> Nat
    432. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    433. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    434. builtin.Char.toText                           : Char
                                                       -> Text
    435. builtin.Float.toText                          : Float
                                                       -> Text
    436. builtin.Handle.toText                         : Handle
                                                       -> Text
    437. builtin.Int.toText                            : Int
                                                       -> Text
    438. builtin.Nat.toText                            : Nat
                                                       -> Text
    439. builtin.Socket.toText                         : Socket
                                                       -> Text
    440. builtin.Link.Term.toText                      : Term
                                                       -> Text
    441. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    442. builtin.Text.toUppercase                      : Text
                                                       -> Text
    443. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    444. builtin.todo                                  : a -> b
    445. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    446. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    447. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    448. builtin.Float.truncate                        : Float
                                                       -> Int
    449. builtin.Int.truncate0                         : Int
                                                       -> Nat
    450. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    451. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    452. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    453. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    454. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    455. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    456. builtin.Value.value                           : a
                                                       -> Value
    457. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    458. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    459. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    460. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    461. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    462. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    463. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    464. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    465. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    466. builtin.Nat.xor                               : Nat
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

  deeply.nested.term : ##Nat
  deeply.nested.term = ##Nat.+ value 1

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

  deeply.nested.term : ##Nat
  deeply.nested.term = ##Nat.+ nested.value 1

```
