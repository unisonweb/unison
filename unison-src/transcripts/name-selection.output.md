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
    43.  builtin type builtin.io2.Promise
    44.  builtin type builtin.Ref
    45.  builtin type builtin.Request
    46.  unique type builtin.Test.Result
    47.  unique type builtin.io2.RuntimeFailure
    48.  builtin ability builtin.io2.STM
    49.  unique type builtin.io2.STMFailure
    50.  builtin ability builtin.Scope
    51.  unique type builtin.io2.SeekMode
    52.  structural type builtin.SeqView a b
    53.  builtin type builtin.io2.Tls.ServerConfig
    54.  builtin type builtin.io2.Tls.SignedCert
    55.  builtin type builtin.io2.Socket
    56.  unique type builtin.io2.StdHandle
    57.  builtin type builtin.io2.TVar
    58.  builtin type builtin.Link.Term
    59.  builtin type builtin.Text
    60.  builtin type builtin.io2.ThreadId
    61.  builtin type builtin.io2.Ref.Ticket
    62.  builtin type builtin.io2.Clock.internals.TimeSpec
    63.  builtin type builtin.io2.Tls
    64.  unique type builtin.io2.TlsFailure
    65.  structural type builtin.Tuple a b
    66.  builtin type builtin.Link.Type
    67.  structural type builtin.Unit
    68.  builtin type builtin.Value
    69.  builtin type builtin.io2.Tls.Version
    70.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    71.  builtin.io2.IOError.AlreadyExists             : IOError
    72.  builtin.io2.FileMode.Append                   : FileMode
    73.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    74.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    75.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    76.  builtin.io2.IOError.EOF                       : IOError
    77.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    78.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    79.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    80.  builtin.io2.IOError.IllegalOperation          : IOError
    81.  builtin.IsPropagated.IsPropagated             : IsPropagated
    82.  builtin.IsTest.IsTest                         : IsTest
    83.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    84.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    85.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    86.  builtin.Doc.Link                              : Link
                                                       -> Doc
    87.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    88.  builtin.io2.IOError.NoSuchThing               : IOError
    89.  builtin.Optional.None                         : Optional
                                                         a
    90.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    91.  builtin.io2.IOError.PermissionDenied          : IOError
    92.  builtin.io2.FileMode.Read                     : FileMode
    93.  builtin.io2.FileMode.ReadWrite                : FileMode
    94.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    95.  builtin.io2.IOError.ResourceBusy              : IOError
    96.  builtin.io2.IOError.ResourceExhausted         : IOError
    97.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    98.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    99.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    100. builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    101. builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    102. builtin.Doc.Source                            : Link
                                                       -> Doc
    103. builtin.io2.StdHandle.StdErr                  : StdHandle
    104. builtin.io2.StdHandle.StdIn                   : StdHandle
    105. builtin.io2.StdHandle.StdOut                  : StdHandle
    106. builtin.Link.Term                             : Term
                                                       -> Link
    107. builtin.Link.Type                             : Type
                                                       -> Link
    108. builtin.Unit.Unit                             : ()
    109. builtin.io2.IOError.UserError                 : IOError
    110. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    111. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    112. builtin.io2.FileMode.Write                    : FileMode
    113. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    114. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    115. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    116. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    117. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    118. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    119. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    120. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    121. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    122. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    123. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    124. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    125. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    126. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    127. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    128. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    129. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    130. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    131. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    132. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    133. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    134. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    135. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    136. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    137. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    138. builtin.Any.Any                               : a
                                                       -> Any
    139. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    140. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    141. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    142. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    143. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    144. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    145. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    146. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    147. builtin.Float.abs                             : Float
                                                       -> Float
    148. builtin.Float.acos                            : Float
                                                       -> Float
    149. builtin.Float.acosh                           : Float
                                                       -> Float
    150. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    151. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    152. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    153. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    154. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    155. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    156. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    157. builtin.Float.asin                            : Float
                                                       -> Float
    158. builtin.Float.asinh                           : Float
                                                       -> Float
    159. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    160. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    161. builtin.Float.atan                            : Float
                                                       -> Float
    162. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    163. builtin.Float.atanh                           : Float
                                                       -> Float
    164. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    165. builtin.bug                                   : a -> b
    166. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    167. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    168. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    169. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    170. ┌ c#gjmq673r1v                                : Nat
    171. └ long.name.but.shortest.suffixification      : Nat
    172. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    173. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    174. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    175. builtin.Float.ceiling                         : Float
                                                       -> Int
    176. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    177. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    178. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    179. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    180. builtin.Int.complement                        : Int
                                                       -> Int
    181. builtin.Nat.complement                        : Nat
                                                       -> Nat
    182. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    183. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    184. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    185. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    186. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    187. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    188. builtin.Float.cos                             : Float
                                                       -> Float
    189. builtin.Float.cosh                            : Float
                                                       -> Float
    190. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    191. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    192. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    193. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    194. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    195. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    196. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    197. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    198. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    199. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    200. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    201. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    202. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    203. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    204. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    205. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    206. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    207. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    208. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    209. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    210. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    211. builtin.Bytes.empty                           : Bytes
    212. builtin.List.empty                            : [a]
    213. builtin.Text.empty                            : Text
    214. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    215. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    216. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    217. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    218. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    219. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    220. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    221. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    222. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    223. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    224. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    225. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    226. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    227. builtin.Float.exp                             : Float
                                                       -> Float
    228. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    229. builtin.Float.floor                           : Float
                                                       -> Int
    230. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    231. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    232. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    233. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    234. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    235. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    236. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    237. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    238. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    239. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    240. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    241. builtin.Char.fromNat                          : Nat
                                                       -> Char
    242. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    243. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    244. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    245. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    246. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    247. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    248. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    249. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    250. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    251. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    252. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    253. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    254. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    255. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    256. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    257. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    258. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    259. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    260. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    261. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    262. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    264. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    265. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    266. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    267. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    268. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    269. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    270. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    271. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    272. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    273. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    274. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    275. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    276. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    277. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    278. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    279. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    280. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    281. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    282. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    284. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    285. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    286. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    287. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    289. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    290. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    291. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    292. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    293. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    294. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    295. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    296. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    297. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    298. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    299. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    300. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    301. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    302. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    303. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    304. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    305. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    306. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    307. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    308. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    309. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    310. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    311. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    312. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    313. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    314. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    315. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    316. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    317. builtin.Int.increment                         : Int
                                                       -> Int
    318. builtin.Nat.increment                         : Nat
                                                       -> Nat
    319. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    320. builtin.Int.isEven                            : Int
                                                       -> Boolean
    321. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    322. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    323. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    324. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    325. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    326. builtin.metadata.isPropagated                 : IsPropagated
    327. builtin.metadata.isTest                       : IsTest
    328. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    329. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    330. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    331. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    332. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    333. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    334. builtin.Float.log                             : Float
                                                       -> Float
    335. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    336. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    337. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    338. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    339. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    340. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    341. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    342. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    343. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    344. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    345. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    346. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    347. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    348. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    349. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    350. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    351. builtin.Int.negate                            : Int
                                                       -> Int
    352. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    353. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    354. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    355. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    356. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    357. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    358. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    359. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    360. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    361. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    362. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    363. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    364. builtin.Int.popCount                          : Int
                                                       -> Nat
    365. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    366. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    367. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    368. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    369. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    370. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    371. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    372. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    373. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    374. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    375. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    376. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    377. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    378. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    379. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    380. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    381. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    382. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    383. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    384. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    385. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    386. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    387. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    388. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    389. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    390. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    391. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    392. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    393. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    394. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    395. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    396. builtin.io2.STM.retry                         : '{STM} a
    397. builtin.Text.reverse                          : Text
                                                       -> Text
    398. builtin.Float.round                           : Float
                                                       -> Int
    399. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    400. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    401. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    402. builtin.Code.serialize                        : Code
                                                       -> Bytes
    403. builtin.Value.serialize                       : Value
                                                       -> Bytes
    404. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    405. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    406. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    407. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    408. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    409. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    410. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    411. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    412. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    413. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    414. builtin.Int.signum                            : Int
                                                       -> Int
    415. builtin.Float.sin                             : Float
                                                       -> Float
    416. builtin.Float.sinh                            : Float
                                                       -> Float
    417. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    418. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    419. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    420. builtin.List.size                             : [a]
                                                       -> Nat
    421. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    422. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    423. builtin.Text.size                             : Text
                                                       -> Nat
    424. builtin.Text.patterns.space                   : Pattern
                                                         Text
    425. builtin.Float.sqrt                            : Float
                                                       -> Float
    426. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    427. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    428. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    429. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    430. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    431. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    432. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    433. builtin.Float.tan                             : Float
                                                       -> Float
    434. builtin.Float.tanh                            : Float
                                                       -> Float
    435. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    436. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    437. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    438. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    439. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    440. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    441. builtin.Int.toFloat                           : Int
                                                       -> Float
    442. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    443. builtin.Nat.toInt                             : Nat
                                                       -> Int
    444. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    445. builtin.Text.toLowercase                      : Text
                                                       -> Text
    446. builtin.Char.toNat                            : Char
                                                       -> Nat
    447. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    448. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    449. builtin.Char.toText                           : Char
                                                       -> Text
    450. builtin.Float.toText                          : Float
                                                       -> Text
    451. builtin.Handle.toText                         : Handle
                                                       -> Text
    452. builtin.Int.toText                            : Int
                                                       -> Text
    453. builtin.Nat.toText                            : Nat
                                                       -> Text
    454. builtin.Socket.toText                         : Socket
                                                       -> Text
    455. builtin.Link.Term.toText                      : Term
                                                       -> Text
    456. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    457. builtin.Text.toUppercase                      : Text
                                                       -> Text
    458. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    459. builtin.todo                                  : a -> b
    460. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    461. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    462. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    463. builtin.Float.truncate                        : Float
                                                       -> Int
    464. builtin.Int.truncate0                         : Int
                                                       -> Nat
    465. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    466. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    467. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    468. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    469. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    470. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    471. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    472. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    473. builtin.Value.value                           : a
                                                       -> Value
    474. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    475. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    476. builtin.io2.Promise.write                     : a
                                                       -> Promise
                                                         a
                                                       ->{IO} Boolean
    477. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    478. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    479. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    480. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    481. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    482. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    483. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    484. builtin.Nat.xor                               : Nat
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
