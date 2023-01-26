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
    43.  builtin type builtin.io2.ProcessHandle
    44.  builtin type builtin.io2.Promise
    45.  builtin type builtin.Ref
    46.  builtin type builtin.Request
    47.  unique type builtin.Test.Result
    48.  unique type builtin.io2.RuntimeFailure
    49.  builtin ability builtin.io2.STM
    50.  unique type builtin.io2.STMFailure
    51.  builtin ability builtin.Scope
    52.  unique type builtin.io2.SeekMode
    53.  structural type builtin.SeqView a b
    54.  builtin type builtin.io2.Tls.ServerConfig
    55.  builtin type builtin.io2.Tls.SignedCert
    56.  builtin type builtin.io2.Socket
    57.  unique type builtin.io2.StdHandle
    58.  builtin type builtin.io2.TVar
    59.  builtin type builtin.Link.Term
    60.  builtin type builtin.Text
    61.  builtin type builtin.io2.ThreadId
    62.  builtin type builtin.io2.Ref.Ticket
    63.  builtin type builtin.io2.Clock.internals.TimeSpec
    64.  builtin type builtin.io2.Tls
    65.  unique type builtin.io2.TlsFailure
    66.  structural type builtin.Tuple a b
    67.  builtin type builtin.Link.Type
    68.  structural type builtin.Unit
    69.  builtin type builtin.Value
    70.  builtin type builtin.io2.Tls.Version
    71.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    72.  builtin.io2.IOError.AlreadyExists             : IOError
    73.  builtin.io2.FileMode.Append                   : FileMode
    74.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    75.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    76.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    77.  builtin.io2.IOError.EOF                       : IOError
    78.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    79.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    80.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    81.  builtin.io2.IOError.IllegalOperation          : IOError
    82.  builtin.IsPropagated.IsPropagated             : IsPropagated
    83.  builtin.IsTest.IsTest                         : IsTest
    84.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    85.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    86.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    87.  builtin.Doc.Link                              : Link
                                                       -> Doc
    88.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    89.  builtin.io2.IOError.NoSuchThing               : IOError
    90.  builtin.Optional.None                         : Optional
                                                         a
    91.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    92.  builtin.io2.IOError.PermissionDenied          : IOError
    93.  builtin.io2.FileMode.Read                     : FileMode
    94.  builtin.io2.FileMode.ReadWrite                : FileMode
    95.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    96.  builtin.io2.IOError.ResourceBusy              : IOError
    97.  builtin.io2.IOError.ResourceExhausted         : IOError
    98.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    99.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    100. builtin.Doc.Signature                         : Term
                                                       -> Doc
    101. builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    102. builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    103. builtin.Doc.Source                            : Link
                                                       -> Doc
    104. builtin.io2.StdHandle.StdErr                  : StdHandle
    105. builtin.io2.StdHandle.StdIn                   : StdHandle
    106. builtin.io2.StdHandle.StdOut                  : StdHandle
    107. builtin.Link.Term                             : Term
                                                       -> Link
    108. builtin.Link.Type                             : Type
                                                       -> Link
    109. builtin.Unit.Unit                             : ()
    110. builtin.io2.IOError.UserError                 : IOError
    111. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    112. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    113. builtin.io2.FileMode.Write                    : FileMode
    114. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    115. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    116. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    117. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    118. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    119. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    120. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    121. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    122. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    123. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    124. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    125. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    126. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    127. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    128. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    129. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    130. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    131. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    132. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    133. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    134. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    135. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    136. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    137. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    138. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    139. builtin.Any.Any                               : a
                                                       -> Any
    140. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    141. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    142. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    143. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    144. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    145. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    146. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    147. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    148. builtin.Float.abs                             : Float
                                                       -> Float
    149. builtin.Float.acos                            : Float
                                                       -> Float
    150. builtin.Float.acosh                           : Float
                                                       -> Float
    151. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    152. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    153. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    154. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    155. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    156. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    157. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    158. builtin.Float.asin                            : Float
                                                       -> Float
    159. builtin.Float.asinh                           : Float
                                                       -> Float
    160. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    161. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    162. builtin.Float.atan                            : Float
                                                       -> Float
    163. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    164. builtin.Float.atanh                           : Float
                                                       -> Float
    165. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    166. builtin.bug                                   : a -> b
    167. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    168. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    169. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    170. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    171. ┌ c#gjmq673r1v                                : Nat
    172. └ long.name.but.shortest.suffixification      : Nat
    173. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    174. builtin.io2.IO.process.call                   : Text
                                                       -> [Text]
                                                       ->{IO} Nat
    175. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    176. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    177. builtin.Float.ceiling                         : Float
                                                       -> Int
    178. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    179. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    180. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    181. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    182. builtin.Int.complement                        : Int
                                                       -> Int
    183. builtin.Nat.complement                        : Nat
                                                       -> Nat
    184. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    185. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    186. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    187. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    188. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    189. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    190. builtin.Float.cos                             : Float
                                                       -> Float
    191. builtin.Float.cosh                            : Float
                                                       -> Float
    192. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    193. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    194. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    195. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    196. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    197. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    198. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    199. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    200. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    201. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    202. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    203. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    204. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    205. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    206. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    207. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    208. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    209. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    210. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    211. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    212. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    213. builtin.Bytes.empty                           : Bytes
    214. builtin.List.empty                            : [a]
    215. builtin.Text.empty                            : Text
    216. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    217. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    218. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    219. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    220. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    221. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    222. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    223. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    224. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    225. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    226. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    227. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    228. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    229. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    230. builtin.Float.exp                             : Float
                                                       -> Float
    231. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    232. builtin.Float.floor                           : Float
                                                       -> Int
    233. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    234. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    235. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    236. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    237. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    238. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    239. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    240. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    241. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    242. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    243. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    244. builtin.Char.fromNat                          : Nat
                                                       -> Char
    245. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    246. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    247. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    248. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    249. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    250. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    251. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    252. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    253. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    254. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    255. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    256. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    257. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    258. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    259. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    260. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    261. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    262. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    263. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    264. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    265. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    266. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    267. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    268. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    269. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    270. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    271. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    272. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    273. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    274. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    275. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    276. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    277. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    278. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    279. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    280. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    281. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    282. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    283. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    284. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    285. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    286. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    287. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    288. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    289. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    290. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    291. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    292. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    293. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    294. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    295. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    296. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    297. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    298. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    299. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    300. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    301. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    302. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    303. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    305. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    306. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    307. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    308. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    309. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    310. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    311. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    312. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    313. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    315. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    316. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    317. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    318. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    319. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    320. builtin.Int.increment                         : Int
                                                       -> Int
    321. builtin.Nat.increment                         : Nat
                                                       -> Nat
    322. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    323. builtin.Int.isEven                            : Int
                                                       -> Boolean
    324. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    325. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    326. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    327. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    328. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    329. builtin.metadata.isPropagated                 : IsPropagated
    330. builtin.metadata.isTest                       : IsTest
    331. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    332. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    333. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    334. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    335. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    336. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    337. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    338. builtin.Float.log                             : Float
                                                       -> Float
    339. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    340. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    341. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    342. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    343. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    344. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    345. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    346. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    347. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    348. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    349. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    350. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    351. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    352. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    353. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    354. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    355. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    356. builtin.Int.negate                            : Int
                                                       -> Int
    357. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    358. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    359. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    360. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    361. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    362. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    363. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    364. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    365. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    366. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    367. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    368. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    369. builtin.Int.popCount                          : Int
                                                       -> Nat
    370. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    371. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    372. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    373. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    374. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    375. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    376. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    377. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    378. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    379. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    380. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    381. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    382. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    383. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    384. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    385. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    386. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    387. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    388. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    389. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    390. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    391. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    392. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    393. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    394. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    395. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    396. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    397. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    398. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    399. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    400. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    401. builtin.io2.STM.retry                         : '{STM} a
    402. builtin.Text.reverse                          : Text
                                                       -> Text
    403. builtin.Float.round                           : Float
                                                       -> Int
    404. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    405. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    406. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    407. builtin.Code.serialize                        : Code
                                                       -> Bytes
    408. builtin.Value.serialize                       : Value
                                                       -> Bytes
    409. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    410. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    411. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    412. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    413. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    414. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    415. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    416. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    417. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    418. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    419. builtin.Int.signum                            : Int
                                                       -> Int
    420. builtin.Float.sin                             : Float
                                                       -> Float
    421. builtin.Float.sinh                            : Float
                                                       -> Float
    422. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    423. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    424. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    425. builtin.List.size                             : [a]
                                                       -> Nat
    426. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    427. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    428. builtin.Text.size                             : Text
                                                       -> Nat
    429. builtin.Text.patterns.space                   : Pattern
                                                         Text
    430. builtin.Float.sqrt                            : Float
                                                       -> Float
    431. builtin.io2.IO.process.start                  : Text
                                                       -> [Text]
                                                       ->{IO} ( Handle,
                                                         Handle,
                                                         Handle,
                                                         ProcessHandle)
    432. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    433. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    434. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    435. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    436. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    437. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    438. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    439. builtin.Float.tan                             : Float
                                                       -> Float
    440. builtin.Float.tanh                            : Float
                                                       -> Float
    441. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    442. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    443. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    444. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    445. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    446. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    447. builtin.Int.toFloat                           : Int
                                                       -> Float
    448. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    449. builtin.Nat.toInt                             : Nat
                                                       -> Int
    450. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    451. builtin.Text.toLowercase                      : Text
                                                       -> Text
    452. builtin.Char.toNat                            : Char
                                                       -> Nat
    453. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    454. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    455. builtin.Char.toText                           : Char
                                                       -> Text
    456. builtin.Debug.toText                          : a
                                                       -> Optional
                                                         (Either
                                                           Text
                                                           Text)
    457. builtin.Float.toText                          : Float
                                                       -> Text
    458. builtin.Handle.toText                         : Handle
                                                       -> Text
    459. builtin.Int.toText                            : Int
                                                       -> Text
    460. builtin.Nat.toText                            : Nat
                                                       -> Text
    461. builtin.Socket.toText                         : Socket
                                                       -> Text
    462. builtin.Link.Term.toText                      : Term
                                                       -> Text
    463. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    464. builtin.Text.toUppercase                      : Text
                                                       -> Text
    465. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    466. builtin.todo                                  : a -> b
    467. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    468. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    469. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    470. builtin.Float.truncate                        : Float
                                                       -> Int
    471. builtin.Int.truncate0                         : Int
                                                       -> Nat
    472. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    473. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    474. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    475. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    476. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    477. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    478. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    479. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    480. builtin.Value.value                           : a
                                                       -> Value
    481. builtin.io2.IO.process.wait                   : ProcessHandle
                                                       ->{IO} Nat
    482. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    483. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    484. builtin.io2.Promise.write                     : Promise
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    485. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    486. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    487. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    488. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    489. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    490. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    491. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    492. builtin.Nat.xor                               : Nat
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
