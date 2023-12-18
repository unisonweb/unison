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
    49.  unique type builtin.RewriteCase a b
    50.  unique type builtin.RewriteSignature a b
    51.  unique type builtin.RewriteTerm a b
    52.  unique type builtin.Rewrites a
    53.  unique type builtin.io2.RuntimeFailure
    54.  builtin ability builtin.io2.STM
    55.  unique type builtin.io2.STMFailure
    56.  builtin ability builtin.Scope
    57.  unique type builtin.io2.SeekMode
    58.  structural type builtin.SeqView a b
    59.  builtin type builtin.io2.Tls.ServerConfig
    60.  builtin type builtin.io2.Tls.SignedCert
    61.  builtin type builtin.io2.Socket
    62.  unique type builtin.io2.StdHandle
    63.  builtin type builtin.io2.TVar
    64.  builtin type builtin.Link.Term
    65.  builtin type builtin.Text
    66.  builtin type builtin.io2.ThreadId
    67.  unique type builtin.io2.ThreadKilledFailure
    68.  builtin type builtin.io2.Ref.Ticket
    69.  builtin type builtin.io2.Clock.internals.TimeSpec
    70.  builtin type builtin.io2.Tls
    71.  unique type builtin.io2.TlsFailure
    72.  structural type builtin.Tuple a b
    73.  builtin type builtin.Link.Type
    74.  structural type builtin.Unit
    75.  builtin type builtin.Value
    76.  builtin type builtin.io2.Tls.Version
    77.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    78.  builtin.io2.IOError.AlreadyExists             : IOError
    79.  builtin.io2.FileMode.Append                   : FileMode
    80.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    81.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    82.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    83.  builtin.io2.IOError.EOF                       : IOError
    84.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    85.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    86.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    87.  builtin.io2.IOError.IllegalOperation          : IOError
    88.  builtin.IsPropagated.IsPropagated             : IsPropagated
    89.  builtin.IsTest.IsTest                         : IsTest
    90.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    91.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    92.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    93.  builtin.Doc.Link                              : Link
                                                       -> Doc
    94.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    95.  builtin.io2.IOError.NoSuchThing               : IOError
    96.  builtin.Optional.None                         : Optional
                                                         a
    97.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    98.  builtin.io2.IOError.PermissionDenied          : IOError
    99.  builtin.io2.FileMode.Read                     : FileMode
    100. builtin.io2.FileMode.ReadWrite                : FileMode
    101. builtin.io2.SeekMode.RelativeSeek             : SeekMode
    102. builtin.io2.IOError.ResourceBusy              : IOError
    103. builtin.io2.IOError.ResourceExhausted         : IOError
    104. builtin.RewriteCase.RewriteCase               : a
                                                       -> b
                                                       -> RewriteCase
                                                         a b
    105. builtin.RewriteSignature.RewriteSignature     : (a
                                                       -> b
                                                       -> ())
                                                       -> RewriteSignature
                                                         a b
    106. builtin.RewriteTerm.RewriteTerm               : a
                                                       -> b
                                                       -> RewriteTerm
                                                         a b
    107. builtin.Rewrites.Rewrites                     : a
                                                       -> Rewrites
                                                         a
    108. builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    109. builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    110. builtin.Doc.Signature                         : Term
                                                       -> Doc
    111. builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    112. builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    113. builtin.Doc.Source                            : Link
                                                       -> Doc
    114. builtin.io2.StdHandle.StdErr                  : StdHandle
    115. builtin.io2.StdHandle.StdIn                   : StdHandle
    116. builtin.io2.StdHandle.StdOut                  : StdHandle
    117. builtin.Link.Term                             : Term
                                                       -> Link
    118. builtin.Link.Type                             : Type
                                                       -> Link
    119. builtin.Unit.Unit                             : ()
    120. builtin.io2.IOError.UserError                 : IOError
    121. builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    122. builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    123. builtin.io2.FileMode.Write                    : FileMode
    124. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    125. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    126. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    127. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    128. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    129. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    130. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    131. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    132. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    133. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    134. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    135. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    136. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    137. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    138. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    139. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    140. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    141. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    142. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    143. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    144. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    145. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    146. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    147. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    148. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    149. builtin.Any.Any                               : a
                                                       -> Any
    150. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    151. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    152. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    153. builtin.crypto.HashAlgorithm.Md5              : HashAlgorithm
    154. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    155. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    156. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    157. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    158. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    159. builtin.Float.abs                             : Float
                                                       -> Float
    160. builtin.Float.acos                            : Float
                                                       -> Float
    161. builtin.Float.acosh                           : Float
                                                       -> Float
    162. builtin.Char.Class.alphanumeric               : Class
    163. builtin.Char.Class.and                        : Class
                                                       -> Class
                                                       -> Class
    164. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    165. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    166. builtin.Char.Class.any                        : Class
    167. builtin.Text.patterns.anyChar                 : Pattern
                                                         Text
    168. builtin.Char.Class.anyOf                      : [Char]
                                                       -> Class
    169. builtin.io2.IO.array                          : Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    170. builtin.Scope.array                           : Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    171. builtin.io2.IO.arrayOf                        : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    172. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    173. builtin.Float.asin                            : Float
                                                       -> Float
    174. builtin.Float.asinh                           : Float
                                                       -> Float
    175. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    176. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    177. builtin.Float.atan                            : Float
                                                       -> Float
    178. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    179. builtin.Float.atanh                           : Float
                                                       -> Float
    180. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    181. builtin.bug                                   : a -> b
    182. builtin.io2.IO.bytearray                      : Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    183. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    184. builtin.io2.IO.bytearrayOf                    : Nat
                                                       -> Nat
                                                       ->{IO} MutableByteArray
                                                         {IO}
    185. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableByteArray
                                                         (Scope
                                                           s)
    186. ┌ c#gjmq673r1v                                : Nat
    187. └ long.name.but.shortest.suffixification      : Nat
    188. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    189. builtin.io2.IO.process.call                   : Text
                                                       -> [Text]
                                                       ->{IO} Nat
    190. builtin.Pattern.capture                       : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    191. builtin.Pattern.captureAs                     : a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    192. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    193. builtin.Float.ceiling                         : Float
                                                       -> Int
    194. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    195. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    196. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    197. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       -> b
    198. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    199. builtin.Int.complement                        : Int
                                                       -> Int
    200. builtin.Nat.complement                        : Nat
                                                       -> Nat
    201. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    202. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    203. builtin.Char.Class.control                    : Class
    204. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    205. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    206. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    207. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    208. builtin.Float.cos                             : Float
                                                       -> Float
    209. builtin.Float.cosh                            : Float
                                                       -> Float
    210. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    211. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    212. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    213. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    214. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    215. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    216. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    217. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    218. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    219. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    220. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    221. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    222. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    223. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    224. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    225. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    226. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    227. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    228. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    229. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    230. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    231. builtin.Bytes.empty                           : Bytes
    232. builtin.List.empty                            : [a]
    233. builtin.Text.empty                            : Text
    234. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    235. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    236. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    237. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    238. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    239. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    240. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    241. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    242. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    243. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    244. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    245. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    246. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    247. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    248. builtin.Float.exp                             : Float
                                                       -> Float
    249. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    250. builtin.Float.floor                           : Float
                                                       -> Int
    251. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    252. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    253. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    254. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    255. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    256. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    257. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    258. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    259. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    260. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    261. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    262. builtin.Char.fromNat                          : Nat
                                                       -> Char
    263. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    264. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    265. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    266. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    267. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    268. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    269. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    270. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    271. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    272. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    273. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    274. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    275. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    276. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    277. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    278. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    279. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    280. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    281. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    282. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    285. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    286. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    287. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    288. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    289. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    290. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    291. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    292. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    293. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    294. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    295. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    296. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    297. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    298. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    299. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    300. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    301. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    302. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    303. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    304. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    305. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    306. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    307. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    308. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    309. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    310. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    311. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    312. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    313. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    315. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    316. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    317. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    318. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    319. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    320. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    321. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    322. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    323. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    324. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    325. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    326. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    327. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    328. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    329. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    330. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    331. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    332. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    333. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    334. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    335. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    336. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    337. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    338. builtin.Int.increment                         : Int
                                                       -> Int
    339. builtin.Nat.increment                         : Nat
                                                       -> Nat
    340. builtin.Bytes.indexOf                         : Bytes
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    341. builtin.Text.indexOf                          : Text
                                                       -> Text
                                                       -> Optional
                                                         Nat
    342. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    343. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    344. builtin.Int.isEven                            : Int
                                                       -> Boolean
    345. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    346. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    347. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    348. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    349. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    350. builtin.metadata.isPropagated                 : IsPropagated
    351. builtin.metadata.isTest                       : IsTest
    352. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    353. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    354. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    355. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    356. builtin.Char.Class.letter                     : Class
    357. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    358. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    359. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    360. builtin.Float.log                             : Float
                                                       -> Float
    361. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    362. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    363. builtin.Char.Class.lower                      : Class
    364. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    365. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    366. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    367. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    368. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    369. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    370. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    371. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    372. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    373. builtin.Char.Class.mark                       : Class
    374. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    375. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    376. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    377. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    378. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    379. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    380. builtin.Int.negate                            : Int
                                                       -> Int
    381. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    382. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    383. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    384. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    385. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    386. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    387. builtin.Char.Class.not                        : Class
                                                       -> Class
    388. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    389. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    390. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    391. builtin.Char.Class.number                     : Class
    392. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    393. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    394. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    395. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    396. builtin.Int.popCount                          : Int
                                                       -> Nat
    397. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    398. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    399. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    400. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    401. builtin.Char.Class.printable                  : Class
    402. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    403. builtin.Char.Class.punctuation                : Class
    404. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    405. builtin.io2.IO.randomBytes                    : Nat
                                                       ->{IO} Bytes
    406. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    407. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    408. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    409. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    410. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    411. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    412. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    413. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    414. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    415. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    416. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    417. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    418. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    419. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    420. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    421. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    422. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    423. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    424. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    425. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    426. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    427. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    428. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    429. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    430. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    431. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    432. builtin.io2.STM.retry                         : '{STM} a
    433. builtin.Text.reverse                          : Text
                                                       -> Text
    434. builtin.Float.round                           : Float
                                                       -> Int
    435. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    436. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    437. builtin.io2.sandboxLinks                      : Term
                                                       ->{IO} [Term]
    438. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    439. builtin.Char.Class.separator                  : Class
    440. builtin.Code.serialize                        : Code
                                                       -> Bytes
    441. builtin.Value.serialize                       : Value
                                                       -> Bytes
    442. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    443. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    444. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    445. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    446. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    447. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    448. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    449. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    450. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    451. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    452. builtin.Int.signum                            : Int
                                                       -> Int
    453. builtin.Float.sin                             : Float
                                                       -> Float
    454. builtin.Float.sinh                            : Float
                                                       -> Float
    455. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    456. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    457. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    458. builtin.List.size                             : [a]
                                                       -> Nat
    459. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    460. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    461. builtin.Text.size                             : Text
                                                       -> Nat
    462. builtin.Text.patterns.space                   : Pattern
                                                         Text
    463. builtin.Float.sqrt                            : Float
                                                       -> Float
    464. builtin.io2.IO.process.start                  : Text
                                                       -> [Text]
                                                       ->{IO} ( Handle,
                                                         Handle,
                                                         Handle,
                                                         ProcessHandle)
    465. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    466. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    467. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    468. builtin.Char.Class.symbol                     : Class
    469. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    470. builtin.io2.Clock.internals.systemTimeZone    : Int
                                                       ->{IO} ( Int,
                                                         Nat,
                                                         Text)
    471. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    472. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    473. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    474. builtin.Float.tan                             : Float
                                                       -> Float
    475. builtin.Float.tanh                            : Float
                                                       -> Float
    476. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    477. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    478. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    479. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    480. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    481. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    482. builtin.Int.toFloat                           : Int
                                                       -> Float
    483. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    484. builtin.Nat.toInt                             : Nat
                                                       -> Int
    485. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    486. builtin.Text.toLowercase                      : Text
                                                       -> Text
    487. builtin.Char.toNat                            : Char
                                                       -> Nat
    488. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    489. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    490. builtin.Char.toText                           : Char
                                                       -> Text
    491. builtin.Debug.toText                          : a
                                                       -> Optional
                                                         (Either
                                                           Text
                                                           Text)
    492. builtin.Float.toText                          : Float
                                                       -> Text
    493. builtin.Handle.toText                         : Handle
                                                       -> Text
    494. builtin.Int.toText                            : Int
                                                       -> Text
    495. builtin.Nat.toText                            : Nat
                                                       -> Text
    496. builtin.Socket.toText                         : Socket
                                                       -> Text
    497. builtin.Link.Term.toText                      : Term
                                                       -> Text
    498. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    499. builtin.Text.toUppercase                      : Text
                                                       -> Text
    500. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    501. builtin.todo                                  : a -> b
    502. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    503. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    504. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    505. builtin.Float.truncate                        : Float
                                                       -> Int
    506. builtin.Int.truncate0                         : Int
                                                       -> Nat
    507. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    508. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    509. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    510. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    511. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    512. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    513. builtin.Char.Class.upper                      : Class
    514. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    515. builtin.Code.validateLinks                    : [( Term,
                                                         Code)]
                                                       ->{Exception} Either
                                                         [Term]
                                                         [Term]
    516. builtin.io2.Value.validateSandboxed           : [Term]
                                                       -> Value
                                                       ->{IO} Either
                                                         [Term]
                                                         [Term]
    517. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    518. builtin.Value.value                           : a
                                                       -> Value
    519. builtin.io2.IO.process.wait                   : ProcessHandle
                                                       ->{IO} Nat
    520. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    521. builtin.Char.Class.whitespace                 : Class
    522. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    523. builtin.io2.Promise.write                     : Promise
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    524. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    525. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    526. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    527. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    528. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    529. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    530. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    531. builtin.Nat.xor                               : Nat
                                                       -> Nat
                                                       -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

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
