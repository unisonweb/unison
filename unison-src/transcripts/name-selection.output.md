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
    191. builtin.io2.Ref.cas                           : Ref
                                                         {IO} a
                                                       -> Ticket
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    192. builtin.Float.ceiling                         : Float
                                                       -> Int
    193. builtin.Text.patterns.char                    : Class
                                                       -> Pattern
                                                         Text
    194. builtin.Text.patterns.charIn                  : [Char]
                                                       -> Pattern
                                                         Text
    195. builtin.Text.patterns.charRange               : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    196. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       -> b
    197. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    198. builtin.Int.complement                        : Int
                                                       -> Int
    199. builtin.Nat.complement                        : Nat
                                                       -> Nat
    200. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    201. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    202. builtin.Char.Class.control                    : Class
    203. builtin.ImmutableArray.copyTo!                : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> ImmutableArray
                                                         a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    204. builtin.ImmutableByteArray.copyTo!            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> ImmutableByteArray
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    205. builtin.MutableArray.copyTo!                  : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    206. builtin.MutableByteArray.copyTo!              : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    207. builtin.Float.cos                             : Float
                                                       -> Float
    208. builtin.Float.cosh                            : Float
                                                       -> Float
    209. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    210. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    211. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    212. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    213. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    214. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    215. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    216. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    217. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    218. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    219. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    220. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    221. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    222. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    223. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    224. builtin.Text.patterns.digit                   : Pattern
                                                         Text
    225. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    226. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    227. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    228. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    229. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    230. builtin.Bytes.empty                           : Bytes
    231. builtin.List.empty                            : [a]
    232. builtin.Text.empty                            : Text
    233. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    234. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    235. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    236. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    237. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    238. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    239. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    240. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    241. builtin.Text.patterns.eof                     : Pattern
                                                         Text
    242. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    243. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    244. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    245. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    246. builtin.io2.IO.process.exitCode               : ProcessHandle
                                                       ->{IO} Optional
                                                         Nat
    247. builtin.Float.exp                             : Float
                                                       -> Float
    248. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    249. builtin.Float.floor                           : Float
                                                       -> Int
    250. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    251. builtin.MutableArray.freeze                   : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableArray
                                                         a
    252. builtin.MutableByteArray.freeze               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g} ImmutableByteArray
    253. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    254. builtin.MutableByteArray.freeze!              : MutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    255. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    256. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    257. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    258. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    259. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    260. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    261. builtin.Char.fromNat                          : Nat
                                                       -> Char
    262. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    263. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    264. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    265. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    266. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    267. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    268. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    269. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    270. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    271. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    272. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    273. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    274. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    275. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    276. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    277. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    278. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    279. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    280. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    281. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    282. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    283. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    284. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    285. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    286. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    287. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    288. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    289. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    290. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    291. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    292. builtin.io2.IO.getChar.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Char
    293. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    294. builtin.io2.IO.getEcho.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    295. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    296. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    297. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    298. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    299. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    300. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    301. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    302. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    303. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    304. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    305. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    306. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    307. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    308. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    309. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    310. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    311. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    312. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    313. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    314. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    315. builtin.io2.IO.ready.impl                     : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    316. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    317. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    318. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    319. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    320. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    321. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    322. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    323. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    324. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    325. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    326. builtin.io2.IO.setEcho.impl                   : Handle
                                                       -> Boolean
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    327. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    328. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    329. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    330. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    331. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    332. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    333. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    334. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    335. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    336. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    337. builtin.Int.increment                         : Int
                                                       -> Int
    338. builtin.Nat.increment                         : Nat
                                                       -> Nat
    339. builtin.Bytes.indexOf                         : Bytes
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    340. builtin.Text.indexOf                          : Text
                                                       -> Text
                                                       -> Optional
                                                         Nat
    341. builtin.Char.Class.is                         : Class
                                                       -> Char
                                                       -> Boolean
    342. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    343. builtin.Int.isEven                            : Int
                                                       -> Boolean
    344. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    345. builtin.Pattern.isMatch                       : Pattern
                                                         a
                                                       -> a
                                                       -> Boolean
    346. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    347. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    348. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    349. builtin.metadata.isPropagated                 : IsPropagated
    350. builtin.metadata.isTest                       : IsTest
    351. builtin.Pattern.join                          : [Pattern
                                                         a]
                                                       -> Pattern
                                                         a
    352. builtin.io2.IO.process.kill                   : ProcessHandle
                                                       ->{IO} ()
    353. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    354. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    355. builtin.Char.Class.letter                     : Class
    356. builtin.Text.patterns.letter                  : Pattern
                                                         Text
    357. builtin.Text.patterns.literal                 : Text
                                                       -> Pattern
                                                         Text
    358. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    359. builtin.Float.log                             : Float
                                                       -> Float
    360. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    361. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    362. builtin.Char.Class.lower                      : Class
    363. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    364. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    365. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    366. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    367. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    368. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    369. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    370. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    371. builtin.Pattern.many                          : Pattern
                                                         a
                                                       -> Pattern
                                                         a
    372. builtin.Char.Class.mark                       : Class
    373. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    374. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    375. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    376. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    377. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    378. builtin.Universal.murmurHash                  : a
                                                       -> Nat
    379. builtin.Int.negate                            : Int
                                                       -> Int
    380. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    381. builtin.io2.Promise.new                       : '{IO} Promise
                                                         a
    382. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    383. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    384. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    385. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    386. builtin.Char.Class.not                        : Class
                                                       -> Class
    387. builtin.Text.patterns.notCharIn               : [Char]
                                                       -> Pattern
                                                         Text
    388. builtin.Text.patterns.notCharRange            : Char
                                                       -> Char
                                                       -> Pattern
                                                         Text
    389. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    390. builtin.Char.Class.number                     : Class
    391. builtin.Char.Class.or                         : Class
                                                       -> Class
                                                       -> Class
    392. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    393. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    394. builtin.Pattern.or                            : Pattern
                                                         a
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    395. builtin.Int.popCount                          : Int
                                                       -> Nat
    396. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    397. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    398. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    399. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    400. builtin.Char.Class.printable                  : Class
    401. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    402. builtin.Char.Class.punctuation                : Class
    403. builtin.Text.patterns.punctuation             : Pattern
                                                         Text
    404. builtin.io2.IO.randomBytes                    : Nat
                                                       ->{IO} Bytes
    405. builtin.Char.Class.range                      : Char
                                                       -> Char
                                                       -> Class
    406. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    407. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    408. builtin.io2.Promise.read                      : Promise
                                                         a
                                                       ->{IO} a
    409. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    410. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    411. builtin.io2.Ref.Ticket.read                   : Ticket
                                                         a
                                                       -> a
    412. builtin.ImmutableByteArray.read16be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    413. builtin.MutableByteArray.read16be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    414. builtin.ImmutableByteArray.read24be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    415. builtin.MutableByteArray.read24be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    416. builtin.ImmutableByteArray.read32be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    417. builtin.MutableByteArray.read32be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    418. builtin.ImmutableByteArray.read40be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    419. builtin.MutableByteArray.read40be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    420. builtin.ImmutableByteArray.read64be           : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    421. builtin.MutableByteArray.read64be             : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    422. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    423. builtin.MutableByteArray.read8                : MutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    424. builtin.io2.Ref.readForCas                    : Ref
                                                         {IO} a
                                                       ->{IO} Ticket
                                                         a
    425. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    426. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    427. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    428. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    429. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    430. builtin.Pattern.replicate                     : Nat
                                                       -> Nat
                                                       -> Pattern
                                                         a
                                                       -> Pattern
                                                         a
    431. builtin.io2.STM.retry                         : '{STM} a
    432. builtin.Text.reverse                          : Text
                                                       -> Text
    433. builtin.Float.round                           : Float
                                                       -> Int
    434. builtin.Pattern.run                           : Pattern
                                                         a
                                                       -> a
                                                       -> Optional
                                                         ( [a],
                                                           a)
    435. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    436. builtin.io2.sandboxLinks                      : Term
                                                       ->{IO} [Term]
    437. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    438. builtin.Char.Class.separator                  : Class
    439. builtin.Code.serialize                        : Code
                                                       -> Bytes
    440. builtin.Value.serialize                       : Value
                                                       -> Bytes
    441. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    442. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    443. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    444. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    445. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    446. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    447. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    448. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    449. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    450. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    451. builtin.Int.signum                            : Int
                                                       -> Int
    452. builtin.Float.sin                             : Float
                                                       -> Float
    453. builtin.Float.sinh                            : Float
                                                       -> Float
    454. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    455. builtin.ImmutableArray.size                   : ImmutableArray
                                                         a
                                                       -> Nat
    456. builtin.ImmutableByteArray.size               : ImmutableByteArray
                                                       -> Nat
    457. builtin.List.size                             : [a]
                                                       -> Nat
    458. builtin.MutableArray.size                     : MutableArray
                                                         g a
                                                       -> Nat
    459. builtin.MutableByteArray.size                 : MutableByteArray
                                                         g
                                                       -> Nat
    460. builtin.Text.size                             : Text
                                                       -> Nat
    461. builtin.Text.patterns.space                   : Pattern
                                                         Text
    462. builtin.Float.sqrt                            : Float
                                                       -> Float
    463. builtin.io2.IO.process.start                  : Text
                                                       -> [Text]
                                                       ->{IO} ( Handle,
                                                         Handle,
                                                         Handle,
                                                         ProcessHandle)
    464. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    465. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    466. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    467. builtin.Char.Class.symbol                     : Class
    468. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    469. builtin.io2.Clock.internals.systemTimeZone    : Int
                                                       ->{IO} ( Int,
                                                         Nat,
                                                         Text)
    470. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    471. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    472. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    473. builtin.Float.tan                             : Float
                                                       -> Float
    474. builtin.Float.tanh                            : Float
                                                       -> Float
    475. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    476. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    477. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    478. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    479. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    480. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    481. builtin.Int.toFloat                           : Int
                                                       -> Float
    482. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    483. builtin.Nat.toInt                             : Nat
                                                       -> Int
    484. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    485. builtin.Text.toLowercase                      : Text
                                                       -> Text
    486. builtin.Char.toNat                            : Char
                                                       -> Nat
    487. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    488. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    489. builtin.Char.toText                           : Char
                                                       -> Text
    490. builtin.Debug.toText                          : a
                                                       -> Optional
                                                         (Either
                                                           Text
                                                           Text)
    491. builtin.Float.toText                          : Float
                                                       -> Text
    492. builtin.Handle.toText                         : Handle
                                                       -> Text
    493. builtin.Int.toText                            : Int
                                                       -> Text
    494. builtin.Nat.toText                            : Nat
                                                       -> Text
    495. builtin.Socket.toText                         : Socket
                                                       -> Text
    496. builtin.Link.Term.toText                      : Term
                                                       -> Text
    497. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    498. builtin.Text.toUppercase                      : Text
                                                       -> Text
    499. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    500. builtin.todo                                  : a -> b
    501. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    502. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    503. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    504. builtin.Float.truncate                        : Float
                                                       -> Int
    505. builtin.Int.truncate0                         : Int
                                                       -> Nat
    506. builtin.io2.IO.tryEval                        : '{IO} a
                                                       ->{IO,
                                                       Exception} a
    507. builtin.io2.Promise.tryRead                   : Promise
                                                         a
                                                       ->{IO} Optional
                                                         a
    508. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    509. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    510. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    511. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    512. builtin.Char.Class.upper                      : Class
    513. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    514. builtin.Code.validateLinks                    : [( Term,
                                                         Code)]
                                                       ->{Exception} Either
                                                         [Term]
                                                         [Term]
    515. builtin.io2.Value.validateSandboxed           : [Term]
                                                       -> Value
                                                       ->{IO} Either
                                                         [Term]
                                                         [Term]
    516. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    517. builtin.Value.value                           : a
                                                       -> Value
    518. builtin.io2.IO.process.wait                   : ProcessHandle
                                                       ->{IO} Nat
    519. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    520. builtin.Char.Class.whitespace                 : Class
    521. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    522. builtin.io2.Promise.write                     : Promise
                                                         a
                                                       -> a
                                                       ->{IO} Boolean
    523. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    524. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    525. builtin.MutableByteArray.write16be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    526. builtin.MutableByteArray.write32be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    527. builtin.MutableByteArray.write64be            : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    528. builtin.MutableByteArray.write8               : MutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    529. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    530. builtin.Nat.xor                               : Nat
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
