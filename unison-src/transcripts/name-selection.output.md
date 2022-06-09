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
    144. builtin.IO.arrayOf                            : a
                                                       -> Nat
                                                       ->{IO} MutableArray
                                                         {IO} a
    145. builtin.Scope.arrayOf                         : a
                                                       -> Nat
                                                       ->{Scope
                                                         s} MutableArray
                                                         (Scope
                                                           s)
                                                         a
    146. builtin.Float.asin                            : Float
                                                       -> Float
    147. builtin.Float.asinh                           : Float
                                                       -> Float
    148. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    149. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    150. builtin.Float.atan                            : Float
                                                       -> Float
    151. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    152. builtin.Float.atanh                           : Float
                                                       -> Float
    153. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    154. builtin.bug                                   : a -> b
    155. builtin.IO.bytearray                          : Nat
                                                       ->{IO} ImmutableByteArray
                                                         {IO}
    156. builtin.Scope.bytearray                       : Nat
                                                       ->{Scope
                                                         s} ImmutableByteArray
                                                         (Scope
                                                           s)
    157. builtin.IO.bytearrayOf                        : Nat
                                                       -> Nat
                                                       ->{IO} ImmutableByteArray
                                                         {IO}
    158. builtin.Scope.bytearrayOf                     : Nat
                                                       -> Nat
                                                       ->{Scope
                                                         s} ImmutableByteArray
                                                         (Scope
                                                           s)
    159. ┌ c#gjmq673r1v                                : Nat
    160. └ aaaa.tooManySegments                        : Nat
    161. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    162. builtin.Float.ceiling                         : Float
                                                       -> Int
    163. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    164. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    165. builtin.Int.complement                        : Int
                                                       -> Int
    166. builtin.Nat.complement                        : Nat
                                                       -> Nat
    167. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    168. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    169. builtin.Float.cos                             : Float
                                                       -> Float
    170. builtin.Float.cosh                            : Float
                                                       -> Float
    171. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    172. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    173. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    174. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    175. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    176. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    177. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    178. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    179. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    180. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    181. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    182. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    183. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    184. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    185. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    186. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    187. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    188. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    189. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    190. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    191. builtin.Bytes.empty                           : Bytes
    192. builtin.List.empty                            : [a]
    193. builtin.Text.empty                            : Text
    194. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    195. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    196. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    197. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    198. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    199. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    200. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    201. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    202. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    203. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    204. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    205. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    206. builtin.Float.exp                             : Float
                                                       -> Float
    207. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    208. builtin.Float.floor                           : Float
                                                       -> Int
    209. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    210. builtin.MutableArray.freeze!                  : MutableArray
                                                         g a
                                                       ->{g} ImmutableArray
                                                         a
    211. builtin.MutableByteArray.freeze!              : ImmutableByteArray
                                                         g
                                                       ->{g} ImmutableByteArray
    212. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    213. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    214. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    215. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    216. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    217. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    218. builtin.Char.fromNat                          : Nat
                                                       -> Char
    219. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    220. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    221. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    222. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    223. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    224. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    225. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    226. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    227. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    228. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    229. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    230. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    231. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    232. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    233. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    234. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    235. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    236. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    237. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    238. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    239. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    240. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    241. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    242. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    243. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    244. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    245. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    246. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    247. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    248. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    249. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    250. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    251. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    252. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    253. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    254. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    255. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    256. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    257. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    258. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    259. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    260. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    261. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    262. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    264. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    265. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    266. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    267. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    268. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    269. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    270. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    271. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    272. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    273. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    275. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    276. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    277. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    278. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    279. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    280. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    281. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    282. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    283. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    284. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    285. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    286. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    287. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    288. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    289. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    290. builtin.Int.increment                         : Int
                                                       -> Int
    291. builtin.Nat.increment                         : Nat
                                                       -> Nat
    292. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    293. builtin.Int.isEven                            : Int
                                                       -> Boolean
    294. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    295. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    296. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    297. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    298. builtin.metadata.isPropagated                 : IsPropagated
    299. builtin.metadata.isTest                       : IsTest
    300. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    301. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    302. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    303. builtin.Float.log                             : Float
                                                       -> Float
    304. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    305. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    306. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    307. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    308. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    309. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    310. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    311. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    312. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    313. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    314. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    315. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    316. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    317. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    318. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    319. builtin.Int.negate                            : Int
                                                       -> Int
    320. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    321. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    322. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    323. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    324. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    325. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    326. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    327. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    328. builtin.Int.popCount                          : Int
                                                       -> Nat
    329. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    330. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    331. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    332. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    333. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    334. builtin.ImmutableArray.read                   : ImmutableArray
                                                         a
                                                       -> Nat
                                                       ->{Exception} a
    335. builtin.MutableArray.read                     : MutableArray
                                                         g a
                                                       -> Nat
                                                       ->{g,
                                                       Exception} a
    336. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    337. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    338. builtin.ImmutableByteArray.read16             : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    339. builtin.MutableByteArray.read16               : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    340. builtin.ImmutableByteArray.read32             : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    341. builtin.MutableByteArray.read32               : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    342. builtin.ImmutableByteArray.read64             : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    343. builtin.MutableByteArray.read64               : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    344. builtin.ImmutableByteArray.read8              : ImmutableByteArray
                                                       -> Nat
                                                       ->{Exception} Nat
    345. builtin.MutableByteArray.read8                : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       ->{g,
                                                       Exception} Nat
    346. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    347. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    348. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    349. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    350. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    351. builtin.io2.STM.retry                         : '{STM} a
    352. builtin.Float.round                           : Float
                                                       -> Int
    353. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    354. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    355. builtin.Code.serialize                        : Code
                                                       -> Bytes
    356. builtin.Value.serialize                       : Value
                                                       -> Bytes
    357. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    358. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    359. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    360. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    361. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    362. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    363. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    364. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    365. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    366. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    367. builtin.Int.signum                            : Int
                                                       -> Int
    368. builtin.Float.sin                             : Float
                                                       -> Float
    369. builtin.Float.sinh                            : Float
                                                       -> Float
    370. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    371. builtin.List.size                             : [a]
                                                       -> Nat
    372. builtin.Text.size                             : Text
                                                       -> Nat
    373. builtin.Float.sqrt                            : Float
                                                       -> Float
    374. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    375. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    376. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    377. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    378. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    379. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    380. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    381. builtin.Float.tan                             : Float
                                                       -> Float
    382. builtin.Float.tanh                            : Float
                                                       -> Float
    383. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    384. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    385. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    386. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    387. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    388. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    389. builtin.Int.toFloat                           : Int
                                                       -> Float
    390. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    391. builtin.Nat.toInt                             : Nat
                                                       -> Int
    392. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    393. builtin.Char.toNat                            : Char
                                                       -> Nat
    394. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    395. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    396. builtin.Char.toText                           : Char
                                                       -> Text
    397. builtin.Float.toText                          : Float
                                                       -> Text
    398. builtin.Handle.toText                         : Handle
                                                       -> Text
    399. builtin.Int.toText                            : Int
                                                       -> Text
    400. builtin.Nat.toText                            : Nat
                                                       -> Text
    401. builtin.Socket.toText                         : Socket
                                                       -> Text
    402. builtin.Link.Term.toText                      : Term
                                                       -> Text
    403. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    404. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    405. builtin.todo                                  : a -> b
    406. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    407. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    408. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    409. builtin.Float.truncate                        : Float
                                                       -> Int
    410. builtin.Int.truncate0                         : Int
                                                       -> Nat
    411. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    412. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    413. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    414. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    415. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    416. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    417. builtin.Value.value                           : a
                                                       -> Value
    418. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    419. builtin.MutableArray.write                    : MutableArray
                                                         g a
                                                       -> Nat
                                                       -> a
                                                       ->{g,
                                                       Exception} ()
    420. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    421. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    422. builtin.MutableByteArray.write16              : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    423. builtin.MutableByteArray.write32              : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    424. builtin.MutableByteArray.write64              : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    425. builtin.MutableByteArray.write8               : ImmutableByteArray
                                                         g
                                                       -> Nat
                                                       -> Nat
                                                       ->{g,
                                                       Exception} ()
    426. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    427. builtin.Nat.xor                               : Nat
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
