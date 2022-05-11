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
    8.   builtin type builtin.Boolean
    9.   unique type builtin.io2.BufferMode
    10.  builtin type builtin.Bytes
    11.  builtin type builtin.Char
    12.  builtin type builtin.io2.Tls.Cipher
    13.  builtin type builtin.io2.Tls.ClientConfig
    14.  builtin type builtin.Code
    15.  unique type builtin.Doc
    16.  structural type builtin.Either a b
    17.  structural ability builtin.Exception
    18.  unique type builtin.io2.Failure
    19.  unique type builtin.io2.FileMode
    20.  builtin type builtin.Float
    21.  builtin type builtin.io2.Handle
    22.  builtin type builtin.crypto.HashAlgorithm
    23.  builtin ability builtin.io2.IO
    24.  unique type builtin.io2.IOError
    25.  unique type builtin.io2.IOFailure
    26.  builtin type builtin.Int
    27.  unique type builtin.IsPropagated
    28.  unique type builtin.IsTest
    29.  unique type builtin.Link
    30.  builtin type builtin.List
    31.  builtin type builtin.io2.MVar
    32.  builtin type builtin.Nat
    33.  structural type builtin.Optional a
    34.  builtin type builtin.io2.Tls.PrivateKey
    35.  builtin type builtin.Ref
    36.  builtin type builtin.Request
    37.  unique type builtin.Test.Result
    38.  builtin ability builtin.io2.STM
    39.  builtin ability builtin.Scope
    40.  unique type builtin.io2.SeekMode
    41.  structural type builtin.SeqView a b
    42.  builtin type builtin.io2.Tls.ServerConfig
    43.  builtin type builtin.io2.Tls.SignedCert
    44.  builtin type builtin.io2.Socket
    45.  unique type builtin.io2.StdHandle
    46.  builtin type builtin.io2.TVar
    47.  builtin type builtin.Link.Term
    48.  builtin type builtin.Text
    49.  builtin type builtin.io2.ThreadId
    50.  builtin type builtin.io2.Clock.internals.TimeSpec
    51.  builtin type builtin.io2.Tls
    52.  unique type builtin.io2.TlsFailure
    53.  structural type builtin.Tuple a b
    54.  builtin type builtin.Link.Type
    55.  structural type builtin.Unit
    56.  builtin type builtin.Value
    57.  builtin type builtin.io2.Tls.Version
    58.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    59.  builtin.io2.IOError.AlreadyExists             : IOError
    60.  builtin.io2.FileMode.Append                   : FileMode
    61.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    62.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    63.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    64.  builtin.io2.IOError.EOF                       : IOError
    65.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    66.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    67.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    68.  builtin.io2.IOError.IllegalOperation          : IOError
    69.  builtin.IsPropagated.IsPropagated             : IsPropagated
    70.  builtin.IsTest.IsTest                         : IsTest
    71.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    72.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    73.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    74.  builtin.Doc.Link                              : Link
                                                       -> Doc
    75.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    76.  builtin.io2.IOError.NoSuchThing               : IOError
    77.  builtin.Optional.None                         : Optional
                                                         a
    78.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    79.  builtin.io2.IOError.PermissionDenied          : IOError
    80.  builtin.io2.FileMode.Read                     : FileMode
    81.  builtin.io2.FileMode.ReadWrite                : FileMode
    82.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    83.  builtin.io2.IOError.ResourceBusy              : IOError
    84.  builtin.io2.IOError.ResourceExhausted         : IOError
    85.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    86.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    87.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    88.  builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    89.  builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    90.  builtin.Doc.Source                            : Link
                                                       -> Doc
    91.  builtin.io2.StdHandle.StdErr                  : StdHandle
    92.  builtin.io2.StdHandle.StdIn                   : StdHandle
    93.  builtin.io2.StdHandle.StdOut                  : StdHandle
    94.  builtin.Link.Term                             : Term
                                                       -> Link
    95.  builtin.Link.Type                             : Type
                                                       -> Link
    96.  builtin.Unit.Unit                             : ()
    97.  builtin.io2.IOError.UserError                 : IOError
    98.  builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    99.  builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    100. builtin.io2.FileMode.Write                    : FileMode
    101. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    102. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    103. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    104. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    105. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    106. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    107. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    108. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    109. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    110. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    111. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    112. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    113. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    114. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    115. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    116. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    117. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    118. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    119. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    120. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    121. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    122. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    123. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    124. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    125. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    126. builtin.Any.Any                               : a
                                                       -> Any
    127. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    128. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    129. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    130. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    131. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    132. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    133. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    134. builtin.Float.abs                             : Float
                                                       -> Float
    135. builtin.Float.acos                            : Float
                                                       -> Float
    136. builtin.Float.acosh                           : Float
                                                       -> Float
    137. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    138. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    139. builtin.Float.asin                            : Float
                                                       -> Float
    140. builtin.Float.asinh                           : Float
                                                       -> Float
    141. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    142. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    143. builtin.Float.atan                            : Float
                                                       -> Float
    144. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    145. builtin.Float.atanh                           : Float
                                                       -> Float
    146. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    147. builtin.bug                                   : a -> b
    148. ┌ c#gjmq673r1v                                : Nat
    149. └ aaaa.tooManySegments                        : Nat
    150. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    151. builtin.Float.ceiling                         : Float
                                                       -> Int
    152. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    153. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    154. builtin.Int.complement                        : Int
                                                       -> Int
    155. builtin.Nat.complement                        : Nat
                                                       -> Nat
    156. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    157. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    158. builtin.Float.cos                             : Float
                                                       -> Float
    159. builtin.Float.cosh                            : Float
                                                       -> Float
    160. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    161. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    162. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    163. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    164. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    165. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    166. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    167. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    168. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    169. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    170. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    171. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    172. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    173. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    174. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    175. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    176. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    177. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    178. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    179. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    180. builtin.Bytes.empty                           : Bytes
    181. builtin.List.empty                            : [a]
    182. builtin.Text.empty                            : Text
    183. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    184. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    185. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    186. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    187. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    188. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    189. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    190. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    191. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    192. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    193. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    194. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    195. builtin.Float.exp                             : Float
                                                       -> Float
    196. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    197. builtin.Float.floor                           : Float
                                                       -> Int
    198. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    199. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    200. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    201. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    202. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    203. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    204. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    205. builtin.Char.fromNat                          : Nat
                                                       -> Char
    206. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    207. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    208. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    209. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    210. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    211. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    212. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    213. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    214. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    215. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    216. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    217. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    218. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    219. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    220. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    221. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    222. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    223. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    224. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    225. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    226. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    227. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    228. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    229. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    230. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    231. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    232. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    233. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    234. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    235. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    236. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    237. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    238. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    239. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    240. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    241. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    242. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    243. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    244. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    245. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    246. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    247. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    248. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    249. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    250. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    251. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    252. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    253. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    254. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    255. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    256. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    257. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    258. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    259. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    260. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    261. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    262. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    264. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    265. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    266. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    267. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    268. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    269. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    270. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    271. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    272. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    273. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    274. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    275. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    276. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    277. builtin.Int.increment                         : Int
                                                       -> Int
    278. builtin.Nat.increment                         : Nat
                                                       -> Nat
    279. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    280. builtin.Int.isEven                            : Int
                                                       -> Boolean
    281. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    282. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    283. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    284. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    285. builtin.metadata.isPropagated                 : IsPropagated
    286. builtin.metadata.isTest                       : IsTest
    287. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    288. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    289. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    290. builtin.Float.log                             : Float
                                                       -> Float
    291. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    292. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    293. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    294. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    295. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    296. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    297. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    298. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    299. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    300. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    301. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    302. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    303. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    304. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    305. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    306. builtin.Int.negate                            : Int
                                                       -> Int
    307. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    308. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    309. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    310. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    311. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    312. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    313. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    314. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    315. builtin.Int.popCount                          : Int
                                                       -> Nat
    316. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    317. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    318. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    319. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    320. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    321. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    322. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    323. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    324. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    325. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    326. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    327. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    328. builtin.io2.STM.retry                         : '{STM} a
    329. builtin.Float.round                           : Float
                                                       -> Int
    330. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    331. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    332. builtin.Code.serialize                        : Code
                                                       -> Bytes
    333. builtin.Value.serialize                       : Value
                                                       -> Bytes
    334. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    335. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    336. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    337. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    338. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    339. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    340. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    341. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    342. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    343. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    344. builtin.Int.signum                            : Int
                                                       -> Int
    345. builtin.Float.sin                             : Float
                                                       -> Float
    346. builtin.Float.sinh                            : Float
                                                       -> Float
    347. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    348. builtin.List.size                             : [a]
                                                       -> Nat
    349. builtin.Text.size                             : Text
                                                       -> Nat
    350. builtin.Float.sqrt                            : Float
                                                       -> Float
    351. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    352. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    353. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    354. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    355. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    356. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    357. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    358. builtin.Float.tan                             : Float
                                                       -> Float
    359. builtin.Float.tanh                            : Float
                                                       -> Float
    360. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    361. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    362. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    363. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    364. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    365. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    366. builtin.Int.toFloat                           : Int
                                                       -> Float
    367. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    368. builtin.Nat.toInt                             : Nat
                                                       -> Int
    369. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    370. builtin.Char.toNat                            : Char
                                                       -> Nat
    371. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    372. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    373. builtin.Char.toText                           : Char
                                                       -> Text
    374. builtin.Float.toText                          : Float
                                                       -> Text
    375. builtin.Handle.toText                         : Handle
                                                       -> Text
    376. builtin.Int.toText                            : Int
                                                       -> Text
    377. builtin.Nat.toText                            : Nat
                                                       -> Text
    378. builtin.Socket.toText                         : Socket
                                                       -> Text
    379. builtin.Link.Term.toText                      : Term
                                                       -> Text
    380. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    381. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    382. builtin.todo                                  : a -> b
    383. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    384. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    385. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    386. builtin.Float.truncate                        : Float
                                                       -> Int
    387. builtin.Int.truncate0                         : Int
                                                       -> Nat
    388. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    389. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    390. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    391. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    392. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    393. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    394. builtin.Value.value                           : a
                                                       -> Value
    395. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    396. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    397. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    398. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    399. builtin.Nat.xor                               : Nat
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
