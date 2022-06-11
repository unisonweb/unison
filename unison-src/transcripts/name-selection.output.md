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
    130. builtin.crypto.HashAlgorithm.Sha1             : HashAlgorithm
    131. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    132. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    133. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    134. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    135. builtin.Float.abs                             : Float
                                                       -> Float
    136. builtin.Float.acos                            : Float
                                                       -> Float
    137. builtin.Float.acosh                           : Float
                                                       -> Float
    138. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    139. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    140. builtin.Float.asin                            : Float
                                                       -> Float
    141. builtin.Float.asinh                           : Float
                                                       -> Float
    142. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    143. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    144. builtin.Float.atan                            : Float
                                                       -> Float
    145. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    146. builtin.Float.atanh                           : Float
                                                       -> Float
    147. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    148. builtin.bug                                   : a -> b
    149. ┌ c#gjmq673r1v                                : Nat
    150. └ aaaa.tooManySegments                        : Nat
    151. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    152. builtin.Float.ceiling                         : Float
                                                       -> Int
    153. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    154. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    155. builtin.Int.complement                        : Int
                                                       -> Int
    156. builtin.Nat.complement                        : Nat
                                                       -> Nat
    157. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    158. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    159. builtin.Float.cos                             : Float
                                                       -> Float
    160. builtin.Float.cosh                            : Float
                                                       -> Float
    161. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    162. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    163. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    164. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    165. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    166. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    167. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    168. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    169. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    170. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    171. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    172. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    173. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    174. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    175. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    176. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    177. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    178. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    179. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    180. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    181. builtin.Bytes.empty                           : Bytes
    182. builtin.List.empty                            : [a]
    183. builtin.Text.empty                            : Text
    184. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    185. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    186. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    187. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    188. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    189. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    190. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    191. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    192. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    193. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    194. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    195. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    196. builtin.Float.exp                             : Float
                                                       -> Float
    197. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    198. builtin.Float.floor                           : Float
                                                       -> Int
    199. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    200. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    201. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    202. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    203. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    204. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    205. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    206. builtin.Char.fromNat                          : Nat
                                                       -> Char
    207. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    208. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    209. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    210. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    211. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    212. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    213. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    214. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    215. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    216. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    217. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    218. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    219. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    220. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    221. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    222. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    223. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    224. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    225. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    226. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    227. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    228. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    229. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    230. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    231. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    232. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    233. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    234. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    235. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    236. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    237. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    238. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    239. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    240. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    241. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    242. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    243. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    244. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    245. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    246. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    247. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    248. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    249. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    250. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    251. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    252. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    253. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    254. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    255. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    256. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    257. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    258. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    259. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    260. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    261. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    262. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    264. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    265. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    266. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    267. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    268. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    269. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    270. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    271. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    272. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    273. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    274. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    275. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    276. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    277. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    278. builtin.Int.increment                         : Int
                                                       -> Int
    279. builtin.Nat.increment                         : Nat
                                                       -> Nat
    280. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    281. builtin.Int.isEven                            : Int
                                                       -> Boolean
    282. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    283. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    284. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    285. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    286. builtin.metadata.isPropagated                 : IsPropagated
    287. builtin.metadata.isTest                       : IsTest
    288. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    289. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    290. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    291. builtin.Float.log                             : Float
                                                       -> Float
    292. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    293. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    294. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    295. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    296. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    297. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    298. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    299. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    300. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    301. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    302. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    303. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    304. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    305. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    306. builtin.io2.Clock.internals.monotonic         : '{IO} Either
                                                         Failure
                                                         TimeSpec
    307. builtin.Int.negate                            : Int
                                                       -> Int
    308. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    309. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    310. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    311. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    312. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    313. builtin.io2.Clock.internals.nsec              : TimeSpec
                                                       -> Nat
    314. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    315. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    316. builtin.Int.popCount                          : Int
                                                       -> Nat
    317. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    318. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    319. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    320. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    321. builtin.io2.Clock.internals.processCPUTime    : '{IO} Either
                                                         Failure
                                                         TimeSpec
    322. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    323. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    324. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    325. builtin.io2.Clock.internals.realtime          : '{IO} Either
                                                         Failure
                                                         TimeSpec
    326. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    327. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    328. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    329. builtin.io2.STM.retry                         : '{STM} a
    330. builtin.Float.round                           : Float
                                                       -> Int
    331. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    332. builtin.io2.Clock.internals.sec               : TimeSpec
                                                       -> Int
    333. builtin.Code.serialize                        : Code
                                                       -> Bytes
    334. builtin.Value.serialize                       : Value
                                                       -> Bytes
    335. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    336. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    337. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    338. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    339. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    340. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    341. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    342. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    343. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    344. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    345. builtin.Int.signum                            : Int
                                                       -> Int
    346. builtin.Float.sin                             : Float
                                                       -> Float
    347. builtin.Float.sinh                            : Float
                                                       -> Float
    348. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    349. builtin.List.size                             : [a]
                                                       -> Nat
    350. builtin.Text.size                             : Text
                                                       -> Nat
    351. builtin.Float.sqrt                            : Float
                                                       -> Float
    352. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    353. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    354. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    355. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    356. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    357. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    358. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    359. builtin.Float.tan                             : Float
                                                       -> Float
    360. builtin.Float.tanh                            : Float
                                                       -> Float
    361. builtin.io2.Clock.internals.threadCPUTime     : '{IO} Either
                                                         Failure
                                                         TimeSpec
    362. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    363. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    364. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    365. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    366. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    367. builtin.Int.toFloat                           : Int
                                                       -> Float
    368. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    369. builtin.Nat.toInt                             : Nat
                                                       -> Int
    370. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    371. builtin.Char.toNat                            : Char
                                                       -> Nat
    372. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    373. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    374. builtin.Char.toText                           : Char
                                                       -> Text
    375. builtin.Float.toText                          : Float
                                                       -> Text
    376. builtin.Handle.toText                         : Handle
                                                       -> Text
    377. builtin.Int.toText                            : Int
                                                       -> Text
    378. builtin.Nat.toText                            : Nat
                                                       -> Text
    379. builtin.Socket.toText                         : Socket
                                                       -> Text
    380. builtin.Link.Term.toText                      : Term
                                                       -> Text
    381. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    382. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    383. builtin.todo                                  : a -> b
    384. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    385. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    386. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    387. builtin.Float.truncate                        : Float
                                                       -> Int
    388. builtin.Int.truncate0                         : Int
                                                       -> Nat
    389. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    390. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    391. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    392. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    393. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    394. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    395. builtin.Value.value                           : a
                                                       -> Value
    396. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    397. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    398. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    399. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    400. builtin.Nat.xor                               : Nat
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
