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
    50.  builtin type builtin.io2.Tls
    51.  unique type builtin.io2.TlsFailure
    52.  structural type builtin.Tuple a b
    53.  builtin type builtin.Link.Type
    54.  structural type builtin.Unit
    55.  builtin type builtin.Value
    56.  builtin type builtin.io2.Tls.Version
    57.  builtin.io2.SeekMode.AbsoluteSeek             : SeekMode
    58.  builtin.io2.IOError.AlreadyExists             : IOError
    59.  builtin.io2.FileMode.Append                   : FileMode
    60.  builtin.Doc.Blob                              : Text
                                                       -> Doc
    61.  builtin.io2.BufferMode.BlockBuffering         : BufferMode
    62.  builtin.Tuple.Cons                            : a
                                                       -> b
                                                       -> Tuple
                                                         a b
    63.  builtin.io2.IOError.EOF                       : IOError
    64.  builtin.Doc.Evaluate                          : Term
                                                       -> Doc
    65.  builtin.Test.Result.Fail                      : Text
                                                       -> Result
    66.  builtin.io2.Failure.Failure                   : Type
                                                       -> Text
                                                       -> Any
                                                       -> Failure
    67.  builtin.io2.IOError.IllegalOperation          : IOError
    68.  builtin.IsPropagated.IsPropagated             : IsPropagated
    69.  builtin.IsTest.IsTest                         : IsTest
    70.  builtin.Doc.Join                              : [Doc]
                                                       -> Doc
    71.  builtin.Either.Left                           : a
                                                       -> Either
                                                         a b
    72.  builtin.io2.BufferMode.LineBuffering          : BufferMode
    73.  builtin.Doc.Link                              : Link
                                                       -> Doc
    74.  builtin.io2.BufferMode.NoBuffering            : BufferMode
    75.  builtin.io2.IOError.NoSuchThing               : IOError
    76.  builtin.Optional.None                         : Optional
                                                         a
    77.  builtin.Test.Result.Ok                        : Text
                                                       -> Result
    78.  builtin.io2.IOError.PermissionDenied          : IOError
    79.  builtin.io2.FileMode.Read                     : FileMode
    80.  builtin.io2.FileMode.ReadWrite                : FileMode
    81.  builtin.io2.SeekMode.RelativeSeek             : SeekMode
    82.  builtin.io2.IOError.ResourceBusy              : IOError
    83.  builtin.io2.IOError.ResourceExhausted         : IOError
    84.  builtin.Either.Right                          : b
                                                       -> Either
                                                         a b
    85.  builtin.io2.SeekMode.SeekFromEnd              : SeekMode
    86.  builtin.Doc.Signature                         : Term
                                                       -> Doc
    87.  builtin.io2.BufferMode.SizedBlockBuffering    : Nat
                                                       -> BufferMode
    88.  builtin.Optional.Some                         : a
                                                       -> Optional
                                                         a
    89.  builtin.Doc.Source                            : Link
                                                       -> Doc
    90.  builtin.io2.StdHandle.StdErr                  : StdHandle
    91.  builtin.io2.StdHandle.StdIn                   : StdHandle
    92.  builtin.io2.StdHandle.StdOut                  : StdHandle
    93.  builtin.Link.Term                             : Term
                                                       -> Link
    94.  builtin.Link.Type                             : Type
                                                       -> Link
    95.  builtin.Unit.Unit                             : ()
    96.  builtin.io2.IOError.UserError                 : IOError
    97.  builtin.SeqView.VElem                         : a
                                                       -> b
                                                       -> SeqView
                                                         a b
    98.  builtin.SeqView.VEmpty                        : SeqView
                                                         a b
    99.  builtin.io2.FileMode.Write                    : FileMode
    100. builtin.Exception.raise                       : Failure
                                                       ->{Exception} x
    101. builtin.Text.!=                               : Text
                                                       -> Text
                                                       -> Boolean
    102. builtin.Float.*                               : Float
                                                       -> Float
                                                       -> Float
    103. builtin.Int.*                                 : Int
                                                       -> Int
                                                       -> Int
    104. builtin.Nat.*                                 : Nat
                                                       -> Nat
                                                       -> Nat
    105. builtin.Float.+                               : Float
                                                       -> Float
                                                       -> Float
    106. builtin.Int.+                                 : Int
                                                       -> Int
                                                       -> Int
    107. builtin.Nat.+                                 : Nat
                                                       -> Nat
                                                       -> Nat
    108. builtin.Bytes.++                              : Bytes
                                                       -> Bytes
                                                       -> Bytes
    109. builtin.List.++                               : [a]
                                                       -> [a]
                                                       -> [a]
    110. builtin.Text.++                               : Text
                                                       -> Text
                                                       -> Text
    111. ┌ builtin.List.+:                             : a
                                                       -> [a]
                                                       -> [a]
    112. └ builtin.List.cons                           : a
                                                       -> [a]
                                                       -> [a]
    113. builtin.Float.-                               : Float
                                                       -> Float
                                                       -> Float
    114. builtin.Int.-                                 : Int
                                                       -> Int
                                                       -> Int
    115. builtin.Float./                               : Float
                                                       -> Float
                                                       -> Float
    116. builtin.Int./                                 : Int
                                                       -> Int
                                                       -> Int
    117. builtin.Nat./                                 : Nat
                                                       -> Nat
                                                       -> Nat
    118. ┌ builtin.List.:+                             : [a]
                                                       -> a
                                                       -> [a]
    119. └ builtin.List.snoc                           : [a]
                                                       -> a
                                                       -> [a]
    120. builtin.Universal.<                           : a
                                                       -> a
                                                       -> Boolean
    121. builtin.Universal.<=                          : a
                                                       -> a
                                                       -> Boolean
    122. builtin.Universal.==                          : a
                                                       -> a
                                                       -> Boolean
    123. builtin.Universal.>                           : a
                                                       -> a
                                                       -> Boolean
    124. builtin.Universal.>=                          : a
                                                       -> a
                                                       -> Boolean
    125. builtin.Any.Any                               : a
                                                       -> Any
    126. builtin.crypto.HashAlgorithm.Blake2b_256      : HashAlgorithm
    127. builtin.crypto.HashAlgorithm.Blake2b_512      : HashAlgorithm
    128. builtin.crypto.HashAlgorithm.Blake2s_256      : HashAlgorithm
    129. builtin.crypto.HashAlgorithm.Sha2_256         : HashAlgorithm
    130. builtin.crypto.HashAlgorithm.Sha2_512         : HashAlgorithm
    131. builtin.crypto.HashAlgorithm.Sha3_256         : HashAlgorithm
    132. builtin.crypto.HashAlgorithm.Sha3_512         : HashAlgorithm
    133. builtin.Float.abs                             : Float
                                                       -> Float
    134. builtin.Float.acos                            : Float
                                                       -> Float
    135. builtin.Float.acosh                           : Float
                                                       -> Float
    136. builtin.Int.and                               : Int
                                                       -> Int
                                                       -> Int
    137. builtin.Nat.and                               : Nat
                                                       -> Nat
                                                       -> Nat
    138. builtin.Float.asin                            : Float
                                                       -> Float
    139. builtin.Float.asinh                           : Float
                                                       -> Float
    140. builtin.Bytes.at                              : Nat
                                                       -> Bytes
                                                       -> Optional
                                                         Nat
    141. builtin.List.at                               : Nat
                                                       -> [a]
                                                       -> Optional
                                                         a
    142. builtin.Float.atan                            : Float
                                                       -> Float
    143. builtin.Float.atan2                           : Float
                                                       -> Float
                                                       -> Float
    144. builtin.Float.atanh                           : Float
                                                       -> Float
    145. builtin.io2.STM.atomically                    : '{STM} a
                                                       ->{IO} a
    146. builtin.bug                                   : a -> b
    147. ┌ c#gjmq673r1v                                : Nat
    148. └ aaaa.tooManySegments                        : Nat
    149. builtin.Code.cache_                           : [( Term,
                                                         Code)]
                                                       ->{IO} [Term]
    150. builtin.Float.ceiling                         : Float
                                                       -> Int
    151. builtin.unsafe.coerceAbilities                : (a
                                                       ->{e1} b)
                                                       -> a
                                                       ->{e2} b
    152. builtin.Universal.compare                     : a
                                                       -> a
                                                       -> Int
    153. builtin.Int.complement                        : Int
                                                       -> Int
    154. builtin.Nat.complement                        : Nat
                                                       -> Nat
    155. builtin.Bytes.gzip.compress                   : Bytes
                                                       -> Bytes
    156. builtin.Bytes.zlib.compress                   : Bytes
                                                       -> Bytes
    157. builtin.Float.cos                             : Float
                                                       -> Float
    158. builtin.Float.cosh                            : Float
                                                       -> Float
    159. builtin.Bytes.decodeNat16be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    160. builtin.Bytes.decodeNat16le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    161. builtin.Bytes.decodeNat32be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    162. builtin.Bytes.decodeNat32le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    163. builtin.Bytes.decodeNat64be                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    164. builtin.Bytes.decodeNat64le                   : Bytes
                                                       -> Optional
                                                         ( Nat,
                                                           Bytes)
    165. builtin.io2.Tls.decodePrivateKey              : Bytes
                                                       -> [PrivateKey]
    166. builtin.Bytes.gzip.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    167. builtin.Bytes.zlib.decompress                 : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    168. builtin.io2.Tls.ClientConfig.default          : Text
                                                       -> Bytes
                                                       -> ClientConfig
    169. builtin.io2.Tls.ServerConfig.default          : [SignedCert]
                                                       -> PrivateKey
                                                       -> ServerConfig
    170. builtin.Code.dependencies                     : Code
                                                       -> [Term]
    171. builtin.Value.dependencies                    : Value
                                                       -> [Term]
    172. builtin.Code.deserialize                      : Bytes
                                                       -> Either
                                                         Text
                                                         Code
    173. builtin.Value.deserialize                     : Bytes
                                                       -> Either
                                                         Text
                                                         Value
    174. builtin.Code.display                          : Text
                                                       -> Code
                                                       -> Text
    175. builtin.Bytes.drop                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    176. builtin.List.drop                             : Nat
                                                       -> [a]
                                                       -> [a]
    177. builtin.Nat.drop                              : Nat
                                                       -> Nat
                                                       -> Nat
    178. builtin.Text.drop                             : Nat
                                                       -> Text
                                                       -> Text
    179. builtin.Bytes.empty                           : Bytes
    180. builtin.List.empty                            : [a]
    181. builtin.Text.empty                            : Text
    182. builtin.io2.Tls.encodeCert                    : SignedCert
                                                       -> Bytes
    183. builtin.Bytes.encodeNat16be                   : Nat
                                                       -> Bytes
    184. builtin.Bytes.encodeNat16le                   : Nat
                                                       -> Bytes
    185. builtin.Bytes.encodeNat32be                   : Nat
                                                       -> Bytes
    186. builtin.Bytes.encodeNat32le                   : Nat
                                                       -> Bytes
    187. builtin.Bytes.encodeNat64be                   : Nat
                                                       -> Bytes
    188. builtin.Bytes.encodeNat64le                   : Nat
                                                       -> Bytes
    189. builtin.io2.Tls.encodePrivateKey              : PrivateKey
                                                       -> Bytes
    190. builtin.Float.eq                              : Float
                                                       -> Float
                                                       -> Boolean
    191. builtin.Int.eq                                : Int
                                                       -> Int
                                                       -> Boolean
    192. builtin.Nat.eq                                : Nat
                                                       -> Nat
                                                       -> Boolean
    193. builtin.Text.eq                               : Text
                                                       -> Text
                                                       -> Boolean
    194. builtin.Float.exp                             : Float
                                                       -> Float
    195. builtin.Bytes.flatten                         : Bytes
                                                       -> Bytes
    196. builtin.Float.floor                           : Float
                                                       -> Int
    197. builtin.io2.IO.forkComp                       : '{IO} a
                                                       ->{IO} ThreadId
    198. builtin.Bytes.fromBase16                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    199. builtin.Bytes.fromBase32                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    200. builtin.Bytes.fromBase64                      : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    201. builtin.Bytes.fromBase64UrlUnpadded           : Bytes
                                                       -> Either
                                                         Text
                                                         Bytes
    202. builtin.Text.fromCharList                     : [Char]
                                                       -> Text
    203. builtin.Bytes.fromList                        : [Nat]
                                                       -> Bytes
    204. builtin.Char.fromNat                          : Nat
                                                       -> Char
    205. builtin.Float.fromRepresentation              : Nat
                                                       -> Float
    206. builtin.Int.fromRepresentation                : Nat
                                                       -> Int
    207. builtin.Float.fromText                        : Text
                                                       -> Optional
                                                         Float
    208. builtin.Int.fromText                          : Text
                                                       -> Optional
                                                         Int
    209. builtin.Nat.fromText                          : Text
                                                       -> Optional
                                                         Nat
    210. builtin.Float.gt                              : Float
                                                       -> Float
                                                       -> Boolean
    211. builtin.Int.gt                                : Int
                                                       -> Int
                                                       -> Boolean
    212. builtin.Nat.gt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    213. builtin.Text.gt                               : Text
                                                       -> Text
                                                       -> Boolean
    214. builtin.Float.gteq                            : Float
                                                       -> Float
                                                       -> Boolean
    215. builtin.Int.gteq                              : Int
                                                       -> Int
                                                       -> Boolean
    216. builtin.Nat.gteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    217. builtin.Text.gteq                             : Text
                                                       -> Text
                                                       -> Boolean
    218. builtin.crypto.hash                           : HashAlgorithm
                                                       -> a
                                                       -> Bytes
    219. builtin.crypto.hashBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
    220. builtin.crypto.hmac                           : HashAlgorithm
                                                       -> Bytes
                                                       -> a
                                                       -> Bytes
    221. builtin.crypto.hmacBytes                      : HashAlgorithm
                                                       -> Bytes
                                                       -> Bytes
                                                       -> Bytes
    222. builtin.io2.IO.clientSocket.impl              : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    223. builtin.io2.IO.closeFile.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    224. builtin.io2.IO.closeSocket.impl               : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    225. builtin.io2.IO.createDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    226. builtin.io2.IO.createTempDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    227. builtin.io2.Tls.decodeCert.impl               : Bytes
                                                       -> Either
                                                         Failure
                                                         SignedCert
    228. builtin.io2.IO.delay.impl                     : Nat
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    229. builtin.io2.IO.directoryContents.impl         : Text
                                                       ->{IO} Either
                                                         Failure
                                                         [Text]
    230. builtin.io2.IO.fileExists.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    231. builtin.Text.fromUtf8.impl                    : Bytes
                                                       -> Either
                                                         Failure
                                                         Text
    232. builtin.io2.IO.getArgs.impl                   : '{IO} Either
                                                         Failure
                                                         [Text]
    233. builtin.io2.IO.getBuffering.impl              : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         BufferMode
    234. builtin.io2.IO.getBytes.impl                  : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    235. builtin.io2.IO.getCurrentDirectory.impl       : '{IO} Either
                                                         Failure
                                                         Text
    236. builtin.io2.IO.getEnv.impl                    : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    237. builtin.io2.IO.getFileSize.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    238. builtin.io2.IO.getFileTimestamp.impl          : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    239. builtin.io2.IO.getLine.impl                   : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Text
    240. builtin.io2.IO.getSomeBytes.impl              : Handle
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    241. builtin.io2.IO.getTempDirectory.impl          : '{IO} Either
                                                         Failure
                                                         Text
    242. builtin.io2.IO.handlePosition.impl            : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    243. builtin.io2.Tls.handshake.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    244. builtin.io2.IO.isDirectory.impl               : Text
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    245. builtin.io2.IO.isFileEOF.impl                 : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    246. builtin.io2.IO.isFileOpen.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    247. builtin.io2.IO.isSeekable.impl                : Handle
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    248. builtin.io2.IO.kill.impl                      : ThreadId
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    249. builtin.io2.IO.listen.impl                    : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    250. builtin.io2.Tls.newClient.impl                : ClientConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    251. builtin.io2.Tls.newServer.impl                : ServerConfig
                                                       -> Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Tls
    252. builtin.io2.IO.openFile.impl                  : Text
                                                       -> FileMode
                                                       ->{IO} Either
                                                         Failure
                                                         Handle
    253. builtin.io2.MVar.put.impl                     : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    254. builtin.io2.IO.putBytes.impl                  : Handle
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    255. builtin.io2.MVar.read.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    256. builtin.io2.Tls.receive.impl                  : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    257. builtin.io2.IO.removeDirectory.impl           : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    258. builtin.io2.IO.removeFile.impl                : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    259. builtin.io2.IO.renameDirectory.impl           : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    260. builtin.io2.IO.renameFile.impl                : Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    261. builtin.io2.IO.seekHandle.impl                : Handle
                                                       -> SeekMode
                                                       -> Int
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    262. builtin.io2.Tls.send.impl                     : Tls
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    263. builtin.io2.IO.serverSocket.impl              : Optional
                                                         Text
                                                       -> Text
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    264. builtin.io2.IO.setBuffering.impl              : Handle
                                                       -> BufferMode
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    265. builtin.io2.IO.setCurrentDirectory.impl       : Text
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    266. builtin.io2.IO.socketAccept.impl              : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Socket
    267. builtin.io2.IO.socketPort.impl                : Socket
                                                       ->{IO} Either
                                                         Failure
                                                         Nat
    268. builtin.io2.IO.socketReceive.impl             : Socket
                                                       -> Nat
                                                       ->{IO} Either
                                                         Failure
                                                         Bytes
    269. builtin.io2.IO.socketSend.impl                : Socket
                                                       -> Bytes
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    270. builtin.io2.MVar.swap.impl                    : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    271. builtin.io2.IO.systemTime.impl                : '{IO} Either
                                                         Failure
                                                         Nat
    272. builtin.io2.MVar.take.impl                    : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         a
    273. builtin.io2.Tls.terminate.impl                : Tls
                                                       ->{IO} Either
                                                         Failure
                                                         ()
    274. builtin.io2.MVar.tryPut.impl                  : MVar a
                                                       -> a
                                                       ->{IO} Either
                                                         Failure
                                                         Boolean
    275. builtin.io2.MVar.tryRead.impl                 : MVar a
                                                       ->{IO} Either
                                                         Failure
                                                         (Optional
                                                           a)
    276. builtin.Int.increment                         : Int
                                                       -> Int
    277. builtin.Nat.increment                         : Nat
                                                       -> Nat
    278. builtin.io2.MVar.isEmpty                      : MVar a
                                                       ->{IO} Boolean
    279. builtin.Int.isEven                            : Int
                                                       -> Boolean
    280. builtin.Nat.isEven                            : Nat
                                                       -> Boolean
    281. builtin.Code.isMissing                        : Term
                                                       ->{IO} Boolean
    282. builtin.Int.isOdd                             : Int
                                                       -> Boolean
    283. builtin.Nat.isOdd                             : Nat
                                                       -> Boolean
    284. builtin.metadata.isPropagated                 : IsPropagated
    285. builtin.metadata.isTest                       : IsTest
    286. builtin.Int.leadingZeros                      : Int
                                                       -> Nat
    287. builtin.Nat.leadingZeros                      : Nat
                                                       -> Nat
    288. builtin.Value.load                            : Value
                                                       ->{IO} Either
                                                         [Term]
                                                         a
    289. builtin.Float.log                             : Float
                                                       -> Float
    290. builtin.Float.logBase                         : Float
                                                       -> Float
                                                       -> Float
    291. builtin.Code.lookup                           : Term
                                                       ->{IO} Optional
                                                         Code
    292. builtin.Float.lt                              : Float
                                                       -> Float
                                                       -> Boolean
    293. builtin.Int.lt                                : Int
                                                       -> Int
                                                       -> Boolean
    294. builtin.Nat.lt                                : Nat
                                                       -> Nat
                                                       -> Boolean
    295. builtin.Text.lt                               : Text
                                                       -> Text
                                                       -> Boolean
    296. builtin.Float.lteq                            : Float
                                                       -> Float
                                                       -> Boolean
    297. builtin.Int.lteq                              : Int
                                                       -> Int
                                                       -> Boolean
    298. builtin.Nat.lteq                              : Nat
                                                       -> Nat
                                                       -> Boolean
    299. builtin.Text.lteq                             : Text
                                                       -> Text
                                                       -> Boolean
    300. builtin.Float.max                             : Float
                                                       -> Float
                                                       -> Float
    301. builtin.Float.min                             : Float
                                                       -> Float
                                                       -> Float
    302. builtin.Int.mod                               : Int
                                                       -> Int
                                                       -> Int
    303. builtin.Nat.mod                               : Nat
                                                       -> Nat
                                                       -> Nat
    304. builtin.Int.negate                            : Int
                                                       -> Int
    305. builtin.io2.MVar.new                          : a
                                                       ->{IO} MVar
                                                         a
    306. builtin.io2.TVar.new                          : a
                                                       ->{STM} TVar
                                                         a
    307. builtin.io2.MVar.newEmpty                     : '{IO} MVar
                                                         a
    308. builtin.io2.TVar.newIO                        : a
                                                       ->{IO} TVar
                                                         a
    309. builtin.Boolean.not                           : Boolean
                                                       -> Boolean
    310. builtin.Int.or                                : Int
                                                       -> Int
                                                       -> Int
    311. builtin.Nat.or                                : Nat
                                                       -> Nat
                                                       -> Nat
    312. builtin.Int.popCount                          : Int
                                                       -> Nat
    313. builtin.Nat.popCount                          : Nat
                                                       -> Nat
    314. builtin.Float.pow                             : Float
                                                       -> Float
                                                       -> Float
    315. builtin.Int.pow                               : Int
                                                       -> Nat
                                                       -> Int
    316. builtin.Nat.pow                               : Nat
                                                       -> Nat
                                                       -> Nat
    317. builtin.Ref.read                              : Ref g a
                                                       ->{g} a
    318. builtin.io2.TVar.read                         : TVar a
                                                       ->{STM} a
    319. builtin.io2.TVar.readIO                       : TVar a
                                                       ->{IO} a
    320. builtin.io2.IO.ref                            : a
                                                       ->{IO} Ref
                                                         {IO} a
    321. builtin.Scope.ref                             : a
                                                       ->{Scope
                                                         s} Ref
                                                         {Scope
                                                           s}
                                                         a
    322. builtin.Text.repeat                           : Nat
                                                       -> Text
                                                       -> Text
    323. builtin.io2.STM.retry                         : '{STM} a
    324. builtin.Float.round                           : Float
                                                       -> Int
    325. builtin.Scope.run                             : (∀ s.
                                                         '{g,
                                                         Scope s} r)
                                                       ->{g} r
    326. builtin.Code.serialize                        : Code
                                                       -> Bytes
    327. builtin.Value.serialize                       : Value
                                                       -> Bytes
    328. builtin.io2.Tls.ClientConfig.certificates.set : [SignedCert]
                                                       -> ClientConfig
                                                       -> ClientConfig
    329. builtin.io2.Tls.ServerConfig.certificates.set : [SignedCert]
                                                       -> ServerConfig
                                                       -> ServerConfig
    330. builtin.io2.TLS.ClientConfig.ciphers.set      : [Cipher]
                                                       -> ClientConfig
                                                       -> ClientConfig
    331. builtin.io2.Tls.ServerConfig.ciphers.set      : [Cipher]
                                                       -> ServerConfig
                                                       -> ServerConfig
    332. builtin.io2.Tls.ClientConfig.versions.set     : [Version]
                                                       -> ClientConfig
                                                       -> ClientConfig
    333. builtin.io2.Tls.ServerConfig.versions.set     : [Version]
                                                       -> ServerConfig
                                                       -> ServerConfig
    334. builtin.Int.shiftLeft                         : Int
                                                       -> Nat
                                                       -> Int
    335. builtin.Nat.shiftLeft                         : Nat
                                                       -> Nat
                                                       -> Nat
    336. builtin.Int.shiftRight                        : Int
                                                       -> Nat
                                                       -> Int
    337. builtin.Nat.shiftRight                        : Nat
                                                       -> Nat
                                                       -> Nat
    338. builtin.Int.signum                            : Int
                                                       -> Int
    339. builtin.Float.sin                             : Float
                                                       -> Float
    340. builtin.Float.sinh                            : Float
                                                       -> Float
    341. builtin.Bytes.size                            : Bytes
                                                       -> Nat
    342. builtin.List.size                             : [a]
                                                       -> Nat
    343. builtin.Text.size                             : Text
                                                       -> Nat
    344. builtin.Float.sqrt                            : Float
                                                       -> Float
    345. builtin.io2.IO.stdHandle                      : StdHandle
                                                       -> Handle
    346. builtin.Nat.sub                               : Nat
                                                       -> Nat
                                                       -> Int
    347. builtin.io2.TVar.swap                         : TVar a
                                                       -> a
                                                       ->{STM} a
    348. builtin.io2.IO.systemTimeMicroseconds         : '{IO} Int
    349. builtin.Bytes.take                            : Nat
                                                       -> Bytes
                                                       -> Bytes
    350. builtin.List.take                             : Nat
                                                       -> [a]
                                                       -> [a]
    351. builtin.Text.take                             : Nat
                                                       -> Text
                                                       -> Text
    352. builtin.Float.tan                             : Float
                                                       -> Float
    353. builtin.Float.tanh                            : Float
                                                       -> Float
    354. builtin.Bytes.toBase16                        : Bytes
                                                       -> Bytes
    355. builtin.Bytes.toBase32                        : Bytes
                                                       -> Bytes
    356. builtin.Bytes.toBase64                        : Bytes
                                                       -> Bytes
    357. builtin.Bytes.toBase64UrlUnpadded             : Bytes
                                                       -> Bytes
    358. builtin.Text.toCharList                       : Text
                                                       -> [Char]
    359. builtin.Int.toFloat                           : Int
                                                       -> Float
    360. builtin.Nat.toFloat                           : Nat
                                                       -> Float
    361. builtin.Nat.toInt                             : Nat
                                                       -> Int
    362. builtin.Bytes.toList                          : Bytes
                                                       -> [Nat]
    363. builtin.Char.toNat                            : Char
                                                       -> Nat
    364. builtin.Float.toRepresentation                : Float
                                                       -> Nat
    365. builtin.Int.toRepresentation                  : Int
                                                       -> Nat
    366. builtin.Char.toText                           : Char
                                                       -> Text
    367. builtin.Float.toText                          : Float
                                                       -> Text
    368. builtin.Handle.toText                         : Handle
                                                       -> Text
    369. builtin.Int.toText                            : Int
                                                       -> Text
    370. builtin.Nat.toText                            : Nat
                                                       -> Text
    371. builtin.Socket.toText                         : Socket
                                                       -> Text
    372. builtin.Link.Term.toText                      : Term
                                                       -> Text
    373. builtin.ThreadId.toText                       : ThreadId
                                                       -> Text
    374. builtin.Text.toUtf8                           : Text
                                                       -> Bytes
    375. builtin.todo                                  : a -> b
    376. builtin.Debug.trace                           : Text
                                                       -> a
                                                       -> ()
    377. builtin.Int.trailingZeros                     : Int
                                                       -> Nat
    378. builtin.Nat.trailingZeros                     : Nat
                                                       -> Nat
    379. builtin.Float.truncate                        : Float
                                                       -> Int
    380. builtin.Int.truncate0                         : Int
                                                       -> Nat
    381. builtin.io2.MVar.tryTake                      : MVar a
                                                       ->{IO} Optional
                                                         a
    382. builtin.Text.uncons                           : Text
                                                       -> Optional
                                                         ( Char,
                                                           Text)
    383. builtin.Any.unsafeExtract                     : Any
                                                       -> a
    384. builtin.Text.unsnoc                           : Text
                                                       -> Optional
                                                         ( Text,
                                                           Char)
    385. builtin.Code.validate                         : [( Term,
                                                         Code)]
                                                       ->{IO} Optional
                                                         Failure
    386. builtin.io2.validateSandboxed                 : [Term]
                                                       -> a
                                                       -> Boolean
    387. builtin.Value.value                           : a
                                                       -> Value
    388. builtin.Debug.watch                           : Text
                                                       -> a
                                                       -> a
    389. builtin.Ref.write                             : Ref g a
                                                       -> a
                                                       ->{g} ()
    390. builtin.io2.TVar.write                        : TVar a
                                                       -> a
                                                       ->{STM} ()
    391. builtin.Int.xor                               : Int
                                                       -> Int
                                                       -> Int
    392. builtin.Nat.xor                               : Nat
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
