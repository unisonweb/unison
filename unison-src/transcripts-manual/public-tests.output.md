Runs tests on the latest releases of some public projects.

It turns out that pulling releases is very fast compared to actually running the tests (which take about 10 minutes total), so it's not worth caching the codebase.

``` ucm
x/base> pull @unison/base/releases/latest

  ✅

  Successfully pulled into x/base, which was empty.

x/base> test

  ✅  

























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































    New test results:

    1.   abilities.Abort.tests.ex1                                       ◉ Passed
    2.   abilities.Abort.tests.ex2                                       ◉ Passed
    3.   abilities.Abort.tests.ex3                                       ◉ Passed
    4.   abilities.Abort.toException.doesn'tThrowOnNoAbort               ◉ Passed
    5.   abilities.Abort.toException.throwsOnAbort                       ◉ Passed
    6.   abilities.Abort.toGenericException.doesn'tThrowOnNoAbort        ◉ Passed
    7.   abilities.Abort.toGenericException.throwsOnAbort                ◉ Passed
    8.   abilities.Ask.map!.test                                         ◉ Passed
    9.   abilities.Ask.provide'.tests                                    ◉ Passed
    10.  abilities.Ask.tests.ex1                                         ◉ Passed
    11.  abilities.Ask.toStore.tests                                     ◉ Passed
    12.  abilities.Each.count.tests                                      ◉ Passed
    13.  abilities.Random.char.ascii.control.tests                       ◉ Passed
    14.  abilities.Random.char.ascii.lower.tests                         ◉ Passed
    15.  abilities.Random.char.ascii.printable.tests                     ◉ Passed
    16.  abilities.Random.char.ascii.upper.tests                         ◉ Passed
    17.  abilities.Random.char.base32Hex.tests.correctCharacterSet       ◉ Passed
    18.  abilities.Random.intIn.tests.ex1                                ◉ Passed
    19.  abilities.Random.intIn.tests.fairness                           ◉ Passed
    20.  abilities.Random.nat.natsWithSum.tests                          ◉ Passed
    21.  abilities.Random.natIn.tests.ex1                                ◉ Passed
    22.  abilities.Random.natIn.tests.fairness                           ◉ Passed
    23.  abilities.Random.shuffle.tests                                  ◉ Passed
    24.  abilities.Random.splits.bytes.tests                             ◉ Passed
    25.  abilities.Random.splits.list.tests                              ◉ Passed
    26.  abilities.Store.local!.tests.ex1                                ◉ Passed
    27.  abilities.Store.local.deprecated.test                           ◉ Passed
    28.  abilities.Store.modify.test                                     ◉ Passed
    29.  abilities.Store.unfold!.tests.ex1                               ◉ Passed
    30.  abilities.Store.withInitialValue.tests.law1                     ◉ Passed
    31.  abilities.Throw.tests.ex1                                       ◉ Passed
    32.  abilities.Throw.tests.ex2                                       ◉ Passed
    33.  abilities.Throw.tests.ex3                                       ◉ Passed
    34.  Boolean.given.test                                              ◉ Passed
    35.  Boolean.implies.test                                            ◉ Passed
    36.  Boolean.until.test                                              ◉ Passed
    37.  Boolean.while.test                                              ◉ Passed
    38.  Boolean.xor.test                                                ◉ Passed
    39.  Bytes.constantTimeEqual.tests                                   ◉ Passed
    40.  Bytes.fromBase32Hex.tests.inverse                               ◉ Passed
    41.  Bytes.indexOf.tests.infix                                       ◉ Passed
    42.  Bytes.isPrefixOf.tests                                          ◉ Passed
    43.  Bytes.splitAt.tests                                             ◉ Passed
    44.  Bytes.toBase32Hex.tests.inverse                                 ◉ Passed
    45.  Char.ascii.tests.propUpperLower                                 ◉ Passed
    46.  Char.ascii.tests.toLower                                        ◉ Passed
    47.  Char.ascii.tests.toUpper                                        ◉ Passed
    48.  crypto.hmac.verify.test.happy                                   ◉ Passed
    49.  crypto.hmac.verify.test.sad                                     ◉ Passed
    50.  crypto.hmac.verifyBytes.test.happy                              ◉ Passed
    51.  crypto.hmac.verifyBytes.test.sad                                ◉ Passed
    52.  crypto.Rsa.sign.test                                            ◉ Passed
    53.  crypto.Rsa.verify.test                                          ◉ Passed
    54.  data.Array.copy.tests.doesCopy                                  ◉ Passed
    55.  data.Array.copy.tests.doesNotCopyPastEndOfDestinationArray      ◉ Passed
    56.  data.Array.find.tests.consistentWithList                        ◉ Passed
    57.  data.Array.firstIndexOf.tests.finds                             ◉ Passed
    58.  data.Array.new!.tests                                           ◉ Passed
    59.  data.Array.randomChoice.test                                    ◉ Passed
    60.  data.Array.randomChoice.testEmpty                               ◉ Passed
    61.  data.Bag.add.tests.adds                                         ◉ Passed
    62.  data.Bag.addAll.tests.homomorphism                              ◉ Passed
    63.  data.Bag.addN.test                                              ◉ Passed
    64.  data.Bag.all.test                                               ◉ Passed
    65.  data.Bag.any.test                                               ◉ Passed
    66.  data.Bag.contains.test                                          ◉ Passed
    67.  data.Bag.convolve.tests.associative                             ◉ Passed
    68.  data.Bag.convolve.tests.natural                                 ◉ Passed
    69.  data.Bag.convolve.tests.unit                                    ◉ Passed
    70.  data.Bag.count.tests.counts                                     ◉ Passed
    71.  data.Bag.difference.tests.homomorphism                          ◉ Passed
    72.  data.Bag.elementOf.test                                         ◉ Passed
    73.  data.Bag.empty.test                                             ◉ Passed
    74.  data.Bag.equals.tests.reflexive                                 ◉ Passed
    75.  data.Bag.equals.tests.symmetric                                 ◉ Passed
    76.  data.Bag.equals.tests.transitive                                ◉ Passed
    77.  data.Bag.flatMap.tests.associative                              ◉ Passed
    78.  data.Bag.flatMap.tests.unit                                     ◉ Passed
    79.  data.Bag.foldLeft.tests.asList                                  ◉ Passed
    80.  data.Bag.foldRight.tests.asList                                 ◉ Passed
    81.  data.Bag.from.tests.subset                                      ◉ Passed
    82.  data.Bag.fromList.tests.roundtrip                               ◉ Passed
    83.  data.Bag.fromMap.tests.roundTrip                                ◉ Passed
    84.  data.Bag.fromOccurrenceList.tests.roundTrip                     ◉ Passed
    85.  data.Bag.fromText.tests.anagram                                 ◉ Passed
    86.  data.Bag.intersect.tests.commutative                            ◉ Passed
    87.  data.Bag.intersect.tests.distributive                           ◉ Passed
    88.  data.Bag.intersect.tests.homomorphism                           ◉ Passed
    89.  data.Bag.intersect.tests.idempotent                             ◉ Passed
    90.  data.Bag.intersect.tests.zero                                   ◉ Passed
    91.  data.Bag.isEmpty.test                                           ◉ Passed
    92.  data.Bag.map.tests.functor                                      ◉ Passed
    93.  data.Bag.map.tests.surjection                                   ◉ Passed
    94.  data.Bag.mapCounts.tests.functor                                ◉ Passed
    95.  data.Bag.modify.tests.double                                    ◉ Passed
    96.  data.Bag.modify.tests.functor                                   ◉ Passed
    97.  data.Bag.modify.tests.zero                                      ◉ Passed
    98.  data.Bag.none.test                                              ◉ Passed
    99.  data.Bag.nth.tests                                              ◉ Passed
    100. data.Bag.product.tests.spec                                     ◉ Passed
    101. data.Bag.randomChoice.test                                      ◉ Passed
    102. data.Bag.randomChoice.testEmpty                                 ◉ Passed
    103. data.Bag.remove.tests.removes                                   ◉ Passed
    104. data.Bag.removeAll.tests.removes                                ◉ Passed
    105. data.Bag.removeN.tests.removes                                  ◉ Passed
    106. data.Bag.removeWhere.tests.removes                              ◉ Passed
    107. data.Bag.scale.tests.scales                                     ◉ Passed
    108. data.Bag.singleton.tests.one                                    ◉ Passed
    109. data.Bag.size.test                                              ◉ Passed
    110. data.Bag.subbag.tests.homomorphism                              ◉ Passed
    111. data.Bag.superbag.tests.homomorphism                            ◉ Passed
    112. data.Bag.toList.tests.roundtrip                                 ◉ Passed
    113. data.Bag.toSet.tests.roundtrip                                  ◉ Passed
    114. data.Bag.toText.tests.roundtrip                                 ◉ Passed
    115. data.Bag.traverse.tests.functor                                 ◉ Passed
    116. data.Bag.union.tests.commutative                                ◉ Passed
    117. data.Bag.union.tests.distributive                               ◉ Passed
    118. data.Bag.union.tests.homomorphism                               ◉ Passed
    119. data.Bag.union.tests.idempotent                                 ◉ Passed
    120. data.Bag.union.tests.unit                                       ◉ Passed
    121. data.ByteArray.append.test                                      ◉ Passed
    122. data.ByteArray.base32Hex.test                                   ◉ Passed
    123. data.ByteArray.drop.test                                        ◉ Passed
    124. data.ByteArray.join.test                                        ◉ Passed
    125. data.ByteArray.randomChoice.test                                ◉ Passed
    126. data.ByteArray.randomChoice.testEmpty                           ◉ Passed
    127. data.ByteArray.size.test                                        ◉ Passed
    128. data.ByteArray.take.test                                        ◉ Passed
    129. data.ByteArray.toList.test                                      ◉ Passed
    130. data.Graph.tests.build                                          ◉ Passed
    131. data.Graph.tests.reverse                                        ◉ Passed
    132. data.Graph.tests.sccs                                           ◉ Passed
    133. data.Graph.tests.topSort.test                                   ◉ Passed
    134. data.List.all.tests.deMorgan                                    ◉ Passed
    135. data.List.all.tests.homomorphism                                ◉ Passed
    136. data.List.any.tests.deMorgan                                    ◉ Passed
    137. data.List.any.tests.homomorphism                                ◉ Passed
    138. data.List.anyIndexOf.tests.empty                                ◉ Passed
    139. data.List.anyIndexOf.tests.lower                                ◉ Passed
    140. data.List.anyIndexOf.tests.notSorted                            ◉ Passed
    141. data.List.anyIndexOf.tests.sorted                               ◉ Passed
    142. data.List.anyIndexOf.tests.upper                                ◉ Passed
    143. data.List.at.tests.index                                        ◉ Passed
    144. data.List.concatOptional.tests.prop1                            ◉ Passed
    145. data.List.concatOptional.tests.test1                            ◉ Passed
    146. data.List.contains.tests.negative1                              ◉ Passed
    147. data.List.contains.tests.negative2                              ◉ Passed
    148. data.List.contains.tests.negative3                              ◉ Passed
    149. data.List.contains.tests.positive                               ◉ Passed
    150. data.List.deleteAt.test                                         ◉ Passed
    151. data.List.deleteFirst.tests.ex1                                 ◉ Passed
    152. data.List.deleteFirst.tests.ex2                                 ◉ Passed
    153. data.List.deleteFirst.tests.ex3                                 ◉ Passed
    154. data.List.deprecated.lubIndexOf.tests.empty                     ◉ Passed
    155. data.List.deprecated.lubIndexOf.tests.notSorted                 ◉ Passed
    156. data.List.deprecated.lubIndexOf.tests.sorted                    ◉ Passed
    157. data.List.distinct.tests.preservation                           ◉ Passed
    158. data.List.distinctBy.tests.preservation                         ◉ Passed
    159. data.List.distinctBy.tests.removesDuplicates                    ◉ Passed
    160. data.List.dropRightWhile.tests                                  ◉ Passed
    161. data.List.dropRightWhile.tests.all                              ◉ Passed
    162. data.List.dropRightWhile.tests.last                             ◉ Passed
    163. data.List.dropRightWhile.tests.none                             ◉ Passed
    164. data.List.dropWhile.tests.all                                   ◉ Passed
    165. data.List.dropWhile.tests.middle                                ◉ Passed
    166. data.List.dropWhile.tests.none                                  ◉ Passed
    167. data.List.fill'.tests                                           ◉ Passed
    168. data.List.fill.tests.ex1                                        ◉ Passed
    169. data.List.filter.tests.empty                                    ◉ Passed
    170. data.List.filter.tests.negative                                 ◉ Passed
    171. data.List.filter.tests.positive                                 ◉ Passed
    172. data.List.filterMap.tests.identity                              ◉ Passed
    173. data.List.filterMap.tests.mapsAndFilters                        ◉ Passed
    174. data.List.find.tests.ex1                                        ◉ Passed
    175. data.List.find.tests.ex2                                        ◉ Passed
    176. data.List.findMap.tests.ex1                                     ◉ Passed
    177. data.List.findMap.tests.ex2                                     ◉ Passed
    178. data.List.firstIndexOf.tests.negative1                          ◉ Passed
    179. data.List.firstIndexOf.tests.negative2                          ◉ Passed
    180. data.List.firstIndexOf.tests.negative3                          ◉ Passed
    181. data.List.firstIndexOf.tests.positive1                          ◉ Passed
    182. data.List.firstIndexOf.tests.positive2                          ◉ Passed
    183. data.List.firstIndexOf.tests.positive3                          ◉ Passed
    184. data.List.flatMap.tests.associative                             ◉ Passed
    185. data.List.flatMap.tests.flatMapIdentityIsJoin                   ◉ Passed
    186. data.List.flatMap.tests.identity                                ◉ Passed
    187. data.List.flatMapRight.tests.flatMapIdentityIsJoin              ◉ Passed
    188. data.List.foldLeft.tests.dual                                   ◉ Passed
    189. data.List.foldLeft.tests.endomorphic                            ◉ Passed
    190. data.List.foldLeft.tests.length                                 ◉ Passed
    191. data.List.foldRight.tests.dual                                  ◉ Passed
    192. data.List.foldRight.tests.endomorphic                           ◉ Passed
    193. data.List.foldRight.tests.homomorphism                          ◉ Passed
    194. data.List.foldRight.tests.length                                ◉ Passed
    195. data.List.foreach.deprecated.tests                              ◉ Passed
    196. data.List.groupBy.tests.groups                                  ◉ Passed
    197. data.List.groupConsecutive.tests.base                           ◉ Passed
    198. data.List.groupMap.tests.groups                                 ◉ Passed
    199. data.List.groupMapReduce.tests.groups                           ◉ Passed
    200. data.List.head.test                                             ◉ Passed
    201. data.List.indexed.tests.isIndexed                               ◉ Passed
    202. data.List.initialize.tests.tests.ex1                            ◉ Passed
    203. data.List.insertSortedDistinct.tests                            ◉ Passed
    204. data.List.intercalate.tests.empty                               ◉ Passed
    205. data.List.intercalate.tests.empty1                              ◉ Passed
    206. data.List.intercalate.tests.empty2                              ◉ Passed
    207. data.List.intercalate.tests.simple                              ◉ Passed
    208. data.List.intercalate.tests.size                                ◉ Passed
    209. data.List.intersperse.tests.base                                ◉ Passed
    210. data.List.intersperse.tests.empty                               ◉ Passed
    211. data.List.isInfixOf.tests.prop1                                 ◉ Passed
    212. data.List.isPrefixOf.tests.prop1                                ◉ Passed
    213. data.List.isPrefixOf.tests.prop2                                ◉ Passed
    214. data.List.isSuffixOf.tests.prop1                                ◉ Passed
    215. data.List.isSuffixOf.tests.prop2                                ◉ Passed
    216. data.List.iterate.tests.ex1                                     ◉ Passed
    217. data.List.join.tests.associative                                ◉ Passed
    218. data.List.join.tests.homomorphism                               ◉ Passed
    219. data.List.join.tests.unit                                       ◉ Passed
    220. data.List.last.tests.elems                                      ◉ Passed
    221. data.List.last.tests.empty                                      ◉ Passed
    222. data.List.last.tests.isHeadOnReversedList                       ◉ Passed
    223. data.List.last.tests.single                                     ◉ Passed
    224. data.List.lefts.tests.ex1                                       ◉ Passed
    225. data.List.map.tests.functor                                     ◉ Passed
    226. data.List.mapIndexed.tests.ex1                                  ◉ Passed
    227. data.List.mapIndexed.tests.ex2                                  ◉ Passed
    228. data.List.mapIndexed.tests.mapsAndIndexes                       ◉ Passed
    229. data.List.maximum.tests.base                                    ◉ Passed
    230. data.List.maximum.tests.empty                                   ◉ Passed
    231. data.List.minimum.tests.base                                    ◉ Passed
    232. data.List.modifyAt.tests                                        ◉ Passed
    233. data.List.Nonempty.align.tests                                  ◉ Passed
    234. data.List.Nonempty.alignWith.tests                              ◉ Passed
    235. data.List.Nonempty.append.tests.associative                     ◉ Passed
    236. data.List.Nonempty.append.tests.homomorphism                    ◉ Passed
    237. data.List.Nonempty.filterMap.test.example                       ◉ Passed
    238. data.List.Nonempty.flatMap.tests.flatMapIdIsjoin                ◉ Passed
    239. data.List.Nonempty.foldLeft.tests.listConsistency               ◉ Passed
    240. data.List.Nonempty.foldLeft.tests.multiple                      ◉ Passed
    241. data.List.Nonempty.foldLeft.tests.single                        ◉ Passed
    242. data.List.Nonempty.foldMap.test                                 ◉ Passed
    243. data.List.Nonempty.foldRight.tests.listConsistency              ◉ Passed
    244. data.List.Nonempty.foldRight.tests.multiple                     ◉ Passed
    245. data.List.Nonempty.foldRight.tests.single                       ◉ Passed
    246. data.List.Nonempty.head.test                                    ◉ Passed
    247. data.List.Nonempty.init.test                                    ◉ Passed
    248. data.List.Nonempty.join.tests.associative                       ◉ Passed
    249. data.List.Nonempty.join.tests.unit                              ◉ Passed
    250. data.List.Nonempty.last.test                                    ◉ Passed
    251. data.List.Nonempty.map.tests.functor                            ◉ Passed
    252. data.List.Nonempty.maximum.test                                 ◉ Passed
    253. data.List.Nonempty.maximumBy.test                               ◉ Passed
    254. data.List.Nonempty.minimum.test                                 ◉ Passed
    255. data.List.Nonempty.minimumBy.test                               ◉ Passed
    256. data.List.Nonempty.randomChoice.test                            ◉ Passed
    257. data.List.Nonempty.reduceLeft.test                              ◉ Passed
    258. data.List.Nonempty.reduceRight.test                             ◉ Passed
    259. data.List.Nonempty.scanLeft.test                                ◉ Passed
    260. data.List.Nonempty.scanRight.test                               ◉ Passed
    261. data.List.Nonempty.sequenceOptional.test.example                ◉ Passed
    262. data.List.Nonempty.singleton.test                               ◉ Passed
    263. data.List.Nonempty.size.test                                    ◉ Passed
    264. data.List.Nonempty.somes.tests.ex1                              ◉ Passed
    265. data.List.Nonempty.tail.test                                    ◉ Passed
    266. data.List.Nonempty.traverseOptional.test.example                ◉ Passed
    267. data.List.nonEmptySubsequences.tests.base                       ◉ Passed
    268. data.List.partitionEithers.tests.ex1                            ◉ Passed
    269. data.List.powerslice.tests.prop1                                ◉ Passed
    270. data.List.powerslice.tests.test1                                ◉ Passed
    271. data.List.randomChoice.test                                     ◉ Passed
    272. data.List.randomChoice.testEmpty                                ◉ Passed
    273. data.List.rangeClosed.tests.empty                               ◉ Passed
    274. data.List.rangeClosed.tests.empty2                              ◉ Passed
    275. data.List.rangeClosed.tests.empty3                              ◉ Passed
    276. data.List.rangeClosed.tests.fromBig                             ◉ Passed
    277. data.List.rangeClosed.tests.fromOne                             ◉ Passed
    278. data.List.rangeClosed.tests.fromZero                            ◉ Passed
    279. data.List.rangeClosed.tests.rangeSize                           ◉ Passed
    280. data.List.rangeClosed.tests.single0                             ◉ Passed
    281. data.List.rangeClosed.tests.single1                             ◉ Passed
    282. data.List.rangeClosed.tests.single99                            ◉ Passed
    283. data.List.replicate.test                                        ◉ Passed
    284. data.List.rights.tests.ex1                                      ◉ Passed
    285. data.List.scanLeft.test                                         ◉ Passed
    286. data.List.scanRight.test                                        ◉ Passed
    287. data.List.slidingPairs.tests.concat                             ◉ Passed
    288. data.List.slidingWindow.tests.ex1                               ◉ Passed
    289. data.List.slidingWindow.tests.ex2                               ◉ Passed
    290. data.List.slidingWindow.tests.ex3                               ◉ Passed
    291. data.List.slidingWindow.tests.ex4                               ◉ Passed
    292. data.List.slidingWindow.tests.ex5                               ◉ Passed
    293. data.List.slidingWindow.tests.isSlidingPairsForLength2          ◉ Passed
    294. data.List.snoc.tests.isReversedConsOnReversedList               ◉ Passed
    295. data.List.somes.tests.ex1                                       ◉ Passed
    296. data.List.sort.test                                             ◉ Passed
    297. data.List.sortWith.tests.isOrdered                              ◉ Passed
    298. data.List.sortWith.tests.length                                 ◉ Passed
    299. data.List.sortWith.tests.permutation                            ◉ Passed
    300. data.List.span.tests.alle                                       ◉ Passed
    301. data.List.span.tests.allf                                       ◉ Passed
    302. data.List.span.tests.middle                                     ◉ Passed
    303. data.List.split.tests.base                                      ◉ Passed
    304. data.List.split.tests.double                                    ◉ Passed
    305. data.List.split.tests.empty                                     ◉ Passed
    306. data.List.splitAt.tests.base                                    ◉ Passed
    307. data.List.splitAt.tests.ob                                      ◉ Passed
    308. data.List.splitAt.tests.zero                                    ◉ Passed
    309. data.List.stripPrefix.tests.all                                 ◉ Passed
    310. data.List.stripPrefix.tests.base                                ◉ Passed
    311. data.List.stripPrefix.tests.none                                ◉ Passed
    312. data.List.subsequences.tests.base                               ◉ Passed
    313. data.List.tail.tests.isReversedInitOnReversedList               ◉ Passed
    314. data.List.tail.tests.prop1                                      ◉ Passed
    315. data.List.tail.tests.test1                                      ◉ Passed
    316. data.List.tail.tests.test2                                      ◉ Passed
    317. data.List.takeWhile.tests.all                                   ◉ Passed
    318. data.List.takeWhile.tests.middle                                ◉ Passed
    319. data.List.takeWhile.tests.none                                  ◉ Passed
    320. data.List.tests.replacements.checkRange                         ◉ Passed
    321. data.List.tests.replacements.checkSort                          ◉ Passed
    322. data.List.tests.sequenceOptional.empty                          ◉ Passed
    323. data.List.tests.sequenceOptional.non_empty                      ◉ Passed
    324. data.List.tests.sequenceOptional.non_empty_2                    ◉ Passed
    325. data.List.tests.traverseOptional.empty                          ◉ Passed
    326. data.List.tests.traverseOptional.non_empty                      ◉ Passed
    327. data.List.tests.traverseOptional.non_empty_2                    ◉ Passed
    328. data.List.toText.tests                                          ◉ Passed
    329. data.List.transpose.tests.identityProp                          ◉ Passed
    330. data.List.transpose.tests.joinProp                              ◉ Passed
    331. data.List.transpose.tests.lengthProp                            ◉ Passed
    332. data.List.uncons.tests.isHeadAndTailMerged                      ◉ Passed
    333. data.List.uncons.tests.isInverseOfCons                          ◉ Passed
    334. data.List.uncons.tests.isReversedUnsnocOnReversedList           ◉ Passed
    335. data.List.unsnoc.tests.isInitAndLastMerged                      ◉ Passed
    336. data.List.unsnoc.tests.isInverseOfSnoc                          ◉ Passed
    337. data.List.updateAt.tests.updates                                ◉ Passed
    338. data.List.zip.tests.length                                      ◉ Passed
    339. data.List.zip.tests.productLaw                                  ◉ Passed
    340. data.List.zipWith.tests.edge1                                   ◉ Passed
    341. data.List.zipWith.tests.edge2                                   ◉ Passed
    342. data.List.zipWith.tests.edge3                                   ◉ Passed
    343. data.List.zipWith.tests.edge4                                   ◉ Passed
    344. data.List.zipWith.tests.ex1                                     ◉ Passed
    345. data.List.zipWith.tests.ex2                                     ◉ Passed
    346. data.List.zipWith.tests.zipWithRange                            ◉ Passed
    347. data.Map.==.tests.reflexive                                     ◉ Passed
    348. data.Map.==.tests.symmetric                                     ◉ Passed
    349. data.Map.==.tests.transitive                                    ◉ Passed
    350. data.Map.adjust.tests.adjusts                                   ◉ Passed
    351. data.Map.adjust.tests.alterMap                                  ◉ Passed
    352. data.Map.adjustWithKey.tests.adjusts                            ◉ Passed
    353. data.Map.alignWithKey.tests.effects                             ◉ Passed
    354. data.Map.alter.tests.functor                                    ◉ Passed
    355. data.Map.alter.tests.homomorphism                               ◉ Passed
    356. data.Map.alter.tests.naturality                                 ◉ Passed
    357. data.Map.breakOffMax.tests.isMax                                ◉ Passed
    358. data.Map.breakOffMin.tests.isMin                                ◉ Passed
    359. data.Map.contains.tests.delete                                  ◉ Passed
    360. data.Map.contains.tests.put                                     ◉ Passed
    361. data.Map.delete.tests.preservesOtherKeys                        ◉ Passed
    362. data.Map.difference.tests.prop                                  ◉ Passed
    363. data.Map.dropLargest.tests                                      ◉ Passed
    364. data.Map.dropSmallest.tests                                     ◉ Passed
    365. data.Map.filterAlignWithKey.tests.difference                    ◉ Passed
    366. data.Map.filterAlignWithKey.tests.effects                       ◉ Passed
    367. data.Map.filterAlignWithKey.tests.empty                         ◉ Passed
    368. data.Map.filterAlignWithKey.tests.intersection                  ◉ Passed
    369. data.Map.filterAlignWithKey.tests.leftIdentity                  ◉ Passed
    370. data.Map.filterAlignWithKey.tests.rightIdentity                 ◉ Passed
    371. data.Map.filterAlignWithKey.tests.union                         ◉ Passed
    372. data.Map.filterWithKey.tests.emptyFiltersEverything             ◉ Passed
    373. data.Map.filterWithKey.tests.filterComposition                  ◉ Passed
    374. data.Map.filterWithKey.tests.filtersDeleted                     ◉ Passed
    375. data.Map.filterWithKey.tests.filtersInserted                    ◉ Passed
    376. data.Map.filterWithKey.tests.trueIdentity                       ◉ Passed
    377. data.Map.foldLeft.tests.onElements                              ◉ Passed
    378. data.Map.foldLeftWithKey.tests.onAssoc                          ◉ Passed
    379. data.Map.foldRight.tests.onElements                             ◉ Passed
    380. data.Map.foldRightWithKey.tests.onElements                      ◉ Passed
    381. data.Map.fromList.tests.roundtrip                               ◉ Passed
    382. data.Map.fromListWith.tests.roundtrip                           ◉ Passed
    383. data.Map.fromListWithKey.tests.roundtrip                        ◉ Passed
    384. data.Map.get.tests.spec                                         ◉ Passed
    385. data.Map.getMax.tests.isMax                                     ◉ Passed
    386. data.Map.getMin.tests.isMin                                     ◉ Passed
    387. data.Map.getOrElse.tests.spec                                   ◉ Passed
    388. data.Map.gt.tests                                               ◉ Passed
    389. data.Map.gteq.tests                                             ◉ Passed
    390. data.Map.insertIfMissing.tests                                  ◉ Passed
    391. data.Map.intersect.tests.associative                            ◉ Passed
    392. data.Map.intersect.tests.idempotent                             ◉ Passed
    393. data.Map.intersect.tests.zero                                   ◉ Passed
    394. data.Map.intersectWith.tests.commutative                        ◉ Passed
    395. data.Map.intersectWith.tests.idempotent                         ◉ Passed
    396. data.Map.intersectWith.tests.zero                               ◉ Passed
    397. data.Map.intersectWithKey.tests.commutative                     ◉ Passed
    398. data.Map.intersectWithKey.tests.idempotent                      ◉ Passed
    399. data.Map.intersectWithKey.tests.zero                            ◉ Passed
    400. data.Map.isEmpty.tests.spec                                     ◉ Passed
    401. data.Map.keys.tests.spec                                        ◉ Passed
    402. data.Map.lt.tests                                               ◉ Passed
    403. data.Map.lteq.tests                                             ◉ Passed
    404. data.Map.map.tests.functor                                      ◉ Passed
    405. data.Map.mapKeys.tests.functorish                               ◉ Passed
    406. data.Map.mapKeys.tests.retainGreatest                           ◉ Passed
    407. data.Map.mapKeysWith.tests.functorish                           ◉ Passed
    408. data.Map.mapKeysWith.tests.greatestFirst                        ◉ Passed
    409. data.Map.mapWithKey.tests.functorish                            ◉ Passed
    410. data.Map.mapWithKey.tests.worksLikeList                         ◉ Passed
    411. data.Map.Nonempty.==.tests.reflexive                            ◉ Passed
    412. data.Map.Nonempty.==.tests.symmetrical                          ◉ Passed
    413. data.Map.Nonempty.==.tests.transitive                           ◉ Passed
    414. data.Map.Nonempty.adjust.tests.adjusts                          ◉ Passed
    415. data.Map.Nonempty.adjustWithKey.tests.adjusts                   ◉ Passed
    416. data.Map.Nonempty.nth.tests                                     ◉ Passed
    417. data.Map.Nonempty.randomChoice.test                             ◉ Passed
    418. data.Map.nth.tests                                              ◉ Passed
    419. data.Map.putGetWithKey.tests.putsAndGets                        ◉ Passed
    420. data.Map.putWith.tests.puts                                     ◉ Passed
    421. data.Map.randomChoice.test                                      ◉ Passed
    422. data.Map.randomChoice.testEmpty                                 ◉ Passed
    423. data.Map.singleton.tests.roundtrip                              ◉ Passed
    424. data.Map.size.tests.numberOfKeys                                ◉ Passed
    425. data.Map.split.tests.splits                                     ◉ Passed
    426. data.Map.takeLargest.tests                                      ◉ Passed
    427. data.Map.takeSmallest.tests                                     ◉ Passed
    428. data.Map.tests.replacements.checkEq                             ◉ Passed
    429. data.Map.tests.replacements.checkFromList                       ◉ Passed
    430. data.Map.tests.replacements.checkGet                            ◉ Passed
    431. data.Map.tests.replacements.checkInsert                         ◉ Passed
    432. data.Map.toList.tests.roundtrip                                 ◉ Passed
    433. data.Map.toStream.tests                                         ◉ Passed
    434. data.Map.union.tests.associative                                ◉ Passed
    435. data.Map.union.tests.idempotent                                 ◉ Passed
    436. data.Map.union.tests.unit                                       ◉ Passed
    437. data.Map.unionWith.tests.commutative                            ◉ Passed
    438. data.Map.unionWith.tests.idempotent                             ◉ Passed
    439. data.Map.unionWith.tests.unit                                   ◉ Passed
    440. data.Map.unionWithKey.tests.commutative                         ◉ Passed
    441. data.Map.unionWithKey.tests.idempotent                          ◉ Passed
    442. data.Map.unionWithKey.tests.unit                                ◉ Passed
    443. data.Map.update.tests.updates                                   ◉ Passed
    444. data.Map.updateWithKey.tests.updates                            ◉ Passed
    445. data.Map.values.tests.roundtrip                                 ◉ Passed
    446. data.Multimap.fromList.tests.roundtrips                         ◉ Passed
    447. data.NatBag.add.nonempty.test                                   ◉ Passed
    448. data.NatBag.addAll.test                                         ◉ Passed
    449. data.NatBag.addMany.test                                        ◉ Passed
    450. data.NatBag.addN.test                                           ◉ Passed
    451. data.NatBag.all.test                                            ◉ Passed
    452. data.NatBag.any.test                                            ◉ Passed
    453. data.NatBag.contains.test                                       ◉ Passed
    454. data.NatBag.count.test                                          ◉ Passed
    455. data.NatBag.counts.test                                         ◉ Passed
    456. data.NatBag.difference.test                                     ◉ Passed
    457. data.NatBag.fromList.test                                       ◉ Passed
    458. data.NatBag.fromNatSet.test                                     ◉ Passed
    459. data.NatBag.Nonempty.add.test                                   ◉ Passed
    460. data.NatBag.Nonempty.addAll.test                                ◉ Passed
    461. data.NatBag.Nonempty.addMany.test                               ◉ Passed
    462. data.NatBag.Nonempty.addN.test                                  ◉ Passed
    463. data.NatBag.Nonempty.count.test                                 ◉ Passed
    464. data.NatBag.Nonempty.difference.test                            ◉ Passed
    465. data.NatBag.Nonempty.equals.test                                ◉ Passed
    466. data.NatBag.Nonempty.fromList.test                              ◉ Passed
    467. data.NatBag.Nonempty.fromNatSet.test                            ◉ Passed
    468. data.NatBag.Nonempty.nth.tests                                  ◉ Passed
    469. data.NatBag.Nonempty.randomChoice.test                          ◉ Passed
    470. data.NatBag.Nonempty.remove.test                                ◉ Passed
    471. data.NatBag.Nonempty.removeAll.test                             ◉ Passed
    472. data.NatBag.Nonempty.removeN.test                               ◉ Passed
    473. data.NatBag.Nonempty.subbag.test                                ◉ Passed
    474. data.NatBag.Nonempty.superbag.test                              ◉ Passed
    475. data.NatBag.Nonempty.toBag.test                                 ◉ Passed
    476. data.NatBag.Nonempty.toNatSet.test                              ◉ Passed
    477. data.NatBag.Nonempty.union.test                                 ◉ Passed
    478. data.NatBag.nth.tests                                           ◉ Passed
    479. data.NatBag.randomChoice.test                                   ◉ Passed
    480. data.NatBag.remove.test                                         ◉ Passed
    481. data.NatBag.removeAll.test                                      ◉ Passed
    482. data.NatBag.removeN.test                                        ◉ Passed
    483. data.NatBag.singleton.test                                      ◉ Passed
    484. data.NatBag.subbag.test                                         ◉ Passed
    485. data.NatBag.superbag.test                                       ◉ Passed
    486. data.NatBag.toBag.test                                          ◉ Passed
    487. data.NatBag.toNatSet.test                                       ◉ Passed
    488. data.NatBag.union.test                                          ◉ Passed
    489. data.NatMap.adjust.test                                         ◉ Passed
    490. data.NatMap.adjustWithKey.test                                  ◉ Passed
    491. data.NatMap.alter.test                                          ◉ Passed
    492. data.NatMap.alterWithKey.test                                   ◉ Passed
    493. data.NatMap.breakOffMax.test                                    ◉ Passed
    494. data.NatMap.breakOffMin.test                                    ◉ Passed
    495. data.NatMap.compareBy.test                                      ◉ Passed
    496. data.NatMap.fromListWith.tests.correctOrder                     ◉ Passed
    497. data.NatMap.insertGetWithKey.tests.correctOrder                 ◉ Passed
    498. data.NatMap.insertWithKey.tests.correctOrder                    ◉ Passed
    499. data.NatMap.keySet.test                                         ◉ Passed
    500. data.NatMap.Nonempty.nth.tests                                  ◉ Passed
    501. data.NatMap.Nonempty.randomChoice.test                          ◉ Passed
    502. data.NatMap.Nonempty.test.diff                                  ◉ Passed
    503. data.NatMap.Nonempty.test.unionAssociative                      ◉ Passed
    504. data.NatMap.Nonempty.test.unionCommutative                      ◉ Passed
    505. data.NatMap.Nonempty.test.unionInsert                           ◉ Passed
    506. data.NatMap.Nonempty.test.updateDelete                          ◉ Passed
    507. data.NatMap.nth.tests                                           ◉ Passed
    508. data.NatMap.randomChoice.test                                   ◉ Passed
    509. data.NatMap.randomChoice.testEmpty                              ◉ Passed
    510. data.NatMap.size.test                                           ◉ Passed
    511. data.NatMap.test.diff                                           ◉ Passed
    512. data.NatMap.test.insertDelete                                   ◉ Passed
    513. data.NatMap.test.single                                         ◉ Passed
    514. data.NatMap.test.unionAssociative                               ◉ Passed
    515. data.NatMap.test.unionCommutative                               ◉ Passed
    516. data.NatMap.test.unionInsert                                    ◉ Passed
    517. data.NatMap.test.updateDelete                                   ◉ Passed
    518. data.NatMap.unionWith.tests.correctOrder                        ◉ Passed
    519. data.NatSet.all.test                                            ◉ Passed
    520. data.NatSet.alter.test                                          ◉ Passed
    521. data.NatSet.any.test                                            ◉ Passed
    522. data.NatSet.contains.test                                       ◉ Passed
    523. data.NatSet.deleteMax.test                                      ◉ Passed
    524. data.NatSet.deleteMin.test                                      ◉ Passed
    525. data.NatSet.difference.test                                     ◉ Passed
    526. data.NatSet.equals.tests.reflexive                              ◉ Passed
    527. data.NatSet.equals.tests.symmetric                              ◉ Passed
    528. data.NatSet.equals.tests.transitive                             ◉ Passed
    529. data.NatSet.filter.test                                         ◉ Passed
    530. data.NatSet.filterMap.test                                      ◉ Passed
    531. data.NatSet.foldLeft.test                                       ◉ Passed
    532. data.NatSet.foldRight.test                                      ◉ Passed
    533. data.NatSet.intersect.test                                      ◉ Passed
    534. data.NatSet.map.tests.functor.homomorphism                      ◉ Passed
    535. data.NatSet.map.tests.functor.identity                          ◉ Passed
    536. data.NatSet.map.tests.noninjective                              ◉ Passed
    537. data.NatSet.maxView.test                                        ◉ Passed
    538. data.NatSet.minView.test                                        ◉ Passed
    539. data.NatSet.Nonempty.delete.test.deletedIsAbsent                ◉ Passed
    540. data.NatSet.Nonempty.disjoint.test                              ◉ Passed
    541. data.NatSet.Nonempty.getAbove.test                              ◉ Passed
    542. data.NatSet.Nonempty.getAtLeast.test                            ◉ Passed
    543. data.NatSet.Nonempty.getAtMost.test                             ◉ Passed
    544. data.NatSet.Nonempty.getBelow.test                              ◉ Passed
    545. data.NatSet.Nonempty.getMax.test                                ◉ Passed
    546. data.NatSet.Nonempty.getMin.test                                ◉ Passed
    547. data.NatSet.Nonempty.insert.test.containsInserted               ◉ Passed
    548. data.NatSet.Nonempty.nth.tests                                  ◉ Passed
    549. data.NatSet.Nonempty.randomChoice.test                          ◉ Passed
    550. data.NatSet.ordering.test                                       ◉ Passed
    551. data.NatSet.partition.test                                      ◉ Passed
    552. data.NatSet.randomChoice.test                                   ◉ Passed
    553. data.NatSet.size.test                                           ◉ Passed
    554. data.NatSet.split.test                                          ◉ Passed
    555. data.NatSet.splitContains.test                                  ◉ Passed
    556. data.NatSet.subsetCompare.test                                  ◉ Passed
    557. data.NatSet.toList.test                                         ◉ Passed
    558. data.NatSet.toListDescending.test                               ◉ Passed
    559. data.NatSet.union.test                                          ◉ Passed
    560. data.Set.breakOffMax.tests                                      ◉ Passed
    561. data.Set.breakOffMin.tests                                      ◉ Passed
    562. data.Set.delete.test                                            ◉ Passed
    563. data.Set.deletes.test                                           ◉ Passed
    564. data.Set.difference.test                                        ◉ Passed
    565. data.Set.dropSmallest.tests                                     ◉ Passed
    566. data.Set.elementAt.tests                                        ◉ Passed
    567. data.Set.flatMap.tests.associative                              ◉ Passed
    568. data.Set.flatMap.tests.unit                                     ◉ Passed
    569. data.Set.flatten.tests.associative                              ◉ Passed
    570. data.Set.flatten.tests.unit                                     ◉ Passed
    571. data.Set.foldLeft.tests.homomorphism                            ◉ Passed
    572. data.Set.foldRight.tests.homomorphism                           ◉ Passed
    573. data.Set.intersects.test                                        ◉ Passed
    574. data.Set.map.test                                               ◉ Passed
    575. data.Set.Nonempty.randomChoice.test                             ◉ Passed
    576. data.Set.randomChoice.test                                      ◉ Passed
    577. data.Set.singleton.test                                         ◉ Passed
    578. data.Set.unions.test                                            ◉ Passed
    579. data.Stream.all.test                                            ◉ Passed
    580. data.Stream.any.test                                            ◉ Passed
    581. data.Stream.changes.tests.multiple                              ◉ Passed
    582. data.Stream.changes.tests.pair                                  ◉ Passed
    583. data.Stream.changes.tests.repeatSingle                          ◉ Passed
    584. data.Stream.changes.tests.single                                ◉ Passed
    585. data.Stream.chunk.deprecated.tests                              ◉ Passed
    586. data.Stream.chunk.tests                                         ◉ Passed
    587. data.Stream.collate.test.testIntersperse                        ◉ Passed
    588. data.Stream.contains.test                                       ◉ Passed
    589. data.Stream.drop.test                                           ◉ Passed
    590. data.Stream.dropWhile!.tests.effects                            ◉ Passed
    591. data.Stream.dropWhile.tests.effects                             ◉ Passed
    592. data.Stream.filter.test                                         ◉ Passed
    593. data.Stream.filterMap.tests.filters                             ◉ Passed
    594. data.Stream.filterMap.tests.identityLaw                         ◉ Passed
    595. data.Stream.find.test                                           ◉ Passed
    596. data.Stream.flatMap.test                                        ◉ Passed
    597. data.Stream.fold.test                                           ◉ Passed
    598. data.Stream.foldBalanced.test                                   ◉ Passed
    599. data.Stream.foldWithResult.test                                 ◉ Passed
    600. data.Stream.iterate.tests.ex1                                   ◉ Passed
    601. data.Stream.map.test                                            ◉ Passed
    602. data.Stream.range.test.emptyRange                               ◉ Passed
    603. data.Stream.range.test.emptyRange2                              ◉ Passed
    604. data.Stream.range.test.listLike                                 ◉ Passed
    605. data.Stream.range.test.singletonRange                           ◉ Passed
    606. data.Stream.take.tests.completion                               ◉ Passed
    607. data.Stream.take.tests.earlyTermination                         ◉ Passed
    608. data.Stream.take.tests.pullMinimal                              ◉ Passed
    609. data.Stream.takeWhile.tests.worksLikeList                       ◉ Passed
    610. data.Stream.terminated.test                                     ◉ Passed
    611. data.Stream.tests.ex1                                           ◉ Passed
    612. data.Stream.toDelayedList.test.bidirectional                    ◉ Passed
    613. data.Stream.uncons.tests                                        ◉ Passed
    614. data.Trie.subtrie.tests                                         ◉ Passed
    615. data.Trie.union.tests.values                                    ◉ Passed
    616. data.Tuple.at1.tests.ex1                                        ◉ Passed
    617. data.Tuple.at2.tests.ex1                                        ◉ Passed
    618. data.Tuple.at3.tests.ex1                                        ◉ Passed
    619. data.Tuple.at3.tests.ex2                                        ◉ Passed
    620. data.Tuple.at4.tests.ex1                                        ◉ Passed
    621. data.Tuple.at4.tests.ex2                                        ◉ Passed
    622. data.Tuple.tests.ex1                                            ◉ Passed
    623. data.Tuple.tests.ex2                                            ◉ Passed
    624. data.Tuple.tests.tailExample                                    ◉ Passed
    625. Either.mapLeft.tests.ex1                                        ◉ Passed
    626. Either.mapRight.tests.ex1                                       ◉ Passed
    627. Float.ceiling.tests.adjunction                                  ◉ Passed
    628. Float.clamp.test                                                ◉ Passed
    629. Float.emod.truncatesTowardsZero                                 ◉ Passed
    630. Float.floor.tests.adjunction                                    ◉ Passed
    631. Float.fromHalfPrecision.tests.largestSubnormal                  ◉ Passed
    632. Float.fromHalfPrecision.tests.maxHalf                           ◉ Passed
    633. Float.fromHalfPrecision.tests.negativeOne                       ◉ Passed
    634. Float.fromHalfPrecision.tests.negativeZero                      ◉ Passed
    635. Float.fromHalfPrecision.tests.normal                            ◉ Passed
    636. Float.fromHalfPrecision.tests.notANumber                        ◉ Passed
    637. Float.fromHalfPrecision.tests.positiveOne                       ◉ Passed
    638. Float.fromHalfPrecision.tests.positiveZero                      ◉ Passed
    639. Float.fromHalfPrecision.tests.smallestNormal                    ◉ Passed
    640. Float.fromHalfPrecision.tests.subnormal                         ◉ Passed
    641. Float.fromSinglePrecision.tests.largestSubnormal                ◉ Passed
    642. Float.fromSinglePrecision.tests.maxSingle                       ◉ Passed
    643. Float.fromSinglePrecision.tests.negativeOne                     ◉ Passed
    644. Float.fromSinglePrecision.tests.negativeZero                    ◉ Passed
    645. Float.fromSinglePrecision.tests.normal                          ◉ Passed
    646. Float.fromSinglePrecision.tests.notANumber                      ◉ Passed
    647. Float.fromSinglePrecision.tests.positiveOne                     ◉ Passed
    648. Float.fromSinglePrecision.tests.positiveZero                    ◉ Passed
    649. Float.fromSinglePrecision.tests.smallestNormal                  ◉ Passed
    650. Float.fromSinglePrecision.tests.subnormal                       ◉ Passed
    651. Float.safeEmod.test                                             ◉ Passed
    652. Float.safeMod.test                                              ◉ Passed
    653. Float.safeRoundToWith.tests.gen                                 ◉ Passed
    654. Float.safeRoundToWith.tests.static                              ◉ Passed
    655. Float.toHalfPrecision.monotonic                                 ◉ Passed
    656. Float.toHalfPrecision.tests.largestSubnormal                    ◉ Passed
    657. Float.toHalfPrecision.tests.maxHalf                             ◉ Passed
    658. Float.toHalfPrecision.tests.negativeOne                         ◉ Passed
    659. Float.toHalfPrecision.tests.negativeZero                        ◉ Passed
    660. Float.toHalfPrecision.tests.normal                              ◉ Passed
    661. Float.toHalfPrecision.tests.notANumber                          ◉ Passed
    662. Float.toHalfPrecision.tests.positiveOne                         ◉ Passed
    663. Float.toHalfPrecision.tests.positiveZero                        ◉ Passed
    664. Float.toHalfPrecision.tests.smallestNormal                      ◉ Passed
    665. Float.toHalfPrecision.tests.subnormal                           ◉ Passed
    666. Float.toNat.tests.cornerCase                                    ◉ Passed
    667. Float.toNat.tests.roundtrip                                     ◉ Passed
    668. Float.toNat.tests.small                                         ◉ Passed
    669. Float.toSinglePrecision.monotonic                               ◉ Passed
    670. Float.toSinglePrecision.tests.largestSubnormal                  ◉ Passed
    671. Float.toSinglePrecision.tests.maxSingle                         ◉ Passed
    672. Float.toSinglePrecision.tests.negativeOne                       ◉ Passed
    673. Float.toSinglePrecision.tests.negativeZero                      ◉ Passed
    674. Float.toSinglePrecision.tests.normal                            ◉ Passed
    675. Float.toSinglePrecision.tests.notANumber                        ◉ Passed
    676. Float.toSinglePrecision.tests.positiveOne                       ◉ Passed
    677. Float.toSinglePrecision.tests.positiveZero                      ◉ Passed
    678. Float.toSinglePrecision.tests.smallestNormal                    ◉ Passed
    679. Float.toSinglePrecision.tests.subnormal                         ◉ Passed
    680. Function.tap.tests.t1                                           ◉ Passed
    681. Int.clamp.test                                                  ◉ Passed
    682. Int.decrement.test                                              ◉ Passed
    683. Int.div.tests.galois                                            ◉ Passed
    684. Int.div.tests.range                                             ◉ Passed
    685. Int.ediv.tests.adjunction                                       ◉ Passed
    686. Int.gcd.tests.abortOnBothZero                                   ◉ Passed
    687. Int.gcd.tests.commonDivisor                                     ◉ Passed
    688. Int.gcd.tests.multipleOfAnyCD                                   ◉ Passed
    689. Int.inRange.test                                                ◉ Passed
    690. Int.mod.test                                                    ◉ Passed
    691. Int.range.tests.invalid.descFromNeg                             ◉ Passed
    692. Int.range.tests.invalid.descFromPos                             ◉ Passed
    693. Int.range.tests.valid.ascFromNeg                                ◉ Passed
    694. Int.range.tests.valid.ascFromNeg2                               ◉ Passed
    695. Int.range.tests.valid.ascFromPos                                ◉ Passed
    696. IO.net.URI.Authority.toText.tests                               ◉ Passed
    697. IO.net.URI.parse.unquotePercentEncoded.impl.tests               ◉ Passed
    698. IO.net.URI.parse.unquotePercentEncodedPlus.impl.tests           ◉ Passed
    699. IO.net.URI.parse._internal.parseQuery.tests.percentPlusIsPlus   ◉ Passed
    700. IO.net.URI.parse._internal.parseQuery.tests.plusIsSpace         ◉ Passed
    701. IO.net.URI.Path.encode.char.test                                ◉ Passed
    702. IO.net.URI.Path.slash.tests                                     ◉ Passed
    703. IO.net.URI.pattern.decOctet.tests                               ◉ Passed
    704. IO.net.URI.pattern.ipv4Address.tests                            ◉ Passed
    705. IO.net.URI.Query.encode.test                                    ◉ Passed
    706. IO.net.URI.Query.encode.test.double.simple                      ◉ Passed
    707. IO.net.URI.Query.encode.test.empty                              ◉ Passed
    708. IO.net.URI.Query.encode.test.single.simple                      ◉ Passed
    709. IO.net.URI.Query.tests.addParamOrder                            ◉ Passed
    710. IO.net.URI.RawQuery.encode.tests                                ◉ Passed
    711. IO.net.URI.RawQuery.encode.tests.double.simple                  ◉ Passed
    712. IO.net.URI.Scheme.toText.tests                                  ◉ Passed
    713. IO.net.URI.tests.testEmptyFragment                              ◉ Passed
    714. IO.net.URI.tests.testPathDecode                                 ◉ Passed
    715. IO.net.URI.tests.testPathToText                                 ◉ Passed
    716. IO.net.URI.tests.testQueryParams                                ◉ Passed
    717. IO.net.URI.tests.testUriRoundTrip                               ◉ Passed
    718. IO.net.URI.toText.tests.complicatedUri                          ◉ Passed
    719. IO.net.URI.toText.tests.roundtrips                              ◉ Passed
    720. IO.net.URI.toText.tests.unisonDocs                              ◉ Passed
    721. math.Natural.gcd.tests.ex1                                      ◉ Passed
    722. math.Natural.parse!.tests.roundtrip                             ◉ Passed
    723. math.Natural.parse.tests.roundtrip                              ◉ Passed
    724. math.Natural.pow.tests.powerOfOne                               ◉ Passed
    725. math.Natural.pow.tests.powerOfPower                             ◉ Passed
    726. math.Natural.pow.tests.powerOfProduct                           ◉ Passed
    727. math.Natural.pow.tests.powerOfZero                              ◉ Passed
    728. math.Natural.pow.tests.product                                  ◉ Passed
    729. math.Natural.pow.tests.quotient                                 ◉ Passed
    730. math.Natural.tests.additionAssociative                          ◉ Passed
    731. math.Natural.tests.additionCommutative                          ◉ Passed
    732. math.Natural.tests.additionZero                                 ◉ Passed
    733. math.Natural.tests.distributiveLaw                              ◉ Passed
    734. math.Natural.tests.multiplicationAssociative                    ◉ Passed
    735. math.Natural.tests.multiplicationCommutative                    ◉ Passed
    736. math.Natural.tests.multiplicationZero                           ◉ Passed
    737. math.Natural.tests.subtraction                                  ◉ Passed
    738. math.Natural.tests.subtractLeftAdjoint                          ◉ Passed
    739. math.Natural.tests.zeroDivisors                                 ◉ Passed
    740. math.Natural.toNat.tests.roundtrip                              ◉ Passed
    741. mutable.Array.copy.tests.doesCopy                               ◉ Passed
    742. mutable.Array.copy.tests.doesNotCopyPastEndOfDestinationArray   ◉ Passed
    743. mutable.Scope.byteArrayOf.test                                  ◉ Passed
    744. Nat.-.tests.leftAdjoint                                         ◉ Passed
    745. Nat.changeBit.tests                                             ◉ Passed
    746. Nat.choose.tests.test1                                          ◉ Passed
    747. Nat.clamp.test                                                  ◉ Passed
    748. Nat.decrement.test                                              ◉ Passed
    749. Nat.factorial.tests.test1                                       ◉ Passed
    750. Nat.gcd.tests.abortOnBothZero                                   ◉ Passed
    751. Nat.gcd.tests.commonDivisor                                     ◉ Passed
    752. Nat.gcd.tests.multipleOfAnyCD                                   ◉ Passed
    753. Nat.inRange.test                                                ◉ Passed
    754. Nat.leadingOnes.tests                                           ◉ Passed
    755. Nat.modExp.tests                                                ◉ Passed
    756. Nat.msb.test                                                    ◉ Passed
    757. Nat.popCount.test                                               ◉ Passed
    758. Nat.reverseBits.test.hom                                        ◉ Passed
    759. Nat.reverseBits.test.iso                                        ◉ Passed
    760. Nat.takeLeftBits.test                                           ◉ Passed
    761. Nat.takeRightBits.test                                          ◉ Passed
    762. Nat.toBytesBigEndian.test.ex1                                   ◉ Passed
    763. Nat.toBytesLittleEndian.test.ex1                                ◉ Passed
    764. Nat.toBytesLittleEndian.test.prop                               ◉ Passed
    765. Natural.gcd.tests.ex1                                           ◉ Passed
    766. Optional.toList.tests.roundtrip                                 ◉ Passed
    767. Ordering.list.orderingBy.tests                                  ◉ Passed
    768. Ordering.medianOf3By.test                                       ◉ Passed
    769. Ordering.pair.orderingBy.tests                                  ◉ Passed
    770. Pattern.someUntil.tests.conflictingPatterns                     ◉ Passed
    771. Pattern.someUntil.tests.handlesEmptyInput                       ◉ Passed
    772. Pattern.someUntil.tests.handlesNoMatches                        ◉ Passed
    773. Pattern.someUntil.tests.happyPath                               ◉ Passed
    774. Pattern.someUntil.tests.overlappingPatterns                     ◉ Passed
    775. test.deprecated.gen.either.test                                 ◉ Passed
    776. test.deprecated.gen.optional.test                               ◉ Passed
    777. Text.alignCenterWith.test                                       ◉ Passed
    778. Text.charAt.tests                                               ◉ Passed
    779. Text.cons.tests                                                 ◉ Passed
    780. Text.fromUtf8.partial.tests                                     ◉ Passed
    781. Text.fromUtf8.stream.tests.invalidUtf8                          ◉ Passed
    782. Text.fromUtf8.stream.tests.success                              ◉ Passed
    783. Text.head.tests                                                 ◉ Passed
    784. Text.indexOf.tests.infix                                        ◉ Passed
    785. Text.join.tests.ex1                                             ◉ Passed
    786. Text.lines.tests.empty                                          ◉ Passed
    787. Text.lines.tests.leading                                        ◉ Passed
    788. Text.lines.tests.multi                                          ◉ Passed
    789. Text.lines.tests.single                                         ◉ Passed
    790. Text.lines.tests.trailing                                       ◉ Passed
    791. Text.pattern.spaces.tests                                       ◉ Passed
    792. Text.patterns.anyChar.test                                      ◉ Passed
    793. Text.patterns.asciiLetter.test                                  ◉ Passed
    794. Text.patterns.char.tests.alphanumeric                           ◉ Passed
    795. Text.patterns.char.tests.and                                    ◉ Passed
    796. Text.patterns.char.tests.not                                    ◉ Passed
    797. Text.patterns.char.tests.or                                     ◉ Passed
    798. Text.patterns.charUntil.tests.consumesAllMatches                ◉ Passed
    799. Text.patterns.charUntil.tests.handlesEmptyInput                 ◉ Passed
    800. Text.patterns.charUntil.tests.handlesNoMatches                  ◉ Passed
    801. Text.replaceAll.tests                                           ◉ Passed
    802. Text.reverse.tests.homomorphism                                 ◉ Passed
    803. Text.reverse.tests.isomorphism                                  ◉ Passed
    804. Text.slice.tests.example1                                       ◉ Passed
    805. Text.slice.tests.example2                                       ◉ Passed
    806. Text.slice.tests.largeEnd                                       ◉ Passed
    807. Text.slice.tests.largeStart                                     ◉ Passed
    808. Text.split.test                                                 ◉ Passed
    809. Text.startsWith.tests.isPrefix                                  ◉ Passed
    810. Text.takeWhile.tests.allMatch                                   ◉ Passed
    811. Text.takeWhile.tests.isLongestPrefix                            ◉ Passed
    812. Text.takeWhile.tests.isPrefix                                   ◉ Passed
    813. Text.trim.tests.all                                             ◉ Passed
    814. Text.trim.tests.edges                                           ◉ Passed
    815. Text.trim.tests.none                                            ◉ Passed
    816. Text.trimStart.tests                                            ◉ Passed
    817. Text.uncons.tests                                               ◉ Passed
    818. Text.unlines.roundtrip                                          ◉ Passed
    819. Text.unlines.tests.empty                                        ◉ Passed
    820. Text.unlines.tests.leading                                      ◉ Passed
    821. Text.unlines.tests.multi                                        ◉ Passed
    822. Text.unlines.tests.single                                       ◉ Passed
    823. Text.unlines.tests.trailing                                     ◉ Passed
    824. Text.unwords.test                                               ◉ Passed
    825. Text.words.test                                                 ◉ Passed
    826. time.Duration.asSeconds.tests.test1                             ◉ Passed
    827. time.Duration.tests.divNotTruncate.nn                           ◉ Passed
    828. time.Duration.tests.divNotTruncate.np                           ◉ Passed
    829. time.Duration.tests.divNotTruncate.pn                           ◉ Passed
    830. time.Duration.tests.divNotTruncate.pp                           ◉ Passed
    831. time.LocalDate.format.test                                      ◉ Passed
    832. time.LocalDate.range.test                                       ◉ Passed
    833. time.LocalDate.tests.comparison                                 ◉ Passed
    834. time.LocalDateTime.format.test                                  ◉ Passed
    835. time.LocalDateTime.tests.comparison                             ◉ Passed
    836. time.LocalTime.comparison                                       ◉ Passed
    837. time.LocalTime.tests.iso8601RoundTrips                          ◉ Passed
    838. time.OffsetDateTime.fromRFC1123.test                            ◉ Passed
    839. time.OffsetDateTime.fromRFC2822.tests.basic                     ◉ Passed
    840. time.OffsetDateTime.fromRFC2822.tests.noSeconds                 ◉ Passed
    841. time.OffsetDateTime.fromRFC2822.tests.obsoleteZones             ◉ Passed
    842. time.OffsetDateTime.fromRFC2822.tests.twoDigitYear1950          ◉ Passed
    843. time.OffsetDateTime.fromRFC2822.tests.twoDigitYear2049          ◉ Passed
    844. time.OffsetDateTime.tests.comparison                            ◉ Passed
    845. time.OffsetDateTime.toInstant.tests.rountrip                    ◉ Passed
    846. time.OffsetTime.tests.comparison                                ◉ Passed
    847. time.patterns.asctimeFormat.test                                ◉ Passed
    848. time.patterns.rfc7231DateTime.test850                           ◉ Passed
    849. time.patterns.rfc7231DateTime.testIMF                           ◉ Passed
    850. Universal.max.tests.associative                                 ◉ Passed
    851. Universal.max.tests.commutative                                 ◉ Passed
    852. Universal.max.tests.distributesOverMin                          ◉ Passed
    853. Universal.max.tests.idempotent                                  ◉ Passed
    854. Universal.max.tests.partialOrder                                ◉ Passed
    855. Universal.min.tests.absorption                                  ◉ Passed
    856. Universal.min.tests.associative                                 ◉ Passed
    857. Universal.min.tests.commutative                                 ◉ Passed
    858. Universal.min.tests.distributesOverMax                          ◉ Passed
    859. Universal.min.tests.idempotent                                  ◉ Passed
    860. Universal.min.tests.partialOrder                                ◉ Passed

  ✅ 860 test(s) passing

  Tip: Use view 1 to view the source of a test.

x/json> pull @unison/json/releases/latest

  ✅

  Successfully pulled into x/json, which was empty.

x/json> test

  ✅  







































    New test results:

    1.  core.Json.fromText.tests.emptyArray             ◉ Passed
    2.  core.Json.fromText.tests.emptyObject            ◉ Passed
    3.  core.Json.internal.numberPattern.tests          ◉ Passed
    4.  core.Json.tests.controlChars                    ◉ Passed
    5.  core.Json.tests.parsing                         ◉ Passed
    6.  core.Json.tests.relaxedRuntimeConsistency       ◉ Passed
    7.  core.Json.tests.roundTrips                      ◉ Passed
    8.  core.Json.tests.roundtripTextEsc1               ◉ Passed
    9.  core.Json.tests.runtimeReplacementConsistency   ◉ Passed
    10. core.Json.tests.unicodeHex                      ◉ Passed
    11. core.Json.tryUnconsText.tests.fix1              ◉ Passed
    12. Decoder.object.optionalAt.tests.absent          ◉ Passed
    13. Decoder.object.optionalAt.tests.present         ◉ Passed
    14. Decoder.tests.ex1                               ◉ Passed
    15. Decoder.tests.ex2                               ◉ Passed
    16. Decoder.tests.ex3                               ◉ Passed
    17. Decoder.tests.ex4                               ◉ Passed
    18. Decoder.tests.ex5                               ◉ Passed
    19. Decoder.tests.ex6                               ◉ Passed

  ✅ 19 test(s) passing

  Tip: Use view 1 to view the source of a test.

x/cloud> pull @unison/cloud/releases/latest

  ✅

  Successfully pulled into x/cloud, which was empty.

x/cloud> test

  ✅  





























    New test results:

    1.  durable.up.Bytes.toBase16.text.test             ◉ Passed
    2.  durable.up.Map.toStream.tests                   ◉ Passed
    3.  durable.up.Stream.distinctBy.tests              ◉ Passed
    4.  pool.toRemote.cachedFor.tests                   ◉ Passed
    5.  Remote.docs.run.tests.test1                     ◉ Passed
    6.  Remote.docs.run.tests.test2                     ◉ Passed
    7.  Remote.docs.run.tests.test3                     ◉ Passed
    8.  Remote.ResourcePool.tests.clearAfterSleepTest   ◉ Passed
    9.  Remote.ResourcePool.tests.reuseTest             ◉ Passed
    10. Remote.test.assertions.runAllPure               ◉ basic cancellation
                                                        ◉ basic program that doesn't use Remote
                                                        ◉ allowCancel allows an interrupt
                                                        ◉ cancel after await
                                                        ◉ cancel await
                                                        ◉ cancel awaits finalization
                                                        ◉ await awaits finalization
                                                        ◉ children get canceled
                                                        ◉ Remote ops: forkAt, near, far, await
                                                        ◉ forking here leaves you in the same location
                                                        ◉ finalizers should run with unique ID generation (they shouldn't reuse a seed)
                                                        ◉ The output of randomBytes should be reasonable
                                                        ◉ delete on a nonexistent ref doesn't fail
                                                        ◉ remote cancellation
                                                        ◉ scope runs finalization synchronously
                                                        ◉ Remote.scope propagates interruption
    11. Storage.doc.example.testDeletion                ◉ Passed
    12. Storage.doc.example.testFork                    ◉ Passed
    13. up.base.Nat.floorPowerOf2.tests                 ◉ Passed
    14. up.base.Ordering.product.tests                  ◉ Passed

  ✅ 29 test(s) passing

  Tip: Use view 1 to view the source of a test.

x/cloud> io.test internal.tests.cloud.runAllLocally

    New test results:

    1. cloud.runAllLocally   ◉ Passed
                             ◉ basic cancellation
                             ◉ basic program that doesn't use Remote
                             ◉ allowCancel allows an interrupt
                             ◉ cancel after await
                             ◉ cancel await
                             ◉ cancel awaits finalization
                             ◉ await awaits finalization
                             ◉ children get canceled
                             ◉ Remote ops: forkAt, near, far, await
                             ◉ forking here leaves you in the same location
                             ◉ finalizers should run with unique ID generation (they shouldn't reuse a seed)
                             ◉ The output of randomBytes should be reasonable
                             ◉ delete on a nonexistent ref doesn't fail
                             ◉ remote cancellation
                             ◉ scope runs finalization synchronously
                             ◉ Remote.scope propagates interruption
                             ◉ bug in fork is returned as a normal failure
                             ◉ bug in tryScope is returned as a normal failure
                             ◉ fork finalizers run when there is a runtime exception
                             ◉ scope finalizers run when there is a runtime exception
                             ◉ Remote.time.now should be reasonable
                             ◉ basic submit
                             ◉ blobs CRUD
                             ◉ environment CRUD
                             ◉ deployment CRUD
                             ◉ Scratch operations
                             ◉ service name CRUD
                             ◉ service name assign via Id
                             ◉ deploy service
                             ◉ deploy websocket service
                             ◉ expose service
                             ◉ database crud
                             ◉ basic State
                             ◉ state via service calls
                             ◉ batch reads
                             ◉ Ordered table constructor stability
                             ◉ daemon CRUD

  ✅ 38 test(s) passing

  Tip: Use view 1 to view the source of a test.

x/orderator> pull @pchiusano/orderator/releases/latest

  ✅

  Successfully pulled into x/orderator, which was empty.

x/orderator> test

  ✅  





















    New test results:

    1.  Orderator.distinctBy.tests               ◉ Passed
    2.  Orderator.fullOuterJoinBy.tests.tricky   ◉ Passed
    3.  Orderator.isEmpty.tests                  ◉ Passed
    4.  Orderator.natsFrom.tests                 ◉ Passed
    5.  Orderator.range.tests                    ◉ Passed
    6.  Orderator.size.tests                     ◉ Passed
    7.  Orderator.sortBy.tests                   ◉ Passed
    8.  Orderator.tests                          ◉ Passed
    9.  Orderator.unionBy.tests.tricky           ◉ Passed
    10. util.gallopingSearch.tests               ◉ Passed

  ✅ 10 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
