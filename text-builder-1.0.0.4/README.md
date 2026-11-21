# Summary

Fast strict text builder and simple type-safe formatting library for Haskell.

# Performance

The benchmarks measure the monoidal composition of builders constructed from `Text` simulating left- and right-biased appends and `mconcat`.

The results show the following:

- It's 2-3 times faster than the `Data.Text.Lazy.Builder` supplied with the "text" package.
- It's 1.25 times faster than the `Data.Text.Encoding.StrictTextBuilder` which has recently arrived in "text".
- It's 1.2-2 times faster than "text-builder-linear".
- It's 1.1 times slower than `Data.Text.Text` in case of using `mconcat` over texts. In all other cases `Text` is slower and slows down exponentially, which is not surprising.

In the years of existence of this package multiple user-stories have been collected proving the performance boosts after switching to it.

```
All
  Competition
    Left-biased mappend
      100B
        TextBuilder.TextBuilder:              OK
          187  ns ±  14 ns, 960 B  allocated,   0 B  copied, 6.0 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          215  ns ±  17 ns, 1.2 KB allocated,   0 B  copied, 6.0 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          510  ns ±  27 ns, 2.6 KB allocated,   1 B  copied, 6.0 MB peak memory
        Data.Text.Text:                       OK
          216  ns ±  15 ns, 1.6 KB allocated,   0 B  copied, 6.0 MB peak memory
        Data.Text.Lazy.Text:                  OK
          424  ns ±  30 ns, 4.2 KB allocated,   2 B  copied, 6.0 MB peak memory
        Data.Text.Builder.Linear:             OK
          368  ns ±  26 ns, 1.3 KB allocated,   0 B  copied, 6.0 MB peak memory
      1kB
        TextBuilder.TextBuilder:              OK
          1.54 μs ± 144 ns, 8.3 KB allocated,   8 B  copied, 6.0 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          1.85 μs ± 131 ns,  12 KB allocated,  10 B  copied, 6.0 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          4.66 μs ± 458 ns,  24 KB allocated,  31 B  copied, 6.0 MB peak memory
        Data.Text.Text:                       OK
          3.80 μs ± 257 ns, 104 KB allocated,  85 B  copied, 6.0 MB peak memory
        Data.Text.Lazy.Text:                  OK
          24.5 μs ± 1.7 μs, 358 KB allocated, 451 B  copied, 6.0 MB peak memory
        Data.Text.Builder.Linear:             OK
          3.04 μs ± 213 ns,  12 KB allocated,   6 B  copied, 6.0 MB peak memory
      10kB
        TextBuilder.TextBuilder:              OK
          15.5 μs ± 1.2 μs,  82 KB allocated, 175 B  copied, 8.0 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          18.3 μs ± 536 ns, 121 KB allocated, 439 B  copied, 8.0 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          47.7 μs ± 3.4 μs, 242 KB allocated, 1.9 KB copied, 8.0 MB peak memory
        Data.Text.Text:                       OK
          355  μs ±  19 μs, 9.6 MB allocated,  19 KB copied,  13 MB peak memory
        Data.Text.Lazy.Text:                  OK
          2.63 ms ± 251 μs,  34 MB allocated, 264 KB copied,  13 MB peak memory
        Data.Text.Builder.Linear:             OK
          30.6 μs ± 1.7 μs, 110 KB allocated, 202 B  copied,  13 MB peak memory
      100kB
        TextBuilder.TextBuilder:              OK
          201  μs ±  14 μs, 1.0 MB allocated,  14 KB copied,  13 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          195  μs ±  19 μs, 1.2 MB allocated,  39 KB copied,  13 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          520  μs ±  30 μs, 2.4 MB allocated, 174 KB copied,  13 MB peak memory
        Data.Text.Text:                       OK
          28.1 ms ± 2.0 ms, 954 MB allocated, 476 KB copied,  25 MB peak memory
        Data.Text.Lazy.Text:                  OK
          616  ms ±  34 ms, 4.7 GB allocated, 136 MB copied,  25 MB peak memory
        Data.Text.Builder.Linear:             OK
          361  μs ±  33 μs, 1.5 MB allocated,  15 KB copied,  25 MB peak memory
    Right-biased mappend
      100B
        TextBuilder.TextBuilder:              OK
          188  ns ±  13 ns, 960 B  allocated,   0 B  copied,  25 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          231  ns ±  17 ns, 1.2 KB allocated,   0 B  copied,  25 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          500  ns ±  34 ns, 2.6 KB allocated,   1 B  copied,  25 MB peak memory
        Data.Text.Text:                       OK
          214  ns ±  18 ns, 1.6 KB allocated,   0 B  copied,  25 MB peak memory
        Data.Text.Lazy.Text:                  OK
          283  ns ±  13 ns, 1.8 KB allocated,   0 B  copied,  25 MB peak memory
        Data.Text.Builder.Linear:             OK
          363  ns ±  19 ns, 1.3 KB allocated,   0 B  copied,  25 MB peak memory
      1kB
        TextBuilder.TextBuilder:              OK
          1.59 μs ± 123 ns, 8.3 KB allocated,   7 B  copied,  25 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          1.98 μs ± 120 ns,  12 KB allocated,   9 B  copied,  25 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          4.66 μs ± 281 ns,  24 KB allocated,  30 B  copied,  25 MB peak memory
        Data.Text.Text:                       OK
          3.76 μs ±  72 ns, 104 KB allocated,  79 B  copied,  25 MB peak memory
        Data.Text.Lazy.Text:                  OK
          2.23 μs ± 215 ns,  17 KB allocated,  29 B  copied,  25 MB peak memory
        Data.Text.Builder.Linear:             OK
          3.03 μs ± 249 ns,  12 KB allocated,  11 B  copied,  25 MB peak memory
      10kB
        TextBuilder.TextBuilder:              OK
          15.6 μs ± 1.0 μs,  82 KB allocated, 205 B  copied,  25 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          32.7 μs ± 2.0 μs, 185 KB allocated, 556 B  copied,  25 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          47.6 μs ± 4.1 μs, 242 KB allocated, 2.1 KB copied,  25 MB peak memory
        Data.Text.Text:                       OK
          437  μs ±  14 μs, 9.6 MB allocated,  22 KB copied,  28 MB peak memory
        Data.Text.Lazy.Text:                  OK
          24.9 μs ± 1.0 μs, 168 KB allocated, 1.9 KB copied,  28 MB peak memory
        Data.Text.Builder.Linear:             OK
          30.4 μs ± 1.7 μs, 110 KB allocated, 427 B  copied,  28 MB peak memory
      100kB
        TextBuilder.TextBuilder:              OK
          176  μs ±  11 μs, 820 KB allocated,  18 KB copied,  28 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          363  μs ±  34 μs, 1.8 MB allocated,  51 KB copied,  28 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          561  μs ±  41 μs, 2.4 MB allocated, 191 KB copied,  28 MB peak memory
        Data.Text.Text:                       OK
          31.4 ms ± 1.5 ms, 954 MB allocated, 379 KB copied,  36 MB peak memory
        Data.Text.Lazy.Text:                  OK
          315  μs ±  13 μs, 1.6 MB allocated, 182 KB copied,  36 MB peak memory
        Data.Text.Builder.Linear:             OK
          333  μs ±  26 μs, 1.3 MB allocated,  34 KB copied,  36 MB peak memory
    mconcat
      100B
        TextBuilder.TextBuilder:              OK
          125  ns ± 6.6 ns, 496 B  allocated,   0 B  copied,  36 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          166  ns ±  13 ns, 1.0 KB allocated,   0 B  copied,  36 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          324  ns ±  27 ns, 2.1 KB allocated,   0 B  copied,  36 MB peak memory
        Data.Text.Text:                       OK
          117  ns ± 7.2 ns, 280 B  allocated,   0 B  copied,  36 MB peak memory
        Data.Text.Lazy.Text:                  OK
          212  ns ±  17 ns, 1.7 KB allocated,   0 B  copied,  36 MB peak memory
        Data.Text.Builder.Linear:             OK
          175  ns ±  13 ns, 936 B  allocated,   0 B  copied,  36 MB peak memory
      1kB
        TextBuilder.TextBuilder:              OK
          1.21 μs ± 104 ns, 3.6 KB allocated,   1 B  copied,  36 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          1.69 μs ± 117 ns, 9.8 KB allocated,  10 B  copied,  36 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          3.04 μs ± 301 ns,  20 KB allocated,  16 B  copied,  36 MB peak memory
        Data.Text.Text:                       OK
          1.07 μs ±  74 ns, 2.0 KB allocated,   1 B  copied,  36 MB peak memory
        Data.Text.Lazy.Text:                  OK
          1.76 μs ± 154 ns,  16 KB allocated,  20 B  copied,  36 MB peak memory
        Data.Text.Builder.Linear:             OK
          1.42 μs ±  58 ns, 8.0 KB allocated,   3 B  copied,  36 MB peak memory
      10kB
        TextBuilder.TextBuilder:              OK
          15.0 μs ± 838 ns,  35 KB allocated,   5 B  copied,  36 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          32.0 μs ± 2.3 μs, 162 KB allocated, 563 B  copied,  36 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          34.8 μs ± 1.7 μs, 202 KB allocated, 927 B  copied,  36 MB peak memory
        Data.Text.Text:                       OK
          10.6 μs ± 899 ns,  20 KB allocated,   5 B  copied,  36 MB peak memory
        Data.Text.Lazy.Text:                  OK
          22.0 μs ± 1.7 μs, 160 KB allocated, 1.3 KB copied,  36 MB peak memory
        Data.Text.Builder.Linear:             OK
          16.3 μs ± 1.4 μs,  71 KB allocated,  13 B  copied,  36 MB peak memory
      100kB
        TextBuilder.TextBuilder:              OK
          176  μs ±  15 μs, 352 KB allocated,  55 B  copied,  36 MB peak memory
        Data.Text.Encoding.StrictTextBuilder: OK
          437  μs ±  21 μs, 1.8 MB allocated,  78 KB copied,  36 MB peak memory
        Data.Text.Lazy.Builder.Builder:       OK
          333  μs ±  26 μs, 2.0 MB allocated,  84 KB copied,  36 MB peak memory
        Data.Text.Text:                       OK
          106  μs ±  10 μs, 195 KB allocated,  60 B  copied,  36 MB peak memory
        Data.Text.Lazy.Text:                  OK
          264  μs ±  15 μs, 1.6 MB allocated,  97 KB copied,  36 MB peak memory
        Data.Text.Builder.Linear:             OK
          170  μs ± 8.1 μs, 952 KB allocated, 166 B  copied,  36 MB peak memory
  Features
    intercalate
      TextBuilder:                            OK
        420  ns ±  28 ns, 3.8 KB allocated,   1 B  copied,  36 MB peak memory
      Text:                                   OK
        143  ns ± 9.9 ns, 824 B  allocated,   0 B  copied,  36 MB peak memory
    binary:                                   OK
      62.8 ns ± 3.7 ns, 152 B  allocated,   0 B  copied,  36 MB peak memory
    octal:                                    OK
      25.1 ns ± 1.7 ns,  96 B  allocated,   0 B  copied,  36 MB peak memory
    decimal:                                  OK
      48.5 ns ± 3.8 ns, 560 B  allocated,   0 B  copied,  36 MB peak memory
    hexadecimal:                              OK
      20.8 ns ± 2.1 ns, 104 B  allocated,   0 B  copied,  36 MB peak memory
```

# How it works

It constructs text in two phases. In the first one it estimates the size of the byte array and in the second one it allocates it once and populates it in one go.
