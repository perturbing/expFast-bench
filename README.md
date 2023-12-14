# modular exponentiation builtin benchmark
This repository contains the code for benchmarking a modular exponentiation function of signature
```haskell
modExp :: Integer -> Integer -> Integer -> Integer
```

# Results
This library has two data sets, data and data-gmp. The former is the a benchmark using the `ghc-bignum` [implementation](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Integer.html#v:integerPowMod-35-), and the latter the `cryptonite` [implementation](https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-Number-ModArithmetic.html#v:expSafe). 

To view both datasets, enter a shell via `nix develop` and open their plots via `python3 plot.py`. Doing so, will open eight windows that contain the graphs of the.

If you want to regenerate these test on your machine, move the `data` folder and create an empty one with the same folder structure. Then in `exp-bench/src/Main.hs` uncomment the four `writeBenchResults`, these benchemarks take around 4 hours. The results can be aggregated (with error propagation) via the the `exp-bench/exe/Main.hs` executable. Both can also be run via nix, use `nix run .#exp-bench:exe:exp-bench` and `nix run .#exp-bench:exe:aggregate-data`.