# modular exponentiation builtin benchmark
This repository contains the code for benchmarking a modular exponentiation function of signature
```haskell
modExp :: Integer -> Integer -> Integer -> Integer
```

# Results
This repository has two data sets, data-bignum and data-gmp. The former is a benchmark using the `ghc-bignum` [implementation](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Integer.html#v:integerPowMod-35-), and the latter the `cryptonite` [implementation](https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-Number-ModArithmetic.html#v:expSafe). 

To view both datasets, enter a shell via `nix develop` and open their plots via `python3 plot.py`. Doing so, will open eight windows that contain the graphs of the two tests derived from the `*-bit-prime-modulus-{bignum/gmp}.csv` files. These benchmarks are preformed on a beelink GTR7 (CPU: AMD Ryzen 7 7840hs, RAM: 64GB DDR5 Dual SO-DIMM at 5600MHz).

If you want to regenerate these test on your machine, make sure the `data` folder has nothing in it (as old data will be overwritten). Then, make sure that in `exp-bench/src/Main.hs` the four `writeBenchResults` are uncommented, these benchmarks take around 4 hours on the above machine. The results can be aggregated (with error propagation) via the `exp-bench/exe/Main.hs` executable. Both executables can also be run via nix, using `nix run .#exp-bench:exe:exp-bench` and `nix run .#exp-bench:exe:aggregate-data`.