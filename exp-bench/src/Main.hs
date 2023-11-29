module Main where

import Criterion.Main
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Random (getRandomBytes)
import Crypto.Number.Serialize (os2ip)
import Data.ByteString (ByteString)

-- Function to generate a random integer of a given byte size
randomInteger :: Int -> IO Integer
randomInteger size = os2ip <$> (getRandomBytes size :: IO ByteString)

-- Setup environment for benchmarks
setupEnv :: Integer -> Int -> IO (Integer, Integer, Integer)
setupEnv modulus modulusSize = do
    base <- randomInteger modulusSize
    exponent <- randomInteger modulusSize
    return (base, exponent, modulus)

-- Benchmark for expFast with specific modulus size
modExpFastBenchmark :: Int -> (Integer, Integer, Integer) -> Benchmark
modExpFastBenchmark modulusSize (base, exponent, modulus) =
    bench ("expFast of " ++ show modulusSize ++ " bytes") $ 
    whnf (expFast base exponent) modulus

main :: IO ()
main = do
  -- Pre-generate random numbers for each modulus size
  env256 <- setupEnv 52435875175126190479447740508185965837690552500527637822603658699938581184513 32
  env381 <- setupEnv 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787 48

  -- Run benchmarks
  defaultMain [
    bgroup "expFast" [
      modExpFastBenchmark 32 env256,
      modExpFastBenchmark 48 env381
        ]
    ]
