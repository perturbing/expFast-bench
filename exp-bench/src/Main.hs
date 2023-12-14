{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Criterion.Main
import Criterion.Types
import Crypto.Random (getRandomBytes)
import Crypto.Number.Prime (generatePrime)
import Crypto.Number.Serialize (os2ip)
import Data.ByteString (ByteString)
import Control.Monad (replicateM)
-- import Crypto.Number.ModArithmetic (expFast)

import GHC.Num.Integer (integerPowMod#,integerToNatural, integerFromNatural)

-- Function to compute the modular exponentiation
expFast2 :: Integer -> Integer -> Integer -> Integer
expFast2 base exp modulus 
    | modulus == 0 = 0
    | otherwise = case integerPowMod# base exp (integerToNatural modulus) of
        (# n | #) -> integerFromNatural n
        (# | () #) -> error "expFast2 failed"

randomInteger :: Int -> IO Integer
randomInteger size = os2ip <$> (getRandomBytes size :: IO ByteString)

setupEnv :: Int -> Int -> Int -> IO (Integer, Integer, Integer)
setupEnv pSizeBits bSize eSize = do
    if bSize < 0
        then if eSize < 0
            then do
                modulus <- generatePrime pSizeBits
                base <- randomInteger (abs bSize)
                exponent <- randomInteger (abs eSize)
                return (-base, -exponent, modulus)
            else do
                modulus <- generatePrime pSizeBits
                base <- randomInteger (abs bSize)
                exponent <- randomInteger eSize
                return (-base, exponent, modulus)
        else if eSize < 0
            then do
                modulus <- generatePrime pSizeBits
                base <- randomInteger bSize
                exponent <- randomInteger (abs eSize)
                return (base, exponent, modulus)
        else
            do modulus <- generatePrime pSizeBits
               base <- randomInteger bSize
               exponent <- randomInteger eSize
               return (base, exponent, modulus)

modExpFastBenchmark :: Int -> (Integer, Integer, Integer) -> Benchmark
modExpFastBenchmark modulusSize (base, exponent, modulus) =
    bench (show modulusSize ++ " bytes") $
    whnf (\(x,y,z) -> expFast2 x y z) (base, exponent, modulus)

writeBench :: Int -> Int -> Int -> IO ()
writeBench psize bsize esize = do
    let myConfig = defaultConfig { csvFile = Just ("data/"<>show psize <> "/benchmark_results_"<> show bsize <> "_" <> show esize <> ".csv"), verbosity = Quiet }
    -- set here how many times you want to run the benchmark per setup (baseSize, exponentSize, modulusSize)
    env <- replicateM 1 (setupEnv psize bsize esize)
    defaultMainWith myConfig [
        bgroup "pSize: " (map (modExpFastBenchmark psize) env)
        ]

writeBenchResults :: Int -> IO ()
writeBenchResults psize = do
    let psizeBits = psize * 8
    let list = [ (x,y) | x <- [-64, -48 .. 64], y <- [-64, -48 .. 64]]
    mapM_ (uncurry (writeBench psizeBits)) list

main :: IO ()
main = do
    print "Running benchmarks..."
    writeBenchResults 16
    writeBenchResults 32
    writeBenchResults 48
    writeBenchResults 64
    print "benchmarking uncommented in the code to not overwrite the results"