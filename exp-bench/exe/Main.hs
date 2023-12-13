module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.Csv
import qualified Data.Vector as V
import System.Directory
import System.FilePath
import Data.List.Split (splitOn)

-- Define a structure for your data
data Benchmark = Benchmark {
    mean :: Double,
    stddev :: Double
} deriving Show

instance FromNamedRecord Benchmark where
    parseNamedRecord r = Benchmark <$> r .: BS.pack "Mean" <*> r .: BS.pack "Stddev"

-- Function to process a single CSV file
processFile :: FilePath -> IO (Int, Int, Double, Double)
processFile filepath = do
    csvData <- BL.readFile filepath
    case decodeByName csvData of
        Left err -> error err
        Right (_, v) ->
            let means = V.map mean v
                stddevs = V.map stddev v
                meanOfMeans = V.sum means / fromIntegral (V.length means)
                errorOnMean = sqrt (V.sum (V.map (^2) stddevs)) / fromIntegral (V.length means)
                -- Extracting base size and exponent size from filename
                parts = splitOn "_" . takeBaseName $ filepath
                baseSize = read (parts !! 2) :: Int
                exponentSize = read (parts !! 3) :: Int
            in return (baseSize, exponentSize, meanOfMeans, errorOnMean)

-- Function to write results to a CSV file
writeResults :: FilePath -> [(Int, Int, Double, Double)] -> IO ()
writeResults outputPath results = do
    let header = ["Base Size", "Exponent Size", "Mean", "Stddev"]
    let records = map (\(b, e, m, s) -> [show b, show e, show m, show s]) results
    let csvData = encode (header : records)
    BL.writeFile outputPath csvData

writeResultsForPrimeSize :: Int -> IO ()
writeResultsForPrimeSize primeSize = do
    let directory = "data/" ++ show primeSize
    files <- listDirectory directory
    results <- mapM (processFile . (directory </>)) files
    writeResults (show primeSize ++ "-bit-prime-modulus.csv") results

-- Main function to process all files in a directory
main :: IO ()
main = do
    writeResultsForPrimeSize 128
    writeResultsForPrimeSize 256
    writeResultsForPrimeSize 384
    writeResultsForPrimeSize 512