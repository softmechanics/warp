#!/usr/bin/env runghc

import HSH
import Text.Printf
import System.Process
import System.Posix.Process (forkProcess, executeFile, getProcessStatus)
import System.Posix.Signals
import System.IO
import System.IO.Unsafe
import System.Directory
import Control.Applicative ((<$>))
import Text.Regex.Posix
import Data.List
import Data.Maybe (fromJust)
import Data.Either (rights, lefts)
import qualified Control.Exception as E

data BenchmarkResults = BenchmarkResults
     { testName :: String
     , testFlags :: [String]
     , httperfResults :: HttperfResults
     , rtsResults :: RtsResults
     }
  deriving (Show)

data HttperfResults = HttperfResults
     { httperfReport :: String
     , minResponseRate :: Double
     , maxResponseRate :: Double
     , avgResponseRate :: Double
     }
  deriving (Show)

data RtsResults = RtsResults
     { rtsReport :: String
     , bytesAllocated :: Integer
     , mutCpuSeconds :: Double
     , gcCpuSeconds :: Double
     }
  deriving (Show)

httperfArgs = words "--hog --num-conns 3 --num-calls 200 --burst-length 20 --port 3000 --rate 1000" 

ghcCmd name ghcOpts = 
  "ghc -hide-package network-2.3 -hide-package network-2.3.0.1 --make -O3 -threaded -rtsopts -fforce-recomp " ++ unwords (name:ghcOpts)

exec name rtsFile =
  executeFile exe False args Nothing
  where exe = "./" ++ name
        args = words $ printf "+RTS -N3 -A4m -t%s --machine-readable" rtsFile

parseHttperfResults :: String -> HttperfResults
parseHttperfResults report = HttperfResults report minRR maxRR avgRR
  where line = head $ filter (isPrefixOf "Reply rate") $ lines report
        minRR = parseRR "min"
        maxRR = parseRR "max"
        avgRR = parseRR "avg"
        parseRR rr = read $ head $ groups line $ rr ++ " ([0-9.]*)"

        groups src pat = 
          let (_,_,_,gs) = src =~ pat :: (String,String,String,[String])
          in gs

parseRtsResults :: String -> RtsResults
parseRtsResults report = RtsResults report bytes mut cpu
  where bytes = get "bytes allocated"
        mut = get "mutator_cpu_seconds"
        cpu = get "GC_cpu_seconds"
        get :: Read a => String -> a
        get = read . fromJust . flip lookup stats
        stats = read $ dropWhile (/= '\n') report :: [(String, String)]

benchmark :: String -> [String] -> IO (Either E.SomeException BenchmarkResults)
benchmark name ghcOpts = E.try $ do
  let rtsFile = "./" ++ name ++ ".stats"

  printf "------------------------ %s (%s) --------------------------\n" name (unwords ghcOpts)
  -- build
  _ <- system $ ghcCmd name ghcOpts

  -- run
  pid <- forkProcess $ exec name rtsFile
  httperfOut <- readProcess "/usr/bin/httperf" httperfArgs []

  -- stop
  signalProcess sigINT pid
  _ <- getProcessStatus True True pid

  rts <- readFile rtsFile 
  --removeFile rtsFile
  putStrLn rts

  -- kill, in case it hung
  E.catch (signalProcess sigKILL pid) (\e -> let _ = e :: E.SomeException in return ())

  return $ BenchmarkResults name ghcOpts 
             (parseHttperfResults httperfOut)
             (parseRtsResults rts)

benchmarkResultsCSV :: [BenchmarkResults] -> [String]
benchmarkResultsCSV results = header : map go results
  where 
    header = 
      "Name, GHC Options, Min Response Rate, Max Response Rate, " ++
      "Avg Response Rate, Bytes Allocated, Mut CPU Seconds, GC CPU Seconds"

    go (BenchmarkResults n os (HttperfResults _ c1 c2 c3) (RtsResults _ c4 c5 c6)) = 
      printf "%s,%s,%f,%f,%f,%d,%f,%f" n (unwords os) c1 c2 c3 c4 c5 c6

benchmarkAll :: [(String, [String])] -> IO [BenchmarkResults]
benchmarkAll tests = do
  es <- mapM (uncurry benchmark) tests
  mapM_ print $ lefts es
  return $ rights es

-- test each with these options
takeHeadersOptions = [[], ["-DTAKE_UNTIL_BLANK"]]
iterSocketOptions = [[] , ["-DITER_SOCKET_ITER_BUILDER"]]
chunkedOptions = [[] , ["-DCHUNKED_RESPONSE"]]

testNames = [
    "pong"
  , "bigBuilder"
  , "bigEnumerator"
  , "bigtable-single"
  , "bigtable-stream"
  ]

tests = [(name, headOpt ++ chunkOpt ++ sockOpt) 
        | name <- testNames
        , headOpt <- takeHeadersOptions
        , chunkOpt <- chunkedOptions
        , sockOpt <- iterSocketOptions
        ]

printTests = sequence_ $ zipWith go [0..] tests
  where go n (name, opts) = printf "%2d: %s (%s)\n" (n::Int) name (unwords opts)

selectTests ['a':_] = tests
selectTests is = go is
  where go [] = []
        go (i:is) = tests !! read i : go is

menu = do
  printTests
  putStrLn "Enter tests to run by number, separated by spaces (or [a]ll)"
  input <- words <$> getLine
  return $ selectTests input

main = do
  tests' <- menu
  rs <- benchmarkAll tests'
  mapM_ putStrLn $ benchmarkResultsCSV rs

