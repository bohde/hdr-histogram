module Main where

import           Criterion.Main                 (bgroup, defaultMain)
import qualified Data.HdrHistogram.MutableBench as M

main :: IO ()
main = defaultMain [
  bgroup "Data.HdrHistogram.Mutable" M.benchmarks
  ]
