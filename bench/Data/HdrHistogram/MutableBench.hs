module Data.HdrHistogram.MutableBench (benchmarks) where

import           Control.Monad             (forM_)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Criterion.Main
import           Data.HdrHistogram.Config
import qualified Data.HdrHistogram.Mutable as MH

fig :: SignificantFigures
fig = case significantFigures 3 of
  Left s -> error s
  Right a -> a

config' :: HistogramConfig Int
config' = config (1 :: Int) 3600000000 fig

new :: PrimMonad m => m (MH.Histogram (PrimState m) Int Int)
new = MH.histogram config'

insertRange :: PrimMonad m => Int -> MH.Histogram (PrimState m) Int Int -> m (MH.Histogram (PrimState m) Int Int)
insertRange r h = do
  forM_ [1..r] $ MH.record h
  return h

benchmarks :: [Benchmark]
benchmarks = [
  env new $ \h ->
    bgroup "insert" [
       bench "1" $ nfIO (MH.record h 12000 >> return h),
       bench "10" $ nfIO (insertRange 10 h),
       bench "100" $ nfIO (insertRange 100 h),
       bench "1000" $ nfIO (insertRange 1000 h),
       bench "10000" $ nfIO (insertRange 10000 h)
      ]
  ]
