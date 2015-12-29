{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Data.HdrHistogram.MutableBench (benchmarks) where

import           Control.Monad             (replicateM_)
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
new = do
  MH.histogram config'

insert :: PrimMonad m => Int -> (MH.Histogram (PrimState m) Int Int) -> m (MH.Histogram (PrimState m) Int Int)
insert r h = do
  replicateM_ r $ do
    MH.record h 12000
  return h

benchmarks :: [Benchmark]
benchmarks = [
  env new $ \ ~h ->
    bgroup "insert" [
      bench "1" $ nfIO (MH.record h 12000 >> return h),
      bench "10" $ nfIO (insert 10 h),
      bench "100" $ nfIO (insert 100 h),
      bench "1000" $ nfIO (insert 1000 h),
      bench "10000" $ nfIO (insert 10000 h)
      ]
  ]
