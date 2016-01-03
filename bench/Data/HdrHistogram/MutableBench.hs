{-# LANGUAGE DataKinds #-}
module Data.HdrHistogram.MutableBench (benchmarks) where

import           Control.Monad             (forM_)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Criterion.Main
import           Data.HdrHistogram.Config
import qualified Data.HdrHistogram.Mutable as MH

type Config' = Config 1 3600000000 3

insertRange :: PrimMonad m => Int -> MH.Histogram (PrimState m) Config' Int Int -> m (MH.Histogram (PrimState m) Config' Int Int)
insertRange r h = do
  forM_ [1..r] $ MH.record h
  return h

benchmarks :: [Benchmark]
benchmarks = [
  env MH.new $ \h ->
    bgroup "insert" [
       bench "1" $ nfIO (MH.record h 12000 >> return h),
       bench "10" $ nfIO (insertRange 10 h),
       bench "100" $ nfIO (insertRange 100 h),
       bench "1000" $ nfIO (insertRange 1000 h),
       bench "10000" $ nfIO (insertRange 10000 h)
      ]
  ]
