{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram (
  Histogram, histogram, record, percentile, config, significantFigures
  ) where

import           Data.Bits                (Bits, FiniteBits)
import           Data.HdrHistogram.Config
import           Data.Vector.Unboxed      ((!), (//))
import qualified Data.Vector.Unboxed      as U


data Histogram a b = Histogram {
  _config    :: HistogramConfig a,
  totalCount :: b,
  counts     :: U.Vector b
} deriving (Eq, Show)


histogram :: (U.Unbox b, Integral b) => HistogramConfig a -> Histogram a b
histogram config' = Histogram {
  _config = config',
  totalCount = 0,
  counts = U.replicate (countsLen config') 0
  }


record :: (U.Unbox b, Integral b, Integral a, FiniteBits a) => Histogram a b -> a -> Histogram a b
record h val = recordValues h val 1


recordValues :: (U.Unbox b, Integral b, Integral a, FiniteBits a) => Histogram a b -> a -> b -> Histogram a b
recordValues h val count = h {
    totalCount = totalCount h + count,
    counts = counts h // [(index, (counts h ! index) + count)]
    }
  where
    index = indexForValue (_config h) val


merge :: Histogram a b -> Histogram a b -> Histogram a b
merge = undefined

recordCorrectedValues :: Integral a => Histogram a b -> a -> a -> Histogram a b
recordCorrectedValues = undefined

percentile :: (Integral a, Integral b, U.Unbox b, Bits a) => Histogram a b -> Float -> a
percentile h q = case U.find ((>= count) . snd) totals of
  Nothing -> 0
  Just (i, _) -> upper $ valueAtIndex (_config h) i
  where
    q' = min q 100
    count = floor $ (q' / 100) * fromIntegral (totalCount h) + 0.5
    totals = U.scanl f (0 :: Int, 0) withIndex
      where
        f (_, v') (i, v) = (i, v' + v)
        withIndex = U.imap (,) (counts h)
