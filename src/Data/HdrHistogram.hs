{-|
Module      : Data.HdrHistogram
Copyright   : (c) Josh Bohde, 2015
License     : GPL-3
Maintainer  : josh@joshbohde.com
Stability   : experimental
Portability : POSIX

A Haskell implementation of <http://www.hdrhistogram.org/ HdrHistogram>.
It allows storing counts of observed values within a range,
while maintaining precision to a configurable number of significant
digits.

-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram (
  -- * Histogram
  Histogram(..), histogram,
  -- * Writing
  record, recordValues,
  -- * Reading
  Range(..),
  percentile,
  -- * Re-exports
  HistogramConfig, config,
  SignificantFigures, significantFigures
  ) where

import           Data.Bits                (Bits, FiniteBits)
import           Data.HdrHistogram.Config
import           Data.Vector.Unboxed      ((!), (//))
import qualified Data.Vector.Unboxed      as U

-- | A pure 'Histogram'
data Histogram value count = Histogram {
  _config    :: HistogramConfig value,
  totalCount :: count,
  counts     :: U.Vector count
} deriving (Eq, Show)

-- | Construct a 'Histogram' from the given 'HistogramConfig'
histogram :: (U.Unbox count, Integral count) => HistogramConfig value -> Histogram value count
histogram config' = Histogram {
  _config = config',
  totalCount = 0,
  counts = U.replicate (size config') 0
  }


-- | Record a single value to the 'Histogram'
record :: (U.Unbox count, Integral count, Integral value, FiniteBits value) => Histogram value count -> value -> Histogram value count
record h val = recordValues h val 1

-- | Record a multiple instances of a value value to the 'Histogram'
recordValues :: (U.Unbox count, Integral count, Integral value, FiniteBits value) => Histogram value count -> value -> count -> Histogram value count
recordValues h val count = h {
    totalCount = totalCount h + count,
    counts = counts h // [(index, (counts h ! index) + count)]
    }
  where
    c = _config h
    index = indexForValue c val


-- merge :: Histogram value count -> Histogram value count -> Histogram value count
-- merge = undefined
--
-- recordCorrectedValues :: Integral value => Histogram value count -> value -> value -> Histogram value count
-- recordCorrectedValues = undefined

-- | Calculate the 'Range' of values at the given percentile
percentile :: (Integral value, Integral count, U.Unbox count, Bits value)
             => Histogram value count
             -> Float -- ^ The percentile in the range 0 to 100
             -> Range value
percentile h q = case U.find ((>= count) . snd) totals of
  Nothing -> Range 0 0
  Just (i, _) -> rangeForIndex c i
  where
    c = _config h
    q' = min q 100
    count = floor $ (q' / 100) * fromIntegral (totalCount h) + 0.5
    totals = U.scanl f (0 :: Int, 0) withIndex
      where
        f (_, v') (i, v) = (i, v' + v)
        withIndex = U.imap (,) (counts h)
