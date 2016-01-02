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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram (
  -- * Histogram
  Histogram(..), empty, fromConfig,
  -- * Writing
  record, recordValues,
  -- * Reading
  Range(..),
  percentile,
  -- * Re-exports
  Config, mkConfig, HasConfig
  ) where

import           Data.Bits                (Bits, FiniteBits)
import           Data.HdrHistogram.Config
import           Data.HdrHistogram.Config.Internal
import           Data.Vector.Unboxed      ((!), (//))
import qualified Data.Vector.Unboxed      as U
import Data.Proxy (Proxy(Proxy))
import Data.Tagged (Tagged(Tagged))

-- | A pure 'Histogram'
data Histogram config value count = Histogram {
  _config    :: HistogramConfig value,
  totalCount :: count,
  counts     :: U.Vector count
} deriving (Eq, Show)

-- | Construct a 'Histogram'.
empty :: forall config value count. (HasConfig config value, U.Unbox count, Integral count) => Histogram config value count
empty = fromConfig (Tagged c :: Tagged config (HistogramConfig value))
  where
    p = Proxy :: Proxy config
    c = getConfig p

-- | Construct a 'Histogram' from the given 'HistogramConfig'. In this
-- case 'c' is a phantom type.
fromConfig :: (U.Unbox count, Integral count) => Tagged c (HistogramConfig value) -> Histogram c value count
fromConfig (Tagged c) = Histogram {
  _config = c,
  totalCount = 0,
  counts = U.replicate (size c) 0
  }

instance (HasConfig config value, U.Unbox count, Integral count) =>
         Monoid (Histogram config value count) where
  mempty = empty
  Histogram config' t c `mappend` Histogram _ t' c' = Histogram config' (t + t') (U.zipWith (+) c c')

-- | Record a single value to the 'Histogram'
record :: (U.Unbox count,
          Integral count, Integral value, FiniteBits value)
         => Histogram config value count
         -> value
         -> Histogram config value count
record h val = recordValues h val 1

-- | Record a multiple instances of a value value to the 'Histogram'
recordValues :: (U.Unbox count, Integral count, Integral value, FiniteBits value) => Histogram config value count -> value -> count -> Histogram config value count
recordValues h val count = h {
    totalCount = totalCount h + count,
    counts = counts h // [(index, (counts h ! index) + count)]
    }
  where
    index = indexForValue (_config h) val

-- recordCorrectedValues :: Integral value => Histogram value count -> value -> value -> Histogram value count
-- recordCorrectedValues = undefined

-- | Calculate the 'Range' of values at the given percentile
percentile :: (Integral value, Integral count, U.Unbox count, Bits value)
             => Histogram config value count
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
