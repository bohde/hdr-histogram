{-|
Module      : Data.HdrHistogram.Mutable
Copyright   : (c) Josh Bohde, 2015
License     : GPL-3
Maintainer  : josh@joshbohde.com
Stability   : experimental
Portability : POSIX

A Haskell implementation of <http://www.hdrhistogram.org/ HdrHistogram>.
It allows storing counts of observed values within a range,
while maintaining precision to a configurable number of significant
digits.

The mutable histogram allows only writes, and conversion to and from
pure histograms. It follows the original implementation, and has
similar performance characteristics. Current recording benchmarks take
about 9ns, and allocates 16 bytes.

-}
{-# LANGUAGE DeriveGeneric #-}
module Data.HdrHistogram.Mutable (
  -- * Histogram
  Histogram(..), histogram,

  -- * Writing
  record, recordValues,

  -- * Converting
  freeze, unsafeFreeze, thaw, unsafeThaw,

  -- * Re-exports
  HistogramConfig, config,
  SignificantFigures, significantFigures
  ) where

import           Control.DeepSeq             (NFData, deepseq, rnf)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Bits                   (FiniteBits)
import qualified Data.HdrHistogram           as H
import           Data.HdrHistogram.Config
import           Data.Primitive.MutVar       (MutVar, modifyMutVar', newMutVar,
                                              readMutVar)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Generics                (Generic)

-- | A mutable 'Histogram'
data Histogram s value count = Histogram {
  _config    :: HistogramConfig value,
  totalCount :: MutVar s count,
  counts     :: U.MVector s count
} deriving (Generic)

instance (NFData value, NFData count) => NFData (Histogram s value count) where
  rnf (Histogram c _ vec) = deepseq c $ deepseq vec ()

-- | Construct a 'Histogram' from the given 'HistogramConfig'
histogram :: (PrimMonad m, U.Unbox count, Integral count) => HistogramConfig value -> m (Histogram (PrimState m) value count)
histogram config' = do
  vect <- MU.replicate (size config') 0
  totals <- newMutVar 0
  return Histogram {
  _config = config',
  totalCount = totals,
  counts = vect
  }

{-# INLINEABLE record #-}
-- | Record value single value to the 'Histogram'
record :: (Integral value, Integral count, FiniteBits value, U.Unbox count, PrimMonad m) =>
         Histogram (PrimState m) value count -> value -> m ()
record h val = recordValues h val 1

{-# INLINEABLE recordValues #-}
-- | Record a multiple instances of a value value to the 'Histogram'
recordValues :: (Integral value, Integral count, FiniteBits value, U.Unbox count, PrimMonad m) =>
               Histogram (PrimState m) value count -> value -> count -> m ()
recordValues h val count = do
  modifyMutVar' (totalCount h) (+ count)
  modify (counts h) (+ count) (indexForValue c val)
  where
    c = _config h
    modify v f i = do
      a <- MU.unsafeRead v i
      MU.unsafeWrite v i (f a)

-- | Convert a mutable 'Histogram' to a pure
freeze :: (MU.Unbox count, PrimMonad m) => Histogram (PrimState m) value count -> m (H.Histogram value count)
freeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.freeze vec
  return $ H.Histogram c t v

-- | Convert a mutable 'Histogram' to a pure. The mutable cannot counte reused after this.
unsafeFreeze :: (MU.Unbox count, PrimMonad m) => Histogram (PrimState m) value count -> m (H.Histogram value count)
unsafeFreeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.unsafeFreeze vec
  return $ H.Histogram c t v

-- | Convert a pure 'Histogram' to a mutable.
thaw :: (MU.Unbox count, PrimMonad m) => H.Histogram value count -> m (Histogram (PrimState m) value count)
thaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.thaw vec
  return $ Histogram c t v

-- | Convert a pure 'Histogram' to a mutable. The pure cannot counte reused after this.
unsafeThaw :: (MU.Unbox count, PrimMonad m) => H.Histogram value count -> m (Histogram (PrimState m) value count)
unsafeThaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.unsafeThaw vec
  return $ Histogram c t v
