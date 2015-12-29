{-# LANGUAGE DeriveGeneric #-}
module Data.HdrHistogram.Mutable (
  Histogram(..), histogram, record, recordValues,
  freeze, unsafeFreeze, thaw, unsafeThaw,
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
data Histogram s a b = Histogram {
  _config    :: HistogramConfig a,
  totalCount :: MutVar s b,
  counts     :: U.MVector s b
} deriving (Generic)

instance (NFData a, NFData b) => NFData (Histogram s a b) where
  rnf (Histogram c _ vec) = deepseq c $ deepseq vec ()

-- | Construct a 'Histogram' from the given 'HistogramConfig'
histogram :: (PrimMonad m, U.Unbox b, Integral b) => HistogramConfig a -> m (Histogram (PrimState m) a b)
histogram config' = do
  vect <- MU.replicate (size config') 0
  totals <- newMutVar 0
  return Histogram {
  _config = config',
  totalCount = totals,
  counts = vect
  }

{-# INLINEABLE record #-}
-- | Record a single value to the 'Histogram'
record :: (Integral a, Integral b, FiniteBits a, U.Unbox b, PrimMonad m) =>
         Histogram (PrimState m) a b -> a -> m ()
record h val = recordValues h val 1

{-# INLINEABLE recordValues #-}
-- | Record a multiple instances of a value value to the 'Histogram'
recordValues :: (Integral a, Integral b, FiniteBits a, U.Unbox b, PrimMonad m) =>
               Histogram (PrimState m) a b -> a -> b -> m ()
recordValues h val count = do
  modifyMutVar' (totalCount h) (+ count)
  modify (counts h) (+ count) (indexForValue c val)
  where
    c = _config h
    modify v f i = do
      a <- MU.unsafeRead v i
      MU.unsafeWrite v i (f a)

-- | Convert a mutable 'Histogram' to a pure
freeze :: (MU.Unbox b, PrimMonad m) => Histogram (PrimState m) a b -> m (H.Histogram a b)
freeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.freeze vec
  return $ H.Histogram c t v

-- | Convert a mutable 'Histogram' to a pure. The mutable cannot be reused after this.
unsafeFreeze :: (MU.Unbox b, PrimMonad m) => Histogram (PrimState m) a b -> m (H.Histogram a b)
unsafeFreeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.unsafeFreeze vec
  return $ H.Histogram c t v

-- | Convert a pure 'Histogram' to a mutable.
thaw :: (MU.Unbox b, PrimMonad m) => H.Histogram a b -> m (Histogram (PrimState m) a b)
thaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.thaw vec
  return $ Histogram c t v

-- | Convert a pure 'Histogram' to a mutable. The pure cannot be reused after this.
unsafeThaw :: (MU.Unbox b, PrimMonad m) => H.Histogram a b -> m (Histogram (PrimState m) a b)
unsafeThaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.unsafeThaw vec
  return $ Histogram c t v
