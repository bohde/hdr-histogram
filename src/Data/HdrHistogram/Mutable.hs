module Data.HdrHistogram.Mutable (
  HistogramConfig, config,
  Histogram(..), histogram, record, recordValues,
  freeze, unsafeFreeze, thaw, unsafeThaw,
  SignificantFigures, significantFigures
  ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Bits                   (FiniteBits)
import qualified Data.HdrHistogram           as H
import           Data.HdrHistogram.Config
import           Data.Primitive.MutVar       (MutVar, modifyMutVar', newMutVar,
                                              readMutVar)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

data Histogram s a b = Histogram {
  _config    :: HistogramConfig a,
  totalCount :: MutVar s b,
  counts     :: U.MVector s b
}

histogram :: (PrimMonad m, U.Unbox b, Integral b) => HistogramConfig a -> m (Histogram (PrimState m) a b)
histogram config' = do
  vect <- MU.replicate (countsLen config') 0
  totals <- newMutVar 0
  return Histogram {
  _config = config',
  totalCount = totals,
  counts = vect
  }

record :: (Integral a, Integral b, FiniteBits a, U.Unbox b, PrimMonad m) =>
         Histogram (PrimState m) a b -> a -> m ()
record h val = recordValues h val 1

recordValues :: (Integral a, Integral b, FiniteBits a, U.Unbox b, PrimMonad m) =>
               Histogram (PrimState m) a b -> a -> b -> m ()
recordValues h val count = do
  modifyMutVar' (totalCount h) (+ count)
  MU.unsafeModify (counts h) (+ count) index
  where
    c = _config h
    index = asInt c $ asIndex c val

freeze :: (MU.Unbox b, PrimMonad m) => Histogram (PrimState m) a b -> m (H.Histogram a b)
freeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.freeze vec
  return $ H.Histogram c t v

unsafeFreeze :: (MU.Unbox b, PrimMonad m) => Histogram (PrimState m) a b -> m (H.Histogram a b)
unsafeFreeze (Histogram c total vec) = do
  t <- readMutVar total
  v <- U.unsafeFreeze vec
  return $ H.Histogram c t v

thaw :: (MU.Unbox b, PrimMonad m) => H.Histogram a b -> m (Histogram (PrimState m) a b)
thaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.thaw vec
  return $ Histogram c t v

unsafeThaw :: (MU.Unbox b, PrimMonad m) => H.Histogram a b -> m (Histogram (PrimState m) a b)
unsafeThaw (H.Histogram c total vec) = do
  t <- newMutVar total
  v <- U.unsafeThaw vec
  return $ Histogram c t v
