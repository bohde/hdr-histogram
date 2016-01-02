{-|
Module      : Data.HdrHistogram.Config
Copyright   : (c) Josh Bohde, 2015
License     : GPL-3
Maintainer  : josh@joshbohde.com
Stability   : experimental
Portability : POSIX

A Haskell implementation of <http://www.hdrhistogram.org/ HdrHistogram>. It allows storing counts of observed values within a range,
while maintaining precision to a configurable number of significant
digits.

This module captures the common functionality of converting values to
and from 'Int' indices, regardless of mutability or memory layout.
-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE FlexibleInstances      #-}
module Data.HdrHistogram.Config (
  -- * HistogramConfig
  Config, mkConfig, getConfig, HasConfig,
  size,
  indexForValue,
  Range(..),
  rangeForIndex
  ) where

import           Data.Bits                         (Bits, FiniteBits)
import Data.Proxy (Proxy(Proxy))
import           GHC.TypeLits (KnownNat, Nat, type (<=), type (-), natVal)
import           Data.HdrHistogram.Config.Internal

data Config (lowest :: Nat) (highest :: Nat) (sig :: Nat) value

type SigBounds sig = (1 <= sig, sig <= 5)
type HighLow a b = (a <= (b - 1))

mkConfig :: (HighLow low high, SigBounds sig, Integral value, FiniteBits value)
           => Proxy (Config low high sig value)
mkConfig = Proxy


class HasConfig s a | s -> a where
  getConfig :: Proxy s -> HistogramConfig a

instance (KnownNat low, KnownNat high, KnownNat sig, HighLow low high, SigBounds sig, Integral value, FiniteBits value) =>
         HasConfig (Config low high sig value) value where
  {-# INLINEABLE getConfig #-}
  getConfig _ = config low' high' (SignificantFigures sig')
    where
      low' = fromIntegral $ natVal $ (Proxy :: Proxy low)
      high' = fromIntegral $ natVal $ (Proxy :: Proxy high)
      sig' = fromIntegral $ natVal $ (Proxy :: Proxy sig)

{-# INLINEABLE indexForValue #-}
-- | The index for a value
indexForValue :: (FiniteBits a, Integral a) =>
                HistogramConfig a -> a -> Int
indexForValue c = asInt c . asIndex c

{-# INLINEABLE rangeForIndex #-}
-- | The possible range of values for a given index
rangeForIndex :: (Integral a, Bits a) =>
                HistogramConfig a -> Int -> Range a
rangeForIndex c = fromIndex c . fromInt c
