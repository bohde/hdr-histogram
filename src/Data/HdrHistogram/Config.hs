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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.HdrHistogram.Config (
  -- * SignificantFigures
  SignificantFigures,
  significantFigures,
  -- * HistogramConfig
  HistogramConfig, config, size,
  indexForValue,
  Range(..),
  rangeForIndex
  ) where

import           Data.Bits                         (Bits, FiniteBits)
import           Data.HdrHistogram.Config.Internal

{-# INLINEABLE indexForValue #-}
-- | The index for a value
indexForValue :: (Integral a, Data.Bits.FiniteBits a) =>
                HistogramConfig a -> a -> Int
indexForValue c = asInt c . asIndex c

{-# INLINEABLE rangeForIndex #-}
-- | The possible range of values for a given index
rangeForIndex :: (Integral a, Data.Bits.Bits a) =>
                HistogramConfig a -> Int -> Range a
rangeForIndex c = fromIndex c . fromInt c
