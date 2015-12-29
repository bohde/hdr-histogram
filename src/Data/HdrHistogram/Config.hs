{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.HdrHistogram.Config (
  HistogramConfig, config, size,
  SignificantFigures,
  significantFigures,
  Range(..),
  indexForValue,
  rangeForIndex
  ) where

import           Data.Bits                         (Bits, FiniteBits)
import           Data.HdrHistogram.Config.Internal

{-# INLINEABLE indexForValue #-}
indexForValue :: (Integral a, Data.Bits.FiniteBits a) =>
                HistogramConfig a -> a -> Int
indexForValue c = asInt c . asIndex c

{-# INLINEABLE rangeForIndex #-}
rangeForIndex :: (Integral a, Data.Bits.Bits a) =>
                HistogramConfig a -> Int -> Range a
rangeForIndex c = fromIndex c . fromInt c
