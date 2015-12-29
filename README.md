# hdr-histogram

## Overview

A Haskell implementation of [HdrHistogram](http://www.hdrhistogram.org/). It allows storing counts of observed values within a range,
while maintaining precision to a configurable number of significant digits.


## Example

    {-# LANGUAGE ScopedTypeVariables #-}
    import           Control.Monad             (forM_)
    import qualified Data.HdrHistogram         as H
    import qualified Data.HdrHistogram.Mutable as MH

    vals :: [Int]

    -- Measurements from 1ms to 1 hour
    config' :: Either String (H.HistogramConfig Int)
    config' = H.significantFigures 3 >>= return . H.config 1 3600000

    main :: IO ()
    main = do
      case config' of
        Left s -> print s
        Right c' -> do
          h <- MH.histogram c'
          forM_ vals (MH.record h)
          (frozen :: H.Histogram Int Int) <- MH.freeze h
          print $ H.percentile frozen 50
