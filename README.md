# hdr-histogram

[![Build Status](https://travis-ci.org/joshbohde/hdr-histogram.svg?branch=default)](https://travis-ci.org/joshbohde/hdr-histogram)
[![Code Climate](https://codeclimate.com/github/joshbohde/hdr-histogram/badges/gpa.svg)](https://codeclimate.com/github/joshbohde/hdr-histogram)
[![Hackage](https://img.shields.io/hackage/v/hdr-histogram.svg)](http://hackage.haskell.org/package/hdr-histogram)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hdr-histogram.svg)](http://packdeps.haskellers.com/feed?needle=hdr-histogram)


## Overview

A Haskell implementation of [HdrHistogram](http://www.hdrhistogram.org/). It allows storing counts of observed values within a range,
while maintaining precision to a configurable number of significant digits.


## Example

    {-# LANGUAGE DataKinds           #-}
    {-# LANGUAGE ScopedTypeVariables #-}

    import           Control.Monad             (forM_)
    import qualified Data.HdrHistogram         as H
    import qualified Data.HdrHistogram.Mutable as MH

    vals :: [Int]
    vals = undefined

    -- Measure from 1ms to 1 hour, with 3 points of precision
    type Config = H.Config 1 3600000 3

    main :: IO ()
    main = do
      h <- MH.new
      forM_ vals (MH.record h)
      (frozen :: H.Histogram Config Int Int) <- MH.freeze h
      print $ H.percentile frozen 50
