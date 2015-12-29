{-|
Module      : Data.HdrHistogram.Tutorial
Copyright   : (c) Josh Bohde, 2015
License     : GPL-3
Maintainer  : josh@joshbohde.com
Stability   : experimental
Portability : POSIX

-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.HdrHistogram.Tutorial (
  -- * Example
  -- $example
  ) where

import           Control.Monad             (forM_)
import qualified Data.HdrHistogram         as H
import qualified Data.HdrHistogram.Mutable as MH

{- $example

> {-# LANGUAGE ScopedTypeVariables #-}
> import           Control.Monad             (forM_)
> import qualified Data.HdrHistogram         as H
> import qualified Data.HdrHistogram.Mutable as MH
>
> vals :: [Int]
>
> -- Measurements from 1ms to 1 hour
> config' :: Either String (H.HistogramConfig Int)
> config' = H.significantFigures 3 >>= return . H.config 1 3600000
>
> main :: IO ()
> main = do
>   case config' of
>     Left s -> print s
>     Right c' -> do
>       h <- MH.histogram c'
>       forM_ vals (MH.record h)
>       (frozen :: H.Histogram Int Int) <- MH.freeze h
>       print $ H.percentile frozen 50

-}
