{-|
Module      : Data.HdrHistogram.Tutorial
Copyright   : (c) Josh Bohde, 2015
License     : GPL-3
Maintainer  : josh@joshbohde.com
Stability   : experimental
Portability : POSIX

-}
{-# LANGUAGE DataKinds           #-}
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
> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> import           Control.Monad             (forM_)
> import qualified Data.HdrHistogram         as H
> import qualified Data.HdrHistogram.Mutable as MH
>
> vals :: [Int]
> vals = undefined
>
> -- Measure from 1ms to 1 hour, with 3 points of precision
> type Config = H.Config 1 3600000 3
>
> main :: IO ()
> main = do
>   h <- MH.new
>   forM_ vals (MH.record h)
>   (frozen :: H.Histogram Config Int Int) <- MH.freeze h
>   print $ H.percentile frozen 50
>
-}
