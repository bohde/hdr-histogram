{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram.MutableSpec where

import           Control.Monad.ST          (runST)
import           Data.HdrHistogram
import           Data.HdrHistogram.Config
import qualified Data.HdrHistogram.Mutable as MH
import qualified Data.Vector               as U
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.Utils

spec :: Spec
spec =
  describe "Histogram" $
    prop "should be close to reference implementation" $ \(ConfigAndVals c (vals :: [Int])) -> do
      let
        v = U.fromList vals
        median' = median v
        h :: Histogram Int Int
        h = runST $ do
          h' <- MH.histogram c
          U.forM_ v (MH.record h')
          MH.unsafeFreeze h'
        p = percentile h 50
      lower p `shouldBeLessThanOrEqual` median'
      upper p `shouldBeGreaterThanOrEqual` median'
