{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogramSpec where

import           Data.HdrHistogram
import qualified Data.Vector           as U
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.Utils

type Config' = Config 1 10 1

spec :: Spec
spec =
  describe "Histogram" $ do
    it "should give equal result to reference" $ do
      let
        h :: Histogram Config' Int Int
        h = foldr (flip record) empty [1..10]
      (upper . (`percentile` 90.0)) h `shouldBe` 9

    prop "should be close to reference implementation" $ \((ConfigAndVals c vals) :: ConfigAndVals c Int) -> do
      let
        empty' :: Histogram c Int Int
        empty' = fromConfig c
        v = U.fromList vals
        median' = median v
        h = U.foldl record empty' v
        p = percentile h 50
      lower p `shouldBeLessThanOrEqual` median'
      upper p `shouldBeGreaterThanOrEqual` median'
