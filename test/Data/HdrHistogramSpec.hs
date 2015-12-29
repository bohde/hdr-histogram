{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogramSpec where

import           Data.HdrHistogram
import           Data.HdrHistogram.Config
import qualified Data.Vector              as U
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.Utils

spec :: Spec
spec =
  describe "Histogram" $ do
    it "should give equal result to reference" $ do
      let
        empty :: Either String (Histogram Int Int)
        empty = (histogram . config 1 10) <$> significantFigures 1
        mark :: Histogram Int Int -> Histogram Int Int
        mark h' = foldr (flip record) h' [1..10]
        h = fmap mark empty
      fmap (upper . (`percentile` 90.0)) h `shouldBe` Right 9

    prop "should be close to reference implementation" $ \(ConfigAndVals c (vals :: [Int])) -> do
      let
        v = U.fromList vals
        median' = median v
        h :: Histogram Int Int
        h = U.foldl record (histogram c) v
        p = percentile h 50
      lower p `shouldBeLessThanOrEqual` median'
      upper p `shouldBeGreaterThanOrEqual` median'
