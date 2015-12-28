module Data.HdrHistogramSpec where

import           Data.Bits             (Bits)
import           Data.HdrHistogram
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Histogram" $ do
    it "should give equal result to reference" $ do
      let
        empty :: Either String (Histogram Int Int)
        empty = (histogram . config 1 10) <$> significantFigures 1
        mark :: Histogram Int Int -> Histogram Int Int
        mark h = foldr (flip record) h [1..10]
        h = fmap mark empty
      fmap ((flip percentile) (90.0)) h `shouldBe` Right 9
