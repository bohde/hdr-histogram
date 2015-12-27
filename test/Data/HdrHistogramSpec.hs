module Data.HdrHistogramSpec where

import           Data.HdrHistogram
import           Test.Hspec


spec :: Spec
spec = do
  describe "Histogram" $ do
    it "should be able to be constructed" $ do
      let
        empty :: Either String (Histogram Int Int)
        empty = new 0 10 <$> significantFigures 3
        mark :: Histogram Int Int -> Int
        mark h = (flip percentile) (0.9) $ foldr (flip record) h [1..10]
      fmap mark empty `shouldBe` Right 9
