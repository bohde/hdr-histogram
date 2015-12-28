{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogramSpec where

import           Control.Monad.ST             (runST)
import           Data.Bits                    (Bits)
import           Data.HdrHistogram
import           Data.HdrHistogram.Config
import           Data.Vector                  ((!))
import qualified Data.Vector                  as U
import           Data.Vector.Algorithms.Intro (sort)
import           System.Random                (Random)
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck

data ConfigAndVals a = ConfigAndVals (HistogramConfig a) [a]
                    deriving (Show, Eq)

instance (Random a, Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (ConfigAndVals a) where
  arbitrary = do
    config' <- arbitrary
    vals <- listOf1 $ choose (lowest config', highest config')
    return $ ConfigAndVals config' vals

median :: Ord a => U.Vector a -> a
median s = sorted ! middle
    where
     middle = if odd len
              then len `div` 2
              else (len `div` 2) - 1
     sorted = runST $ do
       v <- U.thaw s
       sort v
       U.unsafeFreeze v
     len = U.length s

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
