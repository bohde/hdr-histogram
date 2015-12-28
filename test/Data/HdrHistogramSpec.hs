module Data.HdrHistogramSpec where

import           Control.Monad         (unless)
import           Data.HdrHistogram
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

instance Arbitrary SigificantFigures where
  arbitrary = fmap SigificantFigures $ elements [1..5]
  shrink (SigificantFigures a) = fmap SigificantFigures [1..(a - 1)]

data BoundedArgs = BoundedArgs Int Int Int deriving (Show, Eq)

instance Arbitrary BoundedArgs where
  arbitrary = do
    min <- arbitrary `suchThat` (>= 0)
    max <- arbitrary `suchThat` (> min)
    val <- arbitrary `suchThat` (\a -> (a >= min) && (a <= max))
    return $ BoundedArgs min max val

expectTrue :: String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

compareWith :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

shouldBeLessThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeLessThan = compareWith (>) "is not less than"

shouldBeGreaterThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeGreaterThan = compareWith (<) "is not greater than"

spec :: Spec
spec = do
  describe "bitLength" $ do
    it "should match the reference" $ do
      bitLength (0x1 :: Int) `shouldBe` 1
      bitLength (0x2 :: Int) `shouldBe` 2
      bitLength (0x4 :: Int) `shouldBe` 3
      bitLength (0x1000 :: Int) `shouldBe` 13
      bitLength (0x1000000 :: Int) `shouldBe` (64 - 39)

    prop "should return the smallest power of two containing the value" $ \(NonNegative a) -> do
      floor (2 ** fromIntegral (bitLength a)) `shouldBeGreaterThan` (a :: Int)

  describe "Histogram" $ do
    it "should generate bucket indices in bounds" $ property $ \((BoundedArgs min max val), sigfigs) -> do
      let
        h :: Histogram Int Int
        h = new min max sigfigs
      bucketIndex h val `shouldBeLessThan` bucketCount h

    it "not make out of bounds indices" $ property $ \((BoundedArgs min max val), sigfigs) -> do
      let
        h :: Histogram Int Int
        h = new min max sigfigs
      indexForValue h val `shouldBeLessThan` countsLen h

    it "should give equal result to reference" $ do
      let
        empty :: Either String (Histogram Int Int)
        empty = new 1 10 <$> significantFigures 1
        mark :: Histogram Int Int -> Histogram Int Int
        mark h = foldr (flip record) h [1..10]
        h = fmap mark empty
      fmap ((flip percentile) (90.0)) h `shouldBe` Right 9
