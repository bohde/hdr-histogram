{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram.ConfigSpec where

import           Control.Monad            (unless)
import           Data.Bits                (Bits)
import           Data.HdrHistogram.Config
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "bitLength" $ do
    it "should match the reference" $ do
      bitLength (0x1 :: Int) `shouldBe` 1
      bitLength (0x2 :: Int) `shouldBe` 2
      bitLength (0x4 :: Int) `shouldBe` 3
      bitLength (0x1000 :: Int) `shouldBe` 13
      bitLength (0x1000000 :: Int) `shouldBe` (64 - 39)

    prop "should return the smallest power of two containing the value"
      $ \(NonNegative a) -> do
      let
        i = fromIntegral (bitLength a)
      floor (2 ** i) `shouldBeGreaterThan` (a :: Int)
      floor (2 ** (i - 1)) `shouldBeLessThanOrEqual` (a :: Int)

  describe "HistogramConfig" $ do
    prop "should generate bucket indices in bounds" $
      \(config :: HistogramConfig Int, (NonNegative val)) ->
      bucketIndex config val `shouldBeLessThan` bucketCount config

    prop "not make out of bounds indices" $
      \(config :: HistogramConfig Int, (NonNegative val)) ->
      indexForValue config val `shouldBeLessThan` countsLen config
