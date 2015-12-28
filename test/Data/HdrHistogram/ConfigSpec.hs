{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram.ConfigSpec where

import           Control.Monad            (unless)
import           Data.Bits                (Bits)
import           Data.Bits                (FiniteBits)
import           Data.HdrHistogram.Config
import qualified Data.Int                 as Int
import qualified Data.Word                as Word
import           System.Random            (Random)
import           Test.Expectations
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck

data ConfigAndVal a = ConfigAndVal (HistogramConfig a) a
                    deriving (Show, Eq)

instance (Random a, Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (ConfigAndVal a) where
  arbitrary = do
    config' <- arbitrary
    val <- choose (lowest config', highest config')
    return $ ConfigAndVal config' val


data SpecType a = SpecType Spec
runSpecType (SpecType s) = s

typeSpec :: forall a. (Show a, Integral a, Random a, Arbitrary a, Bounded a, FiniteBits a) => SpecType a
typeSpec = SpecType $ do
  prop "should generate bucket indices in bounds" $ \(ConfigAndVal config' (val :: a)) ->
    bucketIndex config' val `shouldBeLessThan` bucketCount config'

  prop "not make out of bounds indices" $ \(ConfigAndVal config' (val :: a)) ->
    indexForValue config' val `shouldBeLessThan` countsLen config'

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

  describe "HistogramConfig Int" $ runSpecType (typeSpec :: SpecType Int)
  describe "HistogramConfig Int32" $ runSpecType (typeSpec :: SpecType Int.Int32)
  describe "HistogramConfig Int64" $ runSpecType (typeSpec :: SpecType Int.Int64)
  describe "HistogramConfig Word32" $ runSpecType (typeSpec :: SpecType Word.Word32)
  describe "HistogramConfig Word64" $ runSpecType (typeSpec :: SpecType Word.Word64)