{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram.ConfigSpec where

import           Data.Bits                (Bits, FiniteBits)
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

data ConfigAndIndex a = ConfigAndIndex (HistogramConfig a) Int
                    deriving (Show, Eq)

instance (Random a, Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (ConfigAndIndex a) where
  arbitrary = do
    config' <- arbitrary
    i <- choose (0, size config' - 1)
    return $ ConfigAndIndex config' i


data SpecType a = SpecType Spec

runSpecType :: SpecType a -> Spec
runSpecType (SpecType s) = s

typeSpec :: forall a. (Show a, Integral a, Random a, Arbitrary a, Bounded a, FiniteBits a) => SpecType a
typeSpec = SpecType $ do
  prop "should generate bucket indices in bounds" $ \(ConfigAndVal config' (val :: a)) -> do
    let
      bi = asIndex config' val
      i = asInt config' bi
    bucket bi `shouldBeLessThan` bucketCount config'
    bucket bi `shouldBeGreaterThanOrEqual` 0
    subBucket bi `shouldBeLessThan` subBucketCount config'
    subBucket bi `shouldBeGreaterThanOrEqual` 0
    i `shouldBeLessThan` size config'
    i `shouldBeGreaterThanOrEqual` 0

  prop "index 0 should contain the lowest value" $ \(config' :: HistogramConfig a) -> do
    let
      val' = fromIndex config' $ fromInt config' 0
    upper val' `shouldBeGreaterThanOrEqual` lowest config'
    lower val' `shouldBeLessThanOrEqual` lowest config'

  prop "highest index should contain the highest value" $ \(config' :: HistogramConfig a) -> do
    pendingWith "Unsure this property should hold"
    let
      val' = fromIndex config' $ fromInt config' (size config' - 1)
    upper val' `shouldBeGreaterThanOrEqual` highest config'
    lower val' `shouldBeLessThanOrEqual` highest config'

  prop "asInt . fromInt == id" $ \(ConfigAndIndex (c :: HistogramConfig a) i) ->
    (asInt c . fromInt c) i `shouldBe` i

  prop "fromIndex . asIndex should give contain value" $ \(ConfigAndVal c (val :: a)) -> do
    let
      range = (fromIndex c . asIndex c) val
    upper range `shouldBeGreaterThanOrEqual` val
    lower range `shouldBeLessThanOrEqual` val

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
      floor ((2 :: Double) ** i) `shouldBeGreaterThan` (a :: Int)
      floor (2 ** (i - 1)) `shouldBeLessThanOrEqual` (a :: Int)

  describe "HistogramConfig Int" $ runSpecType (typeSpec :: SpecType Int)
  describe "HistogramConfig Int16" $ runSpecType (typeSpec :: SpecType Int.Int16)
  describe "HistogramConfig Int32" $ runSpecType (typeSpec :: SpecType Int.Int32)
  describe "HistogramConfig Int64" $ runSpecType (typeSpec :: SpecType Int.Int64)
  describe "HistogramConfig Word16" $ runSpecType (typeSpec :: SpecType Word.Word16)
  describe "HistogramConfig Word32" $ runSpecType (typeSpec :: SpecType Word.Word32)
  describe "HistogramConfig Word64" $ runSpecType (typeSpec :: SpecType Word.Word64)
