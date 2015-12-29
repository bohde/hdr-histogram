{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.HdrHistogram.Config (
  SignificantFigures,
  significantFigures,
  HistogramConfig(..), config,
  Range(..), upper, lower,
  Index, bucket, subBucket,
  asInt, fromInt,
  asIndex, fromIndex,
  bitLength
  ) where

import           Control.DeepSeq (NFData)
import           Data.Bits       (Bits, FiniteBits, bitSizeMaybe,
                                  countLeadingZeros, finiteBitSize, shift,
                                  shiftR, (.&.), (.|.))
import           GHC.Generics    (Generic)
import           Test.QuickCheck (Arbitrary (..), Large (..), Positive (..),
                                  elements, getLarge, suchThat)

newtype SignificantFigures = SignificantFigures Int deriving (Eq, Show, NFData)

significantFigures :: Int -> Either String SignificantFigures
significantFigures i = if i > 0  && i < 6
  then Right $ SignificantFigures i
  else Left "HdrHistogram.significantFigures must be between 1 and 5"

instance Arbitrary SignificantFigures where
  arbitrary = SignificantFigures <$> elements [1..5]
  shrink (SignificantFigures a) = fmap SignificantFigures [1..(a - 1)]

data HistogramConfig a = HistogramConfig {
   lowest                      :: !a,
   highest                     :: !a,
   sigFigures                  :: !SignificantFigures,
   unitMagnitude               :: !Int,
   subBucketHalfCountMagnitude :: !Int,
   subBucketHalfCount          :: !Int,
   subBucketMask               :: !a,
   subBucketCount              :: !Int,
   bucketCount                 :: !Int,
   countsLen                   :: !Int
   } deriving (Eq, Show, Generic)

instance (NFData a) => NFData (HistogramConfig a)

instance (Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (HistogramConfig a) where
  arbitrary = do
    (Positive min') <- arbitrary
    (Large max') <- arbitrary `suchThat` ((> min') . getLarge)
    s <- arbitrary
    return $ config min' max' s

  shrink c = filter (/= c) vals
    where
      vals = do
        min' <- [0..lowest c]
        max' <- [min'+1..highest c]
        s <- shrink $ sigFigures c
        return $ config min' max' s

data Range a = Range a a

upper :: Range a -> a
upper (Range _ a) = a

lower :: Range a -> a
lower (Range a _) = a

config :: forall a. (Integral a, Bits a) => a -> a -> SignificantFigures -> HistogramConfig a
config lowest' highest' s@(SignificantFigures sigfigs) = config'
  where
    config' = HistogramConfig {
      lowest = lowest',
      highest = highest',
      sigFigures = s,
      unitMagnitude = unitMagnitude',
      subBucketHalfCountMagnitude = subBucketHalfCountMagnitude',
      subBucketHalfCount          = floor $ subBucketCount' / 2,
      subBucketMask               = floor (subBucketCount' - 1) `shift` unitMagnitude',
      subBucketCount              = floor subBucketCount',
      bucketCount                 = bucketCount',
      countsLen                   = countsLen'
      }

    toDouble :: (Real b) => b -> Double
    toDouble = fromRational . toRational

    unitMagnitude' = fromInteger $ floor $ max 0 m
      where
        m = logBase 2 (toDouble lowest')

    subBucketHalfCountMagnitude' :: Int
    subBucketHalfCountMagnitude' = max 0 (magnitude - 1)
      where
        desiredMagnitude = (ceiling . logBase 2 . (* 2) . (10 **) . toDouble) sigfigs
        magnitude = case bitSizeMaybe (0 :: a) of
          Nothing -> desiredMagnitude
          Just i ->  min possibleMagnitude desiredMagnitude
            where
              possibleMagnitude = i - 1 - unitMagnitude'


    subBucketCount' :: Double
    subBucketCount' = 2 ** fromIntegral (subBucketHalfCountMagnitude' + 1)

    bucketCount' :: Int
    bucketCount' = 1 + length (takeWhile (< effectiveHighest) $ iterate (`shift` 1) smallestUntrackable)
      where
        effectiveHighest :: Integer
        effectiveHighest = fromIntegral highest'

        smallestUntrackable :: Integer
        smallestUntrackable = floor subBucketCount' `shift` unitMagnitude'

    countsLen' = (bucketCount' + 1) * floor (subBucketCount' / 2)

data Index = Index {
  bucket    :: Int,
  subBucket :: Int
  }

{-# INLINEABLE asInt #-}
asInt :: HistogramConfig a -> Index -> Int
asInt c (Index b sub) = (sub' + bucket') - 1
  where
    sub' = sub - subBucketHalfCount c
    bucket' = (b + 1) `shift` subBucketHalfCountMagnitude c

fromInt :: HistogramConfig a -> Int -> Index
fromInt c i = if bucket' < 0
              then Index 0 (sub' - subBucketHalfCount c)
              else Index bucket' sub'
  where
    i' = i + 1
    bucket' = (i' `shiftR` subBucketHalfCountMagnitude c) - 1
    sub' = i' .&. (subBucketHalfCount c - 1) + subBucketHalfCount c


{-# INLINEABLE asIndex #-}
asIndex :: (Integral a, FiniteBits a) => HistogramConfig a -> a -> Index
asIndex c a = Index bucket' sub
  where
    magnitude :: Int
    magnitude = unitMagnitude c

    bucket' = m - (subBucketHalfCountMagnitude c + 1)
      where
        m :: Int
        m = bitLength (a .|. subBucketMask c) - magnitude

    sub = fromIntegral $ a `shiftR` toShift
      where
        toShift :: Int
        toShift = bucket' + magnitude

fromIndex :: (Integral a, Bits a) => HistogramConfig a -> Index -> Range a
fromIndex c (Index bucket' sub) = Range lower' upper'
  where
    toShift = bucket' + unitMagnitude c
    lower' = fromIntegral $ sub `shift` toShift
    range = 1 `shift` toShift
    upper' = (lower' + range) - 1

{-# INLINEABLE bitLength #-}
bitLength :: FiniteBits b => b -> Int
bitLength b = finiteBitSize b - countLeadingZeros b
