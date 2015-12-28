{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram.Config (
  SignificantFigures,
  significantFigures,
  HistogramConfig(..), config,
  Range, upper, lower,
  indexForValue, valueAtIndex, bucketIndex,
  bitLength
  ) where

import           Data.Bits       (Bits, FiniteBits, countLeadingZeros,
                                  finiteBitSize, shift, shiftR, (.&.), (.|.))
import           Data.Int        (Int64)
import           Test.QuickCheck (Arbitrary (..), Large (..), Positive (..),
                                  elements, getLarge, suchThat)

newtype SignificantFigures = SignificantFigures Int deriving (Eq, Show)

significantFigures :: Int -> Either String SignificantFigures
significantFigures i = case (i > 0  && i < 6) of
  True -> Right $ SignificantFigures i
  False -> Left "HdrHistogram.significantFigures must be between 1 and 5"

instance Arbitrary SignificantFigures where
  arbitrary = SignificantFigures <$> elements [1..5]
  shrink (SignificantFigures a) = fmap SignificantFigures [1..(a - 1)]

data HistogramConfig a = HistogramConfig {
   lowest                      :: a,
   highest                     :: a,
   sigFigures                  :: SignificantFigures,
   unitMagnitude               :: a,
   subBucketHalfCountMagnitude :: !Int,
   subBucketHalfCount          :: !Int,
   subBucketMask               :: a,
   subBucketCount              :: !Int,
   bucketCount                 :: !Int,
   countsLen                   :: !Int
   } deriving (Eq, Show)

instance (Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (HistogramConfig a) where
  arbitrary = do
    (Positive min') <- arbitrary
    (Large max') <- arbitrary `suchThat` ((> min') . getLarge)
    s <- arbitrary
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
      subBucketMask               = floor (subBucketCount' - 1) `shift` toInt unitMagnitude',
      subBucketCount              = floor subBucketCount',
      bucketCount                 = bucketCount',
      countsLen                   = countsLen'
      }

    toDouble :: (Real b) => b -> Double
    toDouble = fromRational . toRational

    toInt :: (Integral b) => b -> Int
    toInt = fromInteger . toInteger

    unitMagnitude' = fromInteger $ floor $ max 0 m
      where
        m = logBase 2 (toDouble lowest')

    subBucketHalfCountMagnitude' :: Int
    subBucketHalfCountMagnitude' = max 0 (m - 1)
      where
        m = (ceiling . logBase 2 . (* 2) . (10 **) . toDouble) sigfigs

    subBucketCount' :: Double
    subBucketCount' = 2 ** fromIntegral (subBucketHalfCountMagnitude' + 1)

    bucketCount' :: Int
    bucketCount' = 1 + length (takeWhile (< effectiveHighest) $ iterate (`shift` 1) smallestUntrackable)
      where
        effectiveHighest :: Integer
        effectiveHighest = fromIntegral highest'

        smallestUntrackable :: Integer
        smallestUntrackable = floor subBucketCount' `shift` toInt unitMagnitude'

    countsLen' = (bucketCount' + 1) * floor (subBucketCount' / 2)

indexForValue :: (Integral a, FiniteBits a) => HistogramConfig a -> a -> Int
indexForValue h val = countsIndex h i sub
  where
    i = bucketIndex h val
    sub = subBucketIndex h val i

bucketIndex :: (Integral a, FiniteBits a) => HistogramConfig a -> a -> Int
bucketIndex h a = fromIntegral $ m - fromIntegral (subBucketHalfCountMagnitude h + 1)
  where
    m :: Int64
    m = fromIntegral $ bitLength (a .|. subBucketMask h) - fromIntegral (unitMagnitude h)

subBucketIndex :: forall a. (Integral a, FiniteBits a) => HistogramConfig a -> a -> Int -> Int
subBucketIndex h v i = fromIntegral $ v `shiftR` fromIntegral toShift
  where
    toShift :: a
    toShift = fromIntegral i + unitMagnitude h

countsIndex :: HistogramConfig a -> Int -> Int -> Int
countsIndex h bucketIdx subBucketIdx = (subBucketIdx - subBucketHalfCount h) + ((bucketIdx + 1) `shift` subBucketHalfCountMagnitude h)

valueFromSubBucket :: (Integral a, Bits a) => HistogramConfig a -> Int -> Int -> Range a
valueFromSubBucket h bucketIndex' subBucketIndex' = Range lower' upper'
  where
    toShift = bucketIndex' + fromIntegral (unitMagnitude h)
    lower' = fromIntegral $ subBucketIndex' `shift` toShift
    range = 1 `shift` toShift
    upper' = (lower' + range) - 1


valueAtIndex :: (Integral a, Bits a) => HistogramConfig a -> Int -> Range a
valueAtIndex h i = if bucketIndex' < 0
                   then valueFromSubBucket h 0 (subBucketIndex' - subBucketHalfCount h)
                   else valueFromSubBucket h bucketIndex' subBucketIndex'
  where
    bucketIndex' = (i `shiftR` subBucketHalfCountMagnitude h) - 1
    subBucketIndex' = i .&. (subBucketHalfCount h - 1) + subBucketHalfCount h

bitLength :: FiniteBits b => b -> Int
bitLength b = finiteBitSize b - countLeadingZeros b
