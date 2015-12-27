{-# LANGUAGE ScopedTypeVariables #-}
module Data.HdrHistogram where

import           Data.Bits           (Bits, FiniteBits, countLeadingZeros,
                                      finiteBitSize, shift, shiftR, (.&.),
                                      (.|.))
import           Data.Int            (Int64)
import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as U


newtype SigificantFigures = SigificantFigures Int deriving (Eq, Show)

significantFigures :: Int -> Either String SigificantFigures
significantFigures i = case (i > 0  && i < 6) of
  True -> Right $ SigificantFigures i
  False -> Left "HdrHistogram.significantFigures must be between 1 and 5"

data Histogram a b = Histogram {
  lowest                      :: a,
  highest                     :: a,
  sigFigures                  :: SigificantFigures ,
  unitMagnitude               :: a,
  subBucketHalfCountMagnitude :: Int,
  subBucketHalfCount          :: Int,
  subBucketMask               :: a,
  subBucketCount              :: Int,
  bucketCount                 :: Int,
  countsLen                   :: Int,
  totalCount                  :: b,
  counts                      :: U.Vector b
} deriving (Eq, Show)

new :: (U.Unbox b, Integral a, Bits a, Integral b) => a -> a -> SigificantFigures -> Histogram a b
new lowest' highest' s@(SigificantFigures sigfigs) = histogram
  where
    histogram = Histogram {
      lowest = lowest',
      highest = highest',
      sigFigures = s,
      unitMagnitude = unitMagnitude',
      subBucketHalfCountMagnitude = subBucketHalfCountMagnitude',
      subBucketHalfCount          = floor $ subBucketCount' / 2,
      subBucketMask               = floor (subBucketCount' - 1) `shift` toInt unitMagnitude',
      subBucketCount              = floor subBucketCount',
      bucketCount                 = bucketCount',
      countsLen                   = countsLen',
      totalCount                  = 0,
      counts                     = U.replicate countsLen' 0
      }

    toDouble :: (Real a) => a -> Double
    toDouble = fromRational . toRational

    toInt :: (Integral a) => a -> Int
    toInt = fromInteger . toInteger

    unitMagnitude' = fromInteger $ floor $ max 0 m
      where
        m = logBase 2 (toDouble lowest')

    subBucketHalfCountMagnitude' :: Int
    subBucketHalfCountMagnitude' = max 0 (m - 1)
      where
        m = (ceiling . logBase 2 . (* 2) . (10 **) . toDouble) sigfigs

    subBucketCount' :: Double
    subBucketCount' = 2 ** (fromIntegral $ subBucketHalfCountMagnitude' + 1)

    bucketCount' :: Int
    bucketCount' = 1 + (length $ takeWhile (<= highest') $ iterate (`shift` 1) smallestUntrackable)
      where
        smallestUntrackable = (floor subBucketCount') `shift` toInt unitMagnitude'

    countsLen' = (bucketCount' + 1) * floor (subBucketCount' / 2)


merge :: Histogram a b -> Histogram a b -> Maybe (Histogram a b)
merge = undefined

record :: (U.Unbox b, Integral b, Integral a, FiniteBits a) => Histogram a b -> a -> Histogram a b
record h val = recordValues h val 1

recordValues :: (U.Unbox b, Integral b, Integral a, FiniteBits a) => Histogram a b -> a -> b -> Histogram a b
recordValues h val count = h {
    totalCount = totalCount h + count,
    counts = counts h // [(index, (counts h ! index) + count)]
    }
  where
    index = indexForValue h val

indexForValue h val = countsIndex h i sub
  where
    i = bucketIndex h val
    sub = subBucketIndex h val i

bucketIndex :: (Integral a, FiniteBits a) => Histogram a b -> a -> Int
bucketIndex h a = fromIntegral $ m - fromIntegral (subBucketHalfCountMagnitude h + 1)
  where
    m :: Int64
    m = fromIntegral $ (bitLength (a .|. subBucketMask h)) - fromIntegral (unitMagnitude h)

subBucketIndex :: forall a b. (Integral a, FiniteBits a) => Histogram a b -> a -> Int -> Int
subBucketIndex h v i = fromIntegral $ v `shiftR` fromIntegral toShift
  where
    toShift :: a
    toShift = fromIntegral i + unitMagnitude h

countsIndex :: Histogram a b -> Int -> Int -> Int
countsIndex h bucketIdx subBucketIdx = subBucketIdx - subBucketHalfCount h + (bucketIdx + 1 `shift` subBucketHalfCountMagnitude h)

recordCorrectedValues :: Integral a => Histogram a b -> a -> a -> Histogram a b
recordCorrectedValues = undefined

percentile :: (Integral a, Integral b, U.Unbox b, Bits a) => Histogram a b -> Float -> a
percentile h q = case U.find ((>= count) . snd) totals of
  Nothing -> 0
  Just (i, _) -> upper $ valueAtIndex h i
  where
    q' = min q 100
    count = floor $ (q' / 100) * fromIntegral (totalCount h) + 0.5
    totals = U.scanr f (0 :: Int, 0) withIndex
      where
        f (_, v') (i, v) = (i, v' + v)
        withIndex = U.imap (,) (counts h)


data Range a = Range a a

upper :: Range a -> a
upper (Range _ a) = a

valueAtIndex :: (Integral b, Integral a, Bits a) => Histogram a b -> Int -> Range a
valueAtIndex h i = if bucketIndex < 0
                   then valueFromSubBucket h 0 (subBucketIndex - subBucketHalfCount h)
                   else valueFromSubBucket h bucketIndex subBucketIndex
  where
    bucketIndex = (i `shiftR` subBucketHalfCountMagnitude h) - 1
    subBucketIndex = i .&. (subBucketHalfCount h - 1) + subBucketHalfCount h

valueFromSubBucket :: (Integral b, Integral a, Bits a) => Histogram a b -> Int -> Int -> Range a
valueFromSubBucket h bucketIndex subBucketIndex = Range lower upper
  where
    toShift = (bucketIndex + (fromIntegral $ unitMagnitude h))
    lower = fromIntegral $ subBucketIndex `shift` toShift
    range = 1 `shift` toShift
    upper = (lower + range) - 1

bitLength :: FiniteBits b => b -> Int
bitLength b = finiteBitSize b - countLeadingZeros b
