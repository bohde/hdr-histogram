module Data.HdrHistogram where

import           Data.Bits           (shift)
import           Data.Bits           (Bits)
import           Data.STRef          (STRef)
import qualified Data.Vector.Unboxed as U
import qualified Data.Word           as Word


newtype SigificantFigures = SigificantFigures Int deriving (Eq, Show)

significantFigures :: Int -> Either String SigificantFigures
significantFigures i = case (i > 0  && i < 6) of
  True -> Right $ SigificantFigures i
  False -> Left "HdrHistogram.significantFigures must be between 1 and 5"

data Histogram a = Histogram {
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
  totalCount                  :: Word.Word64,
  counts                      :: U.Vector a
} deriving (Eq, Show)

new :: (U.Unbox a, Integral a, Bits a) => a -> a -> SigificantFigures -> Histogram a
new lowest' highest' s@(SigificantFigures sigfigs) = histogram
  where
    histogram = Histogram {
      lowest = lowest',
      highest = highest',
      sigFigures = s,
      unitMagnitude = unitMagnitude',
      subBucketHalfCountMagnitude = ceiling subBucketHalfCountMagnitude',
      subBucketHalfCount          = floor $ subBucketCount' / 2,
      subBucketMask               = floor (subBucketCount' - 1) `shift` toInt unitMagnitude',
      subBucketCount              = floor subBucketCount',
      bucketCount                 = bucketCount',
      countsLen                   = (bucketCount' + 1) * floor (subBucketCount' / 2),
      totalCount                  = 0,
      counts                     = U.replicate bucketCount' 0
      }

    toDouble :: (Real a) => a -> Double
    toDouble = fromRational . toRational

    toInt :: (Integral a) => a -> Int
    toInt = fromInteger . toInteger

    unitMagnitude' = fromInteger $ floor $ max 0 m
      where
        m = logBase 2 (toDouble lowest')

    subBucketHalfCountMagnitude' :: Double
    subBucketHalfCountMagnitude' = max 0 (m - 1)
      where
        m = (logBase 2 . (* 2) . (** 10) . toDouble) sigfigs

    subBucketCount' :: Double
    subBucketCount' = (subBucketHalfCountMagnitude' + 1) ** 2

    bucketCount' :: Int
    bucketCount' = length $ takeWhile (< highest') $ iterate (`shift` 1) 1


merge :: Histogram a -> Histogram a -> Histogram a
merge = undefined

record :: Num a => Histogram a -> a -> Histogram a
record h = recordValues h 1

recordValues :: Num a => Histogram a -> a -> a -> Histogram a
recordValues = undefined

recordCorrectedValues :: Num a => Histogram a -> a -> a -> Histogram a
recordCorrectedValues = undefined

percentile :: Histogram a -> Float -> a
percentile = undefined
