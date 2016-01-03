{-# LANGUAGE ScopedTypeVariables #-}
module Test.Utils where

import           Control.Monad.ST                  (runST)
import           Data.Bits                         (Bits)
import           Data.HdrHistogram.Config.Internal
import           Data.Tagged                       (Tagged (Tagged))
import           Data.Vector                       ((!))
import qualified Data.Vector                       as U
import           Data.Vector.Algorithms.Intro      (sort)
import           System.Random                     (Random)
import           Test.QuickCheck

data ConfigAndVals c a = ConfigAndVals (Tagged c (HistogramConfig a)) [a]
                    deriving (Show, Eq)

instance (Random a, Arbitrary a, Bounded a, Integral a, Bits a) => Arbitrary (ConfigAndVals c a) where
  arbitrary = do
    config' <- arbitrary
    vals <- listOf1 $ choose (lowest config', highest config')
    return $ ConfigAndVals (Tagged config') vals

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
