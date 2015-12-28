module Test.Expectations (
  shouldBeLessThan,
  shouldBeLessThanOrEqual,
  shouldBeGreaterThan
  ) where

import           Control.Monad (unless)
import           Test.Hspec

expectTrue :: String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

compareWith :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

shouldBeLessThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeLessThan = compareWith (>) "is not less than"

shouldBeLessThanOrEqual :: (Show a, Ord a) => a -> a -> Expectation
shouldBeLessThanOrEqual = compareWith (>=) "is not less than"

shouldBeGreaterThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeGreaterThan = compareWith (<) "is not greater than"
