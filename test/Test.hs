-- |
-- Module      :  Main
-- Copyright   :  Joshua Simmons 2017
-- License     :  BSD3
--
-- Maintainer  :  joshua.simmons@emptypath.com
--
-- suffix-array test-suite using tasty and etc.
--
module Main
( main
) where

import qualified Data.Set as S
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Data.List (sort, tails)

import           Data.SuffixArray.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basics, naives]

basics :: TestTree
basics = testGroup "tests of basic underlying utils and etc."
  [ QC.testProperty "suffixes == filter (not . null) tails" $
      \xs -> suffixes (xs :: [Int]) == filter (not . null) (tails xs)
  , QC.testProperty "(length . suffixes) == length" $
      \xs -> length (suffixes xs) == length (xs :: [Int])
  ]

naives :: TestTree
naives = testGroup "test the naive implementation to make sure it works"
  [ testCase "banana" $
      naiveOne "banana" @?= [6, 5, 3, 1, 0, 4, 2]
  , QC.testProperty "length" $
      \xs -> length (xs :: [Char]) + 1 == length (naiveOne xs)
  , QC.testProperty "distinct" $
      \xs -> let xs' = naiveOne (xs :: [Char])
              in length xs' == length (distinct xs')
  , QC.testProperty "distinct2" $
      \xs -> let xs' = naiveOne (xs :: [Int])
              in sort xs' == distinct xs'
  ]

distinct :: Ord a => [a] -> [a]
distinct = S.toList . S.fromList
