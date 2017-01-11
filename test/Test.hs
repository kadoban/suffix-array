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

import qualified Data.Array.IArray as A
import qualified Data.Set as S
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Data.List (sort, tails)

import           Data.SuffixArray
import           Data.SuffixArray.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basics, naives, actual, actualLcp]

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
      naiveOne "banana" @?= [6, 5, 3, 1, 0, 4, 2] -- wikipedia example
  , testCase "banana hammock" $ -- hand calculated
      naive ["banana", "hammock"] @?= [6,14,5,8,3,1,0,12,7,13,9,10,4,2,11]
  , QC.testProperty "length" $
      \xs -> length (xs :: [Char]) + 1 == length (naiveOne xs)
  , QC.testProperty "distinct" $
      \xs -> let xs' = naiveOne (xs :: [Char])
              in length xs' == length (distinct xs')
  , QC.testProperty "distinct2" $
      \xs -> let xs' = naiveOne (xs :: [Int])
              in sort xs' == distinct xs'
  , QC.testProperty "length of many" $
      \xs -> sum (map length xs) + length (xs :: [[Char]]) == length (naive xs)
  , QC.testProperty "empty first" $
      \xs -> head (naiveOne xs) == length (xs :: [Integer])
  ]

actual :: TestTree
actual = testGroup "test the actual implementation to make sure it works"
  [ QC.testProperty "against naiveOne" $
      \xs -> naiveOne (xs :: [Int]) == A.elems (toSuffixes (suffixArrayOne xs))
  , QC.testProperty "against naive" $
      \xs -> naive (xs :: [[Int]]) == A.elems (toSuffixes (suffixArray xs))
  , testCase "[0]" $
      A.elems (toSuffixes $ suffixArrayOne [0]) @?= [1, 0]
  ]

actualLcp :: TestTree
actualLcp = testGroup "test the actual implementation of LCP stuff"
  [ QC.testProperty "against naiveLcpOne" $
      \xs -> naiveLcpOne (xs :: [Int]) == justLcp (suffixArrayOne xs)
  , QC.testProperty "against naiveLcp" $
      \xs -> naiveLcp (xs :: [[Int]]) == justLcp (suffixArray xs)
  ]

distinct :: Ord a => [a] -> [a]
distinct = S.toList . S.fromList
