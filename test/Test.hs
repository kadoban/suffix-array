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

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Data.List (tails)

import           Data.SuffixArray.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basics]

basics :: TestTree
basics = testGroup "tests of basic underlying utils and etc."
  [ QC.testProperty "suffixes == filter (not . null) tails" $
      \xs -> suffixes (xs :: [Int]) == filter (not . null) (tails xs)
  , QC.testProperty "(length . suffixes) == length" $
      \xs -> length (suffixes xs) == length (xs :: [Int])
  ]
