-- |
-- Module      :  Main
-- Copyright   :  Joshua Simmons 2017
-- License     :  BSD3
--
-- Maintainer  :  joshua.simmons@emptypath.com
--
-- a single usage of the library for profiling use
--
module Main
( main
) where

import           Data.SuffixArray

main :: IO ()
main = do
  let input :: [Int]
      input = take 400000 $ cycle [0 .. 199]
  print . (\x -> (take 5 . justSuffixes $ x, take 5 . justLcp $ x)) . suffixArrayOne $ input
