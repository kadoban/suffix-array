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
import           Data.SuffixArray.Internal

main :: IO ()
main = do
  let input :: [Int]
      input = take 400000 $ cycle [0 .. 199]
  print . take 5 . justSuffixes . suffixArrayOne $ input
