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

import           System.Random (newStdGen, randoms)

import           Data.SuffixArray
import           Data.SuffixArray.Internal

main :: IO ()
main = do
  g <- newStdGen
  let rands :: [Int]
      rands = map (`mod` 200) . take 400000 $ randoms g
  print . take 5 . justSuffixes . suffixArrayOne $ rands
