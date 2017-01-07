-- |
-- Module      :  Main
-- Copyright   :  Joshua Simmons 2017
-- License     :  BSD3
--
-- Maintainer  :  joshua.simmons@emptypath.com
--
-- suffix-array benchmarks using criterion
--
module Main
( main
) where

import           Criterion.Main
import           System.Random (newStdGen, randoms)

import           Data.SuffixArray
import           Data.SuffixArray.Internal

main :: IO ()
main = do
  g <- newStdGen
  let rands, sorts, revs :: [Int]
      rands = randoms g
      sorts = [1..]
      revs = [1000000, 999999 ..]
      reps = concatMap (\x -> replicate x x) [1..]
      allDists = [rands, sorts, reps, revs]
  defaultMain
   [
    bgroup "single_suffixes"
    [ bench (unwords [show (sz, k), dist', var'])
          $ whnf (\(n,k) -> var (take n (map (`mod` k) dist))) (sz, k)
    | (dist, dist') <- [ (rands, "rands"), (sorts, "sorts")
                       , (revs, "revs"), (reps, "reps")]
    , k <- [5, 40, 90, 200, 1000]
    , sz <- [5000, 10000 .. 100000] ++ [200000]
    , (var, var') <- [ (naiveOne, "naiveOne")
                     , (justSuffixes . suffixArrayOne, "suffixArrayOne")]
    , interesting var' sz k dist'
    ]
   ,bgroup "all_together"
    [ bench (unwords [show (sz, k), var'])
          $ whnf (\(n,k) -> let n' = n `div` length allDists
                             in var (map (take n' . map (`mod` k)) allDists))
                 (sz, k)
    | k <- [5, 40, 90, 200, 1000]
    , sz <- [5000, 10000 .. 200000]
    , (var, var') <- [ (naive, "naive")
                     , (justSuffixes . suffixArray, "suffixArray")]
    , interesting var' sz k "all"
    ]
   ]

interesting "naiveOne" n k v
  | n > 40000 && v `elem` ["sorts", "revs"] = False
  | n > 100000 = False
  | otherwise = True
interesting "naive" n k v
  | k < 40 && n > 60000 = False
  | n > 100000 = False
  | otherwise = True
interesting _ _ _ _ = True
