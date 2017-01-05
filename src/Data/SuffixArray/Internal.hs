-- |
-- Module      :  Data.SuffixArray.Internal
-- Copyright   :  Joshua Simmons 2017
-- License     :  BSD3
--
-- Maintainer  :  joshua.simmons@emptypath.com
--
-- Stability   :  unstable
--
-- Internal implementation details, unstable and not
-- to be relied upon for any reason.
--
module Data.SuffixArray.Internal
( Alpha(..)
, naive
, naiveOne
, prepare
, prepareOne
, rank
, suffixes
) where

import           Data.List (group, sortBy)
import           Data.Ord (comparing)

-- | Yields the non-empty suffixes of a list in order of decreasing length.
--
-- This differs from `Data.List.tails` in that it does not include the
-- empty list at the end.
suffixes :: [a] -> [[a]]
suffixes xxs@(x:xs) = xxs : suffixes xs
suffixes [] = []

-- | A character in a string (or set of strings) we're going to compute the
-- suffix array of.
-- Includes `Sentinal` markers for the end of strings.
data Alpha a = Sentinal Int -- ^ Used to mark the end of a string.
                            -- The `Int` parameter is used to encode
                            -- which string this is the end of, in cases
                            -- where there are multiple.
             | Alpha a -- ^ An actual character in the string.
    deriving (Eq, Ord, Show)

-- | Convenience value containing `Sentinal`s in order.
sentinals :: [Alpha a]
sentinals = map Sentinal [0..]

-- | Prepare a list of strings to compute the suffix array of them.
-- Just wraps every character in `Alpha` and adds `Sentinal`s to the end of
-- each string, and concatenates it together.
prepare :: [[a]] -> [Alpha a]
prepare = concat . zipWith (\a b -> b ++ [a]) sentinals . map (map Alpha)

-- | Convenience function to `prepare` a single string.
prepareOne :: [a] -> [Alpha a]
prepareOne = prepare . pure

-- | A naively implemented suffix array implementation which will be used
-- for correctness checking and possibly to benchmark against. Shouldn't
-- usually be used in production code, as it is quite slow.
--
-- O(n^2 lg n)
-- (where n is the sum of the string lengths + the number of strings)
naive :: Ord a => [[a]] -> [Int]
naive =
  map fst . sortBy (comparing snd) . zip [0 ..] . suffixes . prepare

-- | Convenience wrapper around `naive` for a single string.
naiveOne :: Ord a => [a] -> [Int]
naiveOne = naive . pure

-- | Take a sorted list of elements and replace each value with an `Int`
-- such that any comparisons between elements in the original list would
-- yield exactly the same result in the output list.
--
-- i.e.: let rs = rank xs
--        in all [ (xs!!i) `compare` (xs!!j) == (rs!!i) `compare` (rs!!j)
--               | let idx = [0 .. length xs - 1], i <- idx, j <- idx
--               ]
rank :: Ord a => [a] -> [Int]
rank = concat . zipWith (\n -> map (const n)) [0 ..] . group
