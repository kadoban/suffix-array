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
( suffixes
) where

-- | Yields the non-empty suffixes of a list in order of decreasing length.
--
-- This differs from `Data.List.tails` in that it does not include the
-- empty list at the end.
suffixes :: [a] -> [[a]]
suffixes xxs@(x:xs) = xxs : suffixes xs
suffixes [] = []
