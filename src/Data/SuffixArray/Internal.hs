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
, prepare
, prepareOne
, suffixes
) where

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
