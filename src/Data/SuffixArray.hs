-- |
-- Module      :  Data.SuffixArray
-- Copyright   :  Joshua Simmons 2017
-- License     :  BSD3
--
-- Maintainer  :  joshua.simmons@emptypath.com
--
-- Suffix array library main module
--
module Data.SuffixArray
( SuffixArray(..)
) where

import           Control.Monad (forM_)
import           Control.Monad.ST (ST)
import qualified Data.Array.IArray as A
import           Data.Array.IArray (Array)
import           Data.Array.MArray ( newArray, newListArray, newArray_
                                   , readArray, writeArray)
import           Data.Array.ST (STUArray, runSTUArray)
import           Data.Array.Unboxed (UArray)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Data.SuffixArray.Internal

-- | Holds the suffix array data
data SuffixArray a = SuffixArray
                       { toSuffixes :: UArray Int Int
                         -- ^ The actual array of suffixes in lexicographic
                         -- order.
                       , toAlphas :: Array Int (Alpha a)
                         -- ^ The original string(s) with `Sentinal` values
                         -- included after each string.
                       }
  deriving (Eq, Ord, Show)

suffixArray :: Ord a => [[a]] -> SuffixArray a
suffixArray xs = SuffixArray ss (A.listArray (0, n') ps)
  where
    ps = prepare xs
    n = length ps
    n' = n - 1
    -- we represent each suffix as the number of characters we have
    -- to drop from the original string to get that suffix
    --
    -- and then we order them by their first letter and convert those
    -- first letters into `rank`s, which are `Int`s that preserve the
    -- same `Ord`ering. This is useful so we can sort them more easily
    -- (allows using counting sort), and will help code reuse in the
    -- main body.
    --
    -- Note: We actually don't care about the ordering of suffixes yet,
    -- it's just necessary to use the `rank` function.
    orderedByFirst = sortBy (comparing snd) . zip [0 ..] $ ps
    ranked = let (as, bs) = unzip orderedByFirst
              in zip as (rank bs)
    ss :: UArray Int Int
    ss = runSTUArray $ do
      s <- newListArray (0, n') (map fst ranked) -- the suffixes
      r <- newArray_ (0, n') -- the rank of each suffix
      forM_ ranked $ \(suffix, rank) -> writeArray r suffix rank
