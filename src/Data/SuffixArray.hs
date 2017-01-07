{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
( justSuffixes
, SuffixArray(..)
, suffixArray
, suffixArrayOne
) where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST)
import qualified Data.Array.IArray as A
import           Data.Array.IArray (Array)
import           Data.Array.MArray ( newArray, newListArray, newArray_
                                   , readArray, writeArray)
import           Data.Array.ST (STUArray, runSTUArray, getElems)
import           Data.Array.Unboxed (UArray)
import           Data.Array.Unsafe (unsafeFreeze)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef
                            , modifySTRef')

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

type Arr s = STUArray s Int Int

-- | Compute the suffix array of the given string(s) concatenated together
-- with `Sentinal`s after each.
--
-- O(n lg n) time
-- (where n is the sum of the string lengths + the number of strings)
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
      forM_ ranked $ uncurry (writeArray r)
      t <- newArray_ (0, n') -- scratch array
      go 1 s r t
    go :: forall s. Int -> Arr s -> Arr s -> Arr s -> ST s (Arr s)
    go k s r t
      | k >= n = return s
      | otherwise = do
      let getR i x = let ix = i + x
                      in if ix >= n then return 0
                                    else readArray r ix
          -- counting sort of suffixes, from s into s'
          -- ordered by the rank of suffix i + x, for suffix x
          csort i s s' = do
            (c :: Arr s) <- newArray (0, n') 0 -- counts
            let f = getR i
            forM_ [0 .. n'] $ \x -> do -- count how many of each rank there are
              x' <- f x
              v <- readArray c x'
              writeArray c x' (v+1)
            -- replace each element in 'c' with the starting index of
            -- elements with that value
            getElems c >>= (mapM_ (uncurry (writeArray c))
                          . zip [0 .. n'] . scanl (+) 0)
            elemsS <- (A.elems :: UArray Int Int -> [Int]) <$> unsafeFreeze s
            forM_ elemsS $ \x' -> do
              r' <- f x' -- rank of it
              idx <- readArray c r' -- where it goes, based on its rank
              writeArray c r' (idx + 1) -- next suffix with this rank goes
                                        -- one later
              writeArray s' idx x'
      csort k s t -- these two counting sorts comprise a radix sort of the
      csort 0 t s -- suffixes by their rank pairs
      -- now re-rank the suffixes in order
      fstSuffix <- readArray s 0
      prevVal <- ((,) <$> getR 0 fstSuffix <*> getR k fstSuffix) >>= newSTRef
      nextRank <- newSTRef 0
      elemsS <- (A.elems :: UArray Int Int -> [Int]) <$> unsafeFreeze s
      forM_ elemsS $ \x -> do
        val <- (,) <$> getR 0 x <*> getR k x
        val' <- readSTRef prevVal
        -- if its old rank pair is the same as of the previous suffix
        -- (in partially sorted order), it gets the same rank, otherwise
        -- we increase by one
        when (val /= val') $ modifySTRef' nextRank succ
        readSTRef nextRank >>= writeArray t x
        writeSTRef prevVal val
      nextRank' <- readSTRef nextRank
      if nextRank' < n'
        then go (k*2) s t r -- double the size of the prefix we're sorting by
        else return s -- ranks are already unique for all, stop early

-- | Convenience function to compute the suffix array of a single string.
-- (Still gets a `Sentinal` at the end)
--
-- O(n lg n)
-- (where n is the length of the string)
suffixArrayOne :: Ord a => [a] -> SuffixArray a
suffixArrayOne = suffixArray . pure

-- | Convenience function to just give a list of the suffixes in
-- lexicographic order.
justSuffixes :: SuffixArray a -> [Int]
justSuffixes = A.elems . toSuffixes
