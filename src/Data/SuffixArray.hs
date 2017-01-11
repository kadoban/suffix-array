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
( justAlphas
, justLcp
, justSuffixes
, SuffixArray(..)
, suffixArray
, suffixArrayOne
) where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST)
import qualified Data.Array.IArray as A
import           Data.Array.IArray (Array, (!))
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
                       , toLcp :: UArray Int Int
                         -- ^ Longest Common Prefix of each suffix with the
                         -- previous one in lexicographic order
                       }
  deriving (Eq, Ord, Show)

type Arr s = STUArray s Int Int

-- | Compute the suffix array of the given string(s) concatenated together
-- with `Sentinal`s after each.
--
-- O(n lg n) time
-- (where n is the sum of the string lengths + the number of strings)
suffixArray :: Ord a => [[a]] -> SuffixArray a
suffixArray xs = SuffixArray ss as lcp
  where
    n = snd $ A.bounds as
    as = let ps = prepare xs
             n = length ps - 1
          in A.listArray (0, n) ps
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
    orderedByHead = sortBy (comparing snd) . zip [0 ..] $ A.elems as
    ranked = let (as, bs) = unzip orderedByHead
              in zip as (rank bs)
    ss :: UArray Int Int
    ss = runSTUArray $ do
      s <- newListArray (0, n) (map fst ranked) -- the suffixes
      r <- newArray_ (0, n) -- the rank of each suffix
      forM_ ranked $ uncurry (writeArray r)
      t <- newArray_ (0, n) -- scratch array
      c <- newArray_ (0, n) -- counts array
      go 1 s r t c
    -- After each iteration of `go`, the suffixes are sorted by their
    -- k*2 first characters. k doubles each time, and in each iteration
    -- we do O(n) work and are then ready for the next iteration.
    go :: forall s. Int -> Arr s -> Arr s -> Arr s -> Arr s -> ST s (Arr s)
    go k s r t c
      | k > n = return s
      | otherwise = do
      let getR 0 x = readArray r x
          getR i x = let ix = i + x
                      in if ix > n then return 0
                                   else readArray r ix
          -- counting sort of suffixes, from s into s'
          -- ordered by the rank of suffix i + x, for suffix x
          -- (that is, suffix x without its first i characters)
          csort i s s' = do
            forM_ [0 .. n] $ flip (writeArray c) 0 -- zero out the counts
            let f = getR i
            -- count how many of each rank there are
            writeArray c 0 i -- takes care of all that would be automatically 0
            forM_ [i .. n] $ \x -> do -- count the appropriate values in r
              x' <- readArray r x
              v <- readArray c x'
              writeArray c x' (v+1)
            -- replace each element in c with the starting index of
            -- elements with that value
            soFar <- newSTRef 0
            forM_ [0 .. n] $ \x -> do
              v <- readArray c x
              readSTRef soFar >>= writeArray c x
              modifySTRef' soFar (+v)
            elemsS <- (A.elems :: UArray Int Int -> [Int]) <$> unsafeFreeze s
            forM_ elemsS $ \x -> do
              r' <- f x -- rank of it
              idx <- readArray c r' -- where it goes, based on its rank
              writeArray c r' (idx + 1) -- next suffix with this rank goes
                                        -- one later
              writeArray s' idx x
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
      maxRank <- readSTRef nextRank
      if maxRank < n
        then go (k*2) s t r c -- double the size of the prefix we're sorting by
        else return s -- ranks are already unique for all, stop early
    -- LCP array in the same order as the suffix array
    lcp = A.ixmap (0, n) (ss !) plcp
    -- PLCP, permuted LCP array which is in order by position instead of
    -- lexicographic order by the suffix being referred to.
    --
    -- Algoritm is courtesy of the paper "Permuted Longest-Common-Prefix
    -- Array" by Kärkkäinen, et al.
    -- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.186.2185
    -- (several PDFs available free online)
    --
    -- This runs in O(n) time
    plcp = runSTUArray plcp'
    plcp' :: forall s. ST s (Arr s)
    plcp' = do
      -- keep track of what suffix is before each one, in lexicographic
      -- order (the `first` one has none before it, so we treat it special)
      let first = ss ! 0
      (prev :: Arr s) <- newArray_ (0, n)
      forM_ [1 .. n] $ \i -> writeArray prev (ss ! i) (ss ! (i-1))
      len <- newSTRef 0
      res <- newArray_ (0, n)
      forM_ [0 .. n] $ \i ->
        if i == first -- no previous prefix
          then writeSTRef len 0 >> writeArray res i 0
          else do
            -- See the PLCP Array paper for details, but the important
            -- part is that PLCP[i] >= PLCP[i-1] - 1, which lets us
            -- skip a *lot* of character comparisons in the worst-case
            --
            -- This is otherwise essentially the same as the naive LCP
            -- computation (see 'Data.SuffixArray.Internal.naiveLcp')
            len' <- readSTRef len
            prev' <- readArray prev i
            let suffixOff x = map (as !) [x ..]
                newMatching = length . takeWhile id
                            $ zipWith (==) (suffixOff (i + len'))
                                           (suffixOff (prev' + len'))
            writeArray res i (len' + newMatching)
            writeSTRef len $ max 0 (len' + newMatching - 1)
      return res

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

-- | Convenience function to just give a list characters in the
-- concatenated original strings.
justAlphas :: SuffixArray a -> [Alpha a]
justAlphas = A.elems . toAlphas

-- | Convenience function to just give a list of the longest common
-- prefix of every suffix with the previous suffix in lexicographic
-- order.
justLcp :: SuffixArray a -> [Int]
justLcp = A.elems . toLcp
