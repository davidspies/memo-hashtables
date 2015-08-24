module Data.HashTable.Memo where

import Data.Hashable
import Data.HashTable.IO
import Prelude hiding (lookup)
import System.IO.Unsafe

memo :: (Eq k, Hashable k) => (k -> v) -> k -> v
memo f = nf where
    t :: CuckooHashTable k v
    {-# NOINLINE t #-}
    t = unsafePerformIO new
    nf k = unsafePerformIO $ do
        r <- lookup t k
        case r of
            Just v -> return v
            Nothing -> do
                let v = f k
                insert t k v
                return v
    {-# NOINLINE nf #-}