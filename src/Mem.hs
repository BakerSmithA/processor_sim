module Mem where

import Data.Array

data Mem k v = Mem { arr :: Array k v, maxAddr :: k }
           deriving (Eq, Show)

-- Return memory containing values in list.
fromList :: (Ix k, Num k, Enum k, Integral k) => [v] -> Mem k v
fromList xs = Mem { arr = array (0, len) (zip [0, 1, 2, 3] xs), maxAddr = len } where
    len = fromIntegral $ (length xs) - 1

f :: (Ix k, Num k, Enum k, Integral k) => [v] -> Mem k v
f xs = Mem { arr = listArray (0, len) xs, maxAddr = 0 } where
    len = fromIntegral $ (length xs) - 1

-- Return memory containing all zeros.
zeroed :: (Ix k, Num k, Enum k, Num v, Integral k) => k -> Mem k v
zeroed maxAddr = fromList (take n (repeat 0)) where
    n = fromIntegral maxAddr + 1

-- Checks index is within range 0 to maxAddr, inclusive. If so the operation
-- is performed. Otherwise Nothing is returned.
checkedAddr :: (Num k, Ix k) => (k -> Mem k v -> a) -> k -> Mem k v -> Maybe a
checkedAddr op i m@(Mem mem maxAddr) =
    if i > maxAddr || i < 0
        then Nothing
        else Just (op i m)

-- Returns value from memory, or Nothing if index is invalid.
load :: (Num k, Ix k) => k -> Mem k v -> Maybe v
load = checkedAddr (\i mem -> (arr mem) ! i)

-- Returns new state of memory after storing value, or nothing if index is invalid.
store :: (Num k, Ix k) => k -> v -> Mem k v -> Maybe (Mem k v)
store i val = checkedAddr f i where
    f i' mem = mem { arr = (arr mem) // [(i', val)] }
