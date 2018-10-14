module Mem (Mem, fromList, zeroed, load, store, maxAddr) where

import Data.Array

data Mem k v = Mem { arr :: Array k v, maxAddr :: k }
           deriving (Eq, Show)

fromList :: (Ix k, Num k, Enum k, Integral k) => [v] -> Mem k v
fromList xs = Mem { arr = array (0, len) (zip [0..] xs), maxAddr = len } where
    len = fromIntegral $ (length xs) - 1

zeroed :: (Ix k, Num k, Enum k, Num v, Integral k) => k -> Mem k v
zeroed maxAddr = fromList (take n (repeat 0)) where
    n = fromIntegral maxAddr + 1

load :: (Ix k, Show k) => Mem k v -> k -> v
load (Mem mem maxAddr) addr =
    if addr > maxAddr
        then error $ "Tried to access memory " ++ (show addr) ++ " / " ++ (show maxAddr)
        else mem ! addr

store :: (Ix k) => Mem k v -> k -> v -> Mem k v
store mem addr val = mem { arr = (arr mem) // [(addr, val)] }
