module Mem (Word32, Mem, Addr, fromList, zeroed, load, store, maxAddr) where

import Data.Word (Word32)
import Data.Array

type Addr = Word32
data Mem a = Mem { arr :: Array Addr a, maxAddr :: Addr }
           deriving (Show)

fromList :: [a] -> Mem a
fromList xs = Mem { arr = array (0, len) (zip [0..] xs), maxAddr = len } where
    len = fromIntegral $ (length xs) - 1

zeroed :: Addr -> Mem Word32
zeroed maxAddr = fromList (take n (repeat 0)) where
    n = fromIntegral maxAddr + 1

load :: Mem a -> Addr -> a
load (Mem mem _) addr = mem ! addr

store :: Mem a -> Addr -> a -> Mem a
store mem addr val = mem { arr = (arr mem) // [(addr, val)] }
