module Mem (Word32, Mem, Addr, zeroed, load, store) where

import Data.Word (Word32)
import Data.Array

type Addr = Word32
newtype Mem a = Mem (Array Addr a)

fromList :: [a] -> Mem a
fromList xs = Mem $ array (0, len) (zip [0..] xs) where
    len = fromIntegral $ length xs

zeroed :: Addr -> Mem Word32
zeroed maxAddr = fromList [0..maxAddr]

load :: Mem a -> Addr -> a
load (Mem mem) addr = mem ! addr

store :: Mem a -> Addr -> a -> Mem a
store (Mem mem) addr val = Mem (mem // [(addr, val)])
