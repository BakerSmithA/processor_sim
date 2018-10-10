module Mem (Word32, Mem, Addr, zeroed, load, store) where

import Data.Word (Word32)
import Data.Array

type Addr = Word32
newtype Mem = Mem (Array Addr Word32)

zeroed :: Addr -> Mem
zeroed maxAddr = Mem (array (0, maxAddr) [(i, 0) | i <- [0..maxAddr]])

load :: Mem -> Addr -> Word32
load (Mem mem) addr = mem ! addr

store :: Mem -> Addr -> Word32 -> Mem
store (Mem mem) addr val = Mem (mem // [(addr, val)])
