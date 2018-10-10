module Mem (Mem, zeroed) where

import Data.Word (Word32)
import Data.Array

newtype Mem = Mem (Array Word32 Word32)

zeroed :: Word32 -> Mem
zeroed size = Mem (array (0, size-1) [(i, 0) | i <- [0..size-1]])
