module Reg (RegFile) where

import Mem

type RegIdx = Int
newtype RegFile = RegFile Mem

file :: Int -> RegFile
file numRegs = RegFile (zeroed maxIdx) where
    maxIdx = fromIntegral (numRegs - 1)

read :: RegFile -> RegIdx -> Word32
read (RegFile file) idx = load file (fromIntegral idx)

write :: RegFile -> RegIdx -> Word32 -> RegFile
write (RegFile file) idx val = RegFile (store file idx' val) where
    idx' = fromIntegral idx
