module Reg (RegFile, RegIdx, file, Reg.read, Reg.write) where

import Data.Word
import Mem

type RegIdx = Word8
type RegVal = Word32

newtype RegFile = RegFile (Mem RegIdx RegVal)
                deriving (Show)

file :: RegIdx -> RegFile
file maxIdx = RegFile (zeroed maxIdx)

read :: RegFile -> RegIdx -> Word32
read (RegFile file) idx = load file idx

write :: RegFile -> RegIdx -> Word32 -> RegFile
write (RegFile file) idx val = RegFile (store file idx val)
