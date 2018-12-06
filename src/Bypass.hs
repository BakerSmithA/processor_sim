module Bypass where

import Types
import Pipeline
import WriteBack

-- Data in the write-back stage that may be required for execution stage, e.g.
-- data dependencies:
--
--  ADD R0 R1 R2
--  SUB R3 R0 R4
--
-- There exists a data dependency because the value of R0 is computed by the ADD
-- and used in the SUB. Using a bypass, data is transferred backwards in the
-- piepline.
data Bypass = Empty
            | BypassReg PhyReg Val
            | BypassMem Addr Val
            deriving (Eq, Show)

empty :: Bypass
empty = Empty

-- Return values of registers and memory that will be written back, to be made
-- available to execution stage of pipeline.
fromWriteback :: WriteBack -> Bypass
fromWriteback (WriteReg reg val)  = BypassReg reg val
fromWriteback (WriteMem addr val) = BypassMem addr val
fromWriteback _ = Empty

-- Convenience method for generating bypass values from just executed stage of
-- pipeline. The value is fed back into the pipeline.
fromPipeline :: Pipeline -> Bypass
fromPipeline p = maybe Empty fromWriteback (fmap snd (executed p))

-- Return value of register written if matches given register index, and bypass
-- contains register write.
regVal :: PhyReg -> Bypass -> Maybe Val
regVal exp (BypassReg reg val) | exp == reg = Just val
                               | otherwise  = Nothing
regVal _ _ = Nothing

-- Return value of address written if matches given address, and bypass contains
-- memory write.
memVal :: Addr -> Bypass -> Maybe Val
memVal exp (BypassMem addr val) | exp == addr = Just val
                                | otherwise   = Nothing
memVal _ _ = Nothing
