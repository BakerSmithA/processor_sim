module Bypass where

import Types
import WriteBack
import Helper (tryPick)

-- Data in the write-back stage that may be required for execution stage, e.g.
-- data dependencies:
--
--  ADD R0 R1 R2
--  SUB R3 R0 R4
--
-- There exists a data dependency because the value of R0 is computed by the ADD
-- and used in the SUB. Using a bypass, data is transferred backwards in the
-- piepline.
data BypassEntry = BypassReg PhyReg Val
                 | BypassMem Addr Val
                 deriving (Eq, Show)

type Bypass = [BypassEntry]

empty :: Bypass
empty = []

fromList :: [BypassEntry] -> Bypass
fromList = id

-- Convenience method for generating bypass values from just executed stage of
-- pipeline. The value is fed back into the pipeline.
fromWbs :: [WriteBack] -> Bypass
fromWbs = foldr f [] where
    f wb acc  =
        case fromWriteback wb of
            Nothing    -> acc
            Just entry -> (entry:acc)

-- Return values of registers and memory that will be written back, to be made
-- available to execution stage of pipeline.
fromWriteback :: WriteBack -> Maybe BypassEntry
fromWriteback (WriteReg reg val)  = Just $ BypassReg reg val
fromWriteback (WriteMem addr val) = Just $ BypassMem addr val
fromWriteback _                   = Nothing

-- Return value of register written if matches given physical index.
regVal :: PhyReg -> Bypass -> Maybe Val
regVal = tryPick . entryRegVal

-- Return value of address written if matches given address, and bypass contains
-- memory write.
memVal :: Addr -> Bypass -> Maybe Val
memVal = tryPick . entryMemVal

-- Return value of register written if matches given physical index.
entryRegVal :: PhyReg -> BypassEntry -> Maybe Val
entryRegVal exp (BypassReg reg val) | exp == reg = Just val
                                    | otherwise  = Nothing
entryRegVal _ _ = Nothing

-- Return value of address written if matches given address.
entryMemVal :: Addr -> BypassEntry  -> Maybe Val
entryMemVal exp (BypassMem addr val) | exp == addr = Just val
                                     | otherwise   = Nothing
entryMemVal _ _ = Nothing
