module WriteBack where

import Types

type Valid = Bool
type IsBranch = Bool

data LoadData
    = None IsBranch
    | ValidLoad Addr
    | InvalidLoad
    deriving (Show, Eq)

invalidateLoadData :: Addr -> LoadData -> LoadData
invalidateLoadData _       (None isBranch)  = None isBranch
invalidateLoadData _       (InvalidLoad)    = InvalidLoad
invalidateLoadData chkAddr (ValidLoad addr) | chkAddr == addr = InvalidLoad
                                            | otherwise       = ValidLoad addr

-- Generated by execution step of pipeline.
-- FInstruction to machine of values to update.
data WriteBack
    -- Valid used to determine whether to flush the pipeline if a load is
    -- determined to be invalid.
    = WriteReg PhyReg Val LoadData
    | WriteMem Addr Val
    | WritePrint String
    | NoOp
    | Terminate
    deriving (Show, Eq)

-- Invalidates a load instruction if it has a matching address.
invalidateLoad :: Addr -> WriteBack -> WriteBack
invalidateLoad addr (WriteReg r v ld) = WriteReg r v ld' where
    ld' = invalidateLoadData addr ld
invalidateLoad _ wb = wb

isInvalidLoad :: WriteBack -> Bool
isInvalidLoad (WriteReg _ _ InvalidLoad) = True
isInvalidLoad _ = False
