module VM where

import State as St
import Error
import Instr
import Pipeline as P
import qualified Mem as Mem
import qualified Mem as Reg
import qualified Bypass as BP

-- E.g. Mult, Add, And, Or, etc
type ValOp = (Val -> Val -> Val)

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data VM a = VM a
          | Crash Error State
          | Exit State
          deriving (Eq, Show)

instance Functor VM where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (VM x)       = VM (f x)
    fmap _ (Crash e st) = Crash e st
    fmap _ (Exit st)    = Exit st

instance Applicative VM where
    -- pure :: a -> VM a
    pure = VM
    -- (<*>) :: f (a -> b) -> f a -> f b
    (VM f)       <*> vm = fmap f vm
    (Crash e st) <*> _ = Crash e st
    (Exit st)    <*> _ = Exit st

instance Monad VM where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (VM x)       >>= f = f x
    (Crash e st) >>= _ = Crash e st
    (Exit st)    >>= _ = Exit st

-- Adds PC address at time of crash.
crash :: (InstrAddr -> Error) -> State -> VM a
crash f st = do
    pc <- fmap fromIntegral (regVal (pcIdx st) st)
    Crash (f pc) st

-- Return value of a register, from bypass or register. Crash if invalid index.
regVal :: RegIdx -> State -> VM Val
regVal i st =
    case BP.regVal i (bypass st) of
        Just val -> return val
        Nothing ->
            case Reg.load i (regs st) of
                Nothing  -> crash (RegOutOfRange i) st
                Just val -> return val

-- Returns value of an address from bypass or memory. Crash if invalid address.
memVal :: Addr -> State -> VM Val
memVal i st =
    case BP.memVal i (bypass st) of
        Just val -> return val
        Nothing ->
            case Mem.load i (mem st) of
                Nothing  -> crash (MemOutOfRange i) st
                Just val -> return val

-- Loads contents of memory at address into register.
load :: Addr -> RegIdx -> State -> VM WriteBack
load addr r st = do
    val <- memVal addr st
    return (WriteReg r val)

-- Stores contents of register into memory address.
store :: RegIdx -> Addr -> State -> VM WriteBack
store r addr st = do
    val <- regVal r st
    return (WriteMem addr val)

-- Perform operation on value stored in register, and immediate value.
opI :: RegIdx -> RegIdx -> ValOp -> Val -> State -> VM WriteBack
opI r x f imm st = do
    val <- regVal x st
    return (WriteReg r (f val imm))

-- Perform operation on two values stored in registers.
opReg :: RegIdx -> RegIdx -> ValOp -> RegIdx -> State -> VM WriteBack
opReg r x f y st = do
    val <- regVal y st
    opI r x f val st

eqVal :: Val -> Val -> Val
eqVal x y | x == y    = 1
          | otherwise = 0

ltVal :: Val -> Val -> Val
ltVal x y | x < y     = 1
          | otherwise = 0

orVal :: Val -> Val -> Val
orVal x y | x == 1 || y == 1 = 1
          | otherwise        = 0

andVal :: Val -> Val -> Val
andVal x y | x == 1 && y == 1 = 1
           | otherwise        = 0

notVal :: Val -> Val
notVal x | x == 1    = 0
         | otherwise = 1

-- Perform fetch state of pipeline by retreiving instruction. Or, return Nothing
-- if the value of the pc is after the last instruction.
fetch :: State -> VM (Maybe Instr)
fetch st = do
    pc <- fmap fromIntegral (regVal (pcIdx st) st)
    case Mem.load pc (instrs st) of
        Nothing    -> return Nothing
        Just instr -> return (Just instr)

-- Perform decode stage of pipeline.
decode :: Instr -> VM Instr
decode = return

-- Executes a branch by writing PC.
branch :: Addr -> State -> VM WriteBack
branch addr st = return (WriteReg pc addr') where
    pc = pcIdx st
    -- +1 because pipeline stalls until branch executed, and PC not updated.
    addr' = fromIntegral (addr+1)

-- Executes a branch if the value in a register passes a condition, otherwise NoOp.
branchCond :: RegIdx -> (Val -> Bool) -> Addr -> State -> VM WriteBack
branchCond reg cond addr st = do
    val <- regVal reg st
    if cond val
        then branch addr st
        else return NoOp

-- Perform execution stage of pipeline, and generate instruction of what
-- to modify in machine.
exec :: Instr -> State -> VM WriteBack
-- Memory
-- Move immediate value into register.
exec (MoveI r val) st =
    return (WriteReg r val)
-- Move value from one register into another.
exec (Move r from) st = do
    val <- regVal from st
    return (WriteReg r val)
-- Load value from memory into register, where reg and immediate provide
-- base and offset.
exec (LoadIdx r base offset) st = do
    baseAddr <- regVal base st
    load (fromIntegral (baseAddr + offset)) r st
-- Load value from memory into register, where two regs provide base and offset.
exec (LoadBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    load (fromIntegral (baseAddr + offsetAddr)) r st
-- Store value from register into memory, where reg and immediate provide
-- base and offset.
exec (StoreIdx r base offset) st = do
    baseAddr <- regVal base st
    store r (fromIntegral (baseAddr + offset)) st
-- Store value from register into memory, where two regs provide base and offset.
exec (StoreBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    store r (fromIntegral (baseAddr + offsetAddr)) st
-- Arithmetic/Logic
exec (Add r x y)  st = opReg r x (+) y st
exec (AddI r x i) st = opI r x (+) i st
exec (Sub r x y)  st = opReg r x (-) y st
exec (SubI r x i) st = opI r x (-) i st
exec (Mult r x y) st = opReg r x (*) y st
exec (Div r x y)  st = opReg r x (div) y st
exec (Eq r x y)   st = opReg r x eqVal y st
exec (Lt r x y)   st = opReg r x ltVal y st
exec (Or r x y)   st = opReg r x orVal y st
exec (And r x y)  st = opReg r x andVal y st
exec (Not r x)    st = do
    val <- regVal x st
    return (WriteReg r (notVal val))
-- Branching
-- Unconditional branch to address.
exec (B addr) st = branch addr st
-- Branch if value in register is true.
exec (BT r addr) st = branchCond r (==1) addr st
-- Branch if value in register is false.
exec (BF r addr) st = branchCond r (==0) addr st
-- Branch to value stored in link register.
exec (Ret) st = do
    addr <- regVal (lrIdx st) st
    branch (fromIntegral addr) st
-- Terminate execution of the program.
exec (SysCall) st =
    return Terminate
-- Debugging.
-- Print value in register.
exec (Print r) st = do
    val <- regVal r st
    return (WritePrint (show val))
-- Print newline.
exec (PrintLn) st =
    return (WritePrint "\n")

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: RegIdx -> Val -> State -> VM State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> crash (RegOutOfRange i) st
        Just regs -> return st { regs = regs }

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> VM State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> crash (MemOutOfRange i) st
        Just mem -> return st { mem = mem }

-- Add string to output of VM.
addOutput :: String -> State -> VM State
addOutput s st = return st { output = (output st) ++ s }

-- Perform write-back stage of pipeline, writing result back to register/memory.
writeBack :: WriteBack -> State -> VM State
writeBack instr st = do
    st' <- writeBack' instr
    return (St.incExec st')
        where
            writeBack' (WriteReg r val) = setRegVal r val st
            writeBack' (WriteMem i val) = setMemVal i val st
            writeBack' (WritePrint s)   = addOutput s st
            writeBack' (NoOp)           = return st
            writeBack' (Terminate)      = Exit st

-- Increment PC by 1.
incPc :: State -> VM State
incPc st = do
    pc <- regVal (pcIdx st) st
    setRegVal (pcIdx st) (pc+1) st

-- Return whether the pipeline should stall to wait for branch instructions
-- to be executed, i.e. if there are branch instructions in the fetch or decode
-- stages. Do not need to check for execute stage because write-back results
-- are available via bypass.
shouldStall :: State -> Pipeline -> Bool
shouldStall st p = f || d where
    f  = maybe False isBranch (fetched p)
    d  = maybe False isBranch (decoded p)

-- Shifts instructions through pipeline.
advancePipeline :: Maybe Instr -> State -> Pipeline -> VM (State, Pipeline)
advancePipeline fetched st p = do
    let executer = (flip exec) st
        writer   = (flip writeBack) st
    (st', p') <- P.advance fetched decode executer writer p
    -- No write-back instructions may have been executed, in which case the state
    -- is not updated. Therefore, return old state.
    let st'' = maybe st id st'
    return (st'', p')

-- Returns state which contains bypass value that was just written as part of
-- the write-back stage of the pipeline. This makes this value available to
-- previous stages of the pipeline.
bypassed :: State -> Pipeline -> State
bypassed st p = St.withBypass b st where
    b = BP.fromPipeline p

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> VM (State, Pipeline)
cycle st p = do
    fetched <- fetch st
    (st', p') <- advancePipeline fetched st p
    incSt <- incPc st'
    return (bypassed incSt p', p')

-- Shift instructions through pipeline without fetching a new instruction.
-- PC is also NOT updated.
cycleStall :: State -> Pipeline -> VM (State, Pipeline)
cycleStall st p = do
    (st', p') <- advancePipeline Nothing st p
    return (bypassed st' p', p')

-- Run VM to completion, i.e. until exit system call occurs.
runPipeline :: State -> Pipeline -> VM (State, Pipeline)
runPipeline st p = do
    let x = if not (shouldStall st p)
                then VM.cycle st p
                else VM.cycleStall st p
    (st', p') <- x
    runPipeline (St.incCycles st') p'

-- Run VM to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ show st)
        VM x       -> error ("Did not terminate:" ++ show x)
