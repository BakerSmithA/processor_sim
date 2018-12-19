module Timer where

type CyclesRem = Word

-- Used to time how long an instruction has before it is 'ready', i.e. has been
-- processed in an ALU.
data Timer a
    = Done a
    | Tick CyclesRem a
    deriving (Eq, Show)

start :: CyclesRem -> a -> Timer a
start r x = Tick r x

tick :: Timer a -> Timer a
tick (Tick 0 x) = Done x
tick (Tick n x) = Tick (n-1) x
tick (Done x)   = Done x
