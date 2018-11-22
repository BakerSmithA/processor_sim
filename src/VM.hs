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
