module Result
( Result(..)
, module Error)
where

import Error
import State

-- Whether result of a computation crashes is okay, crashes the VM, or exits VM.
data Result a = OK a
              | Crash Error State
              | Exit State
              deriving (Eq, Show)

instance Functor Result where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (OK x)       = OK (f x)
    fmap _ (Crash e st) = Crash e st
    fmap _ (Exit st)    = Exit st

instance Applicative Result where
    -- pure :: a -> Result a
    pure = OK
    -- (<*>) :: f (a -> b) -> f a -> f b
    (OK f)       <*> vm = fmap f vm
    (Crash e st) <*> _ = Crash e st
    (Exit st)    <*> _ = Exit st

instance Monad Result where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (OK x)       >>= f = f x
    (Crash e st) >>= _ = Crash e st
    (Exit st)    >>= _ = Exit st
