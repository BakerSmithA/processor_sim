module VM where

import State

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data VM a = VM a
          | Crash

instance Functor VM where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (VM x)  = VM (f x)
    fmap _ (Crash) = Crash

instance Applicative VM where
    -- pure :: a -> VM a
    pure = VM
    -- (<*>) :: f (a -> b) -> f a -> f b
    (VM f) <*> vm = fmap f vm
    (Crash) <*> _ = Crash

instance Monad VM where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (VM x) >>= f  = f x
    (Crash) >>= _ = Crash
