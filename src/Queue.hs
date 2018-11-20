module Queue where

import Data.Array

-- Queue to which elements can be added to start, and from which elements can
-- be taken from end.
data Queue a = Queue {
    -- Elements in queue. Array as allows O(1) access since start and end
    -- pointers used to access.
    elems :: Array Int a
    -- Points to start of queue, to where elements are added.
  , start  :: Int
    -- Points to end of queue, to where elements are taken from.
  , end    :: Int
}

-- Insert element into queue.
ins :: a -> Queue a -> Queue a
ins x (Queue xs s e) = Queue xs' s' e where
    xs' = xs // [(s, x)]
    s'  = s + 1 `mod` length xs

-- Remove element from queue.
rem :: Queue a -> (a, Queue a)
rem (Queue xs s e) = (x, Queue xs s e') where
    x  = xs ! e
    e' = e - 1 `mod` length xs
