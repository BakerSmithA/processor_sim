module Queue where

import Data.Array as Arr

-- Queue to which elements can be added to start, and from which elements can
-- be taken from end.
data Queue a = Queue {
    -- Elements in queue. Array as allows O(1) access since start and end
    -- pointers used to access.
    _elems :: Array Int a
    -- Points to start of queue, to where elements are added.
  , start  :: Int
    -- Points to end of queue, to where elements are taken from.
  , end    :: Int
}

fromList :: [a] -> Queue a
fromList xs = Queue arr s e where
    arr = listArray (0, length xs - 1) xs
    s = 0
    e = (length xs) - 1

elems :: Queue a -> [a]
elems = Arr.elems . _elems

-- Insert element into queue.
ins :: a -> Queue a -> Queue a
ins x (Queue xs s e) = Queue xs' s' e where
    xs' = xs // [(s, x)]
    s'  = wrapIdx (s - 1) xs

-- Remove element from queue.
rem :: Queue a -> (a, Queue a)
rem (Queue xs s e) = (x, Queue xs s e') where
    x  = xs ! e
    e' = wrapIdx (e - 1) xs

wrapIdx :: Int -> Array Int a -> Int
wrapIdx i xs = i `mod` (length xs) - 1