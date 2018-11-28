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

-- Return queue containing the given elements.
fromList :: [a] -> Queue a
fromList xs = Queue arr s e where
    arr = listArray (0, length xs - 1) xs
    s = 0
    e = 0

-- Returns all elements of the queue.
elems :: Queue a -> [a]
elems = Arr.elems . _elems

-- Allocate space for an element at tail of queue, and return index allocated at.
-- The `set` function can then be used to modify the contents of the address.
alloc :: Queue a -> (Int, Queue a)
alloc (Queue xs s e) = (s, Queue xs s' e) where
    s' = wrapIdx (s - 1) xs

-- Remove element from head of queue.
rem :: Queue a -> (a, Queue a)
rem (Queue xs s e) = (x, Queue xs s e') where
    x  = xs ! e
    e' = wrapIdx (e - 1) xs

-- Set the element stored at a given index.
set :: Int -> a -> Queue a -> Queue a
set i x q = q { _elems = es } where
    es = _elems q // [(i, x)]

-- Return element at index in queue.
get :: Int -> Queue a -> a
get i (Queue xs _ _) = xs ! i

-- Stops index going out of bounds of array.
wrapIdx :: Int -> Array Int a -> Int
wrapIdx i xs = i `mod` length xs
