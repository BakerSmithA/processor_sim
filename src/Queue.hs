module Queue where

import Data.Array as Arr

-- Queue to which elements can be added to start, and from which elements can
-- be taken from end.
data Queue a = Queue {
    -- Elements in queue. Array as allows O(1) access since start and end
    -- pointers used to access.
    _elems :: Array Int a
    -- Points to start of queue, to where the next element will be allocated.
  , start  :: Int
    -- Points to end of queue, to where elements are taken from.
  , end    :: Int
} deriving (Show)

-- Return queue containing the given elements.
fromList :: [a] -> Queue a
fromList xs = Queue arr s e where
    arr = listArray (0, length xs - 1) xs
    s = 0
    e = 0

-- Returns all elements of the queue.
elems :: Queue a -> [a]
elems = Arr.elems . _elems

-- Allocate space for an element at end of queue, and return index allocated at.
-- The `set` function can then be used to modify the contents of the address.
enq :: a -> Queue a -> (Int, Queue a)
enq x (Queue xs s e) = (s, Queue xs' s' e) where
    xs' = xs // [(s, x)]
    s'  = wrapIdx (s - 1) xs

-- Remove element from start of queue.
rem :: Queue a -> Queue a
rem (Queue xs s e) = Queue xs s e' where
    e' = wrapIdx (e - 1) xs

-- Return element at the start of the queue.
peek :: Queue a -> a
peek (Queue xs _ e) = xs ! e

-- Set the element stored at a given index.
set :: Int -> a -> Queue a -> Queue a
set i x q = q { _elems = es } where
    es = _elems q // [(i, x)]

-- Return element at index in queue.
get :: Int -> Queue a -> a
get i (Queue xs _ _) = xs ! i

-- Searches through the queue from end to start looking for the first element
-- to statisfy the predicate. I.e. finds most recentl added element.
-- Returns Nothing if none statisfy.
findNewest :: (Show a) => (a -> Bool) -> Queue a -> Maybe a
findNewest p (Queue es s e) = find' p es is where
    is = wrapIdxs (wrapIdx (s+1) es) e (length es)

    -- Search through given indices in es looking for match.
    find' _ _  []     = Nothing
    find' p es (i:is) | p (es ! i) = Just (es ! i)
                      | otherwise  = find' p es is

-- Stops index going out of bounds of array.
wrapIdx :: Int -> Array Int a -> Int
wrapIdx i xs = i `mod` length xs

-- Returns a list of indexes from start to end (not including end), wrapped to
-- be in the given range.
wrapIdxs :: Int -> Int -> Int -> [Int]
wrapIdxs s e len = fmap (\x -> x `mod` len) [s..(s+num)] where
    num = if s > e then len - s + e else s - e
