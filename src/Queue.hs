module Queue where

import Data.Array (Array)
import qualified Data.Array as Arr
import Debug.Trace

type Start = Int
type End   = Int

data Range = Empty
           | In   Start End
           | Wrap Start End
           deriving (Show, Eq)

-- Queue to which elements can be added to start, and from which elements can
-- be taken from end.
data Queue a = Queue {
    -- Elements in queue. Array as allows O(1) access since start and end
    -- pointers used to access.
    es :: Array Int a
    -- Range of elements that are valid. Required as old elements are left in
    -- the queue.
  , range :: Range
} deriving (Show, Eq)

-- Return whether an index is in the range.
inRange :: Range -> Int -> Bool
inRange = undefined

-- Return queue containing the given elements.
fromList :: [a] -> Queue a
fromList xs = Queue arr Empty where
    arr = Arr.listArray (0, length xs - 1) xs

-- Returns all valid elements of the queue.
elems :: Queue a -> [a]
elems (Queue es r) = foldr f [] (Arr.assocs es) where
    f (i, x) xs = if inRange r i then (x:xs) else xs

-- Allocate space for an element at end of queue, and return index allocated at.
-- The `set` function can then be used to modify the contents of the address.
enq :: a -> Queue a -> (Int, Queue a)
enq = undefined
-- enq x (Queue xs s e) = (s, Queue xs' s' e) where
--     xs' = xs // [(s, x)]
--     s'  = wrapIdx (s - 1) xs

-- Remove element from start of queue.
rem :: Queue a -> Queue a
rem = undefined
-- rem (Queue xs s e) = Queue xs s e' where
--     e' = wrapIdx (e - 1) xs

-- Return element at the start of the queue.
peek :: Queue a -> a
peek = undefined
-- peek (Queue xs _ e) = xs ! e

-- Set the element stored at a given index.
set :: Int -> a -> Queue a -> Queue a
set = undefined
-- set i x q = q { es = es' } where
--     es' = es q // [(i, x)]

-- Return element at index in queue.
get :: Int -> Queue a -> a
get = undefined
-- get i (Queue xs _ _) = xs ! i

-- Searches through the queue from end to start looking for the first element
-- to statisfy the predicate. I.e. finds most recentl added element.
-- Returns Nothing if none statisfy.
findNewest :: (Show a) => (a -> Bool) -> Queue a -> Maybe a
findNewest = undefined
-- findNewest p (Queue es s e) = find' p es is where
--     is = wrapIdxs (wrapIdx s es) e (length es)
--
--     -- Search through given indices in es looking for match.
--     find' _ _  []     = trace ("\tQ: found nothing") $ Nothing
--     find' p es (i:is) | p (es ! i) = trace ("\tQ found: " ++ show i) $ Just (es ! i)
--                       | otherwise  = trace ("\tQ no match: " ++ show i) $ find' p es is
