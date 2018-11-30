module Queue where

import Data.Array (Array, (//), (!))
import qualified Data.Array as Arr
import Data.List (find)

type Start = Int
type End   = Int
type Len   = Int

-- Start points to next available space, and end points to last element in array.
data Range = Range Start End Len
           deriving (Show, Eq)

-- Returns index of last element in queue, or Nothing if the queue is empty.
last :: Range -> Maybe Int
last r@(Range _ e _) | not (isEmpty r) = Just e
                     | otherwise       = Nothing

-- Decrements the start pointer, thus allowing space for a new element to be
-- added. Returns the old start pointer, which can now be used to insert
-- and element to.
decStart :: Range -> (Int, Range)
decStart (Range s e len) = (s, Range s' e len) where
    s' = (s-1) `mod` len

-- Decrements the start pointer, thus removing an element from the end of the
-- queue.
decEnd :: Range -> Range
decEnd (Range s e len) = Range s e' len where
    e' = (e-1) `mod` len

-- Returns whether an index is in the range.
inRange :: Int -> Range -> Bool
inRange i (Range s e l) | s == e = False
                        | s <  e = s < i && i <= e
                        | s >  e = (0 <= i && i <= e) || (s < i && i < l)
                        | otherwise = error "Shouldn't be here"

isEmpty :: Range -> Bool
isEmpty (Range s e _) = s == e

-- Return all indices in range, not including start.
indices :: Range -> [Int]
indices (Range s e l) | s == e = []
                      | s <  e = [(s+1)..e]
                      | s >  e = [(s+1)..(l-1)] ++ [0..e]

-- Queue to which elements can be ADDED to START, and from which elements can
-- be TAKEN from the END.
data Queue a = Queue {
    -- Elements in queue. Array as allows O(1) access since start and end
    -- pointers used to access.
    es :: Array Int a
    -- Range of elements that are valid. Required as old elements are left in
    -- the queue.
  , range :: Range
} deriving (Show, Eq)

-- Return queue containing the given elements.
fromList :: [a] -> Queue a
fromList xs = Queue arr (Range 0 0 (length xs)) where
    arr = Arr.listArray (0, length xs - 1) xs

-- Returns all elements in the queue, valid or invalid.
allElems :: Queue a -> [a]
allElems (Queue es _) = Arr.elems es

-- Returns all valid elements of the queue, in order from start to end.
-- I.e. newest to oldest.
elems :: Queue a -> [a]
elems (Queue es r) = foldr f [] (indices r) where
    f i acc = (es ! i):acc

-- Place an element at the start of the queue.
enq :: a -> Queue a -> (Int, Queue a)
enq x (Queue es r) = (i, Queue es' r') where
    es'     = es // [(i, x)]
    (i, r') = decStart r

-- Remove element from end of queue and replace with given, or perform no
-- action if empty.
rem :: Queue a -> a -> Queue a
rem q@(Queue es r) x = maybe q f (Queue.last r) where
    f i = Queue es' (decEnd r) where
        es' = es // [(i, x)]

-- Return element at the start of the end, without modifying queue.
peek :: (Show a) => Queue a -> Maybe a
peek (Queue es r) = fmap (\i -> es ! i) (Queue.last r)

-- Set the element stored at a given index.
set :: Int -> a -> Queue a -> Queue a
set i x (Queue es r) =
    if inRange i r
        then Queue (es // [(i, x)]) r
        else error "Out of bound set in Queue"

-- Return element at index in queue.
get :: Int -> Queue a -> a
get i (Queue es r) =
    if inRange i r
        then es ! i
        else error "Out of bounds get in Queue"

-- Searches for most recent element (i.e. closest to start) that statisfies predicate.
findNewest :: (Show a) => (a -> Bool) -> Queue a -> Maybe a
findNewest cond q = find cond (elems q)
