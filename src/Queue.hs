module Queue where

import Data.Array (Array, (//), (!))
import qualified Data.Array as Arr
import Data.List as List (find)

type Start = Int
type End   = Int
type Len   = Int

-- Start points to next available space, and end points to last element in array.
data Range = Range Start End Len
           deriving (Show, Eq)

-- Sets a start index for the range.
newStart :: Start -> Range -> Range
newStart s (Range _ e l) = Range s e l

-- Sets a new end index for the range.
newEnd :: Start -> Range -> Range
newEnd e (Range s _ l) = Range s e l

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

-- The total size assigned the queue, including invalid elements.
totalSize :: Queue a -> Int
totalSize (Queue _ (Range _ _ l)) = l

mapQ :: (a -> a) -> Queue a -> Queue a
mapQ f (Queue es r) = Queue es' r where
    es' = foldr modify es (indices r)
    modify i arr = arr // [(i, f (arr ! i))]

-- Return queue containing the given elements.
fromList :: [a] -> Queue a
fromList xs = Queue arr (Range 0 0 (length xs)) where
    arr = Arr.listArray (0, length xs - 1) xs

-- Returns all elements in the queue, valid or invalid.
allElems :: Queue a -> [a]
allElems (Queue es _) = Arr.elems es

-- Takes elements of array from list of indices.
takeIndices :: [Int] -> Array Int a -> [a]
takeIndices is arr = foldr f [] is where
    f i acc = (arr ! i):acc

-- Returns all valid elements of the queue in the range.
elemsRange :: Range -> Queue a -> [a]
elemsRange r (Queue es _) = takeIndices (indices r) es

-- Return all elements after the given index, excluding the index.
elemsAfter :: Int -> Queue a -> [a]
elemsAfter i q = elemsRange (newStart i (range q)) q

-- Return elements in order newest to oldest.
elemsNewOld :: Queue a -> [a]
elemsNewOld (Queue es r) = takeIndices (indices r) es

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

-- Return index and element at the start of the end, without modifying queue.
peek :: (Show a) => Queue a -> Maybe (Int, a)
peek (Queue es r) = fmap (\i -> (i, es ! i)) (Queue.last r)

-- Set the element stored at a given index.
set :: Int -> a -> Queue a -> Queue a
set i x (Queue es r) =
    if inRange i r
        then Queue (es // [(i, x)]) r
        else error ("Out of bound set in Queue at: " ++ show i)

-- Return element at index in queue.
get :: Int -> Queue a -> a
get i (Queue es r) =
    if inRange i r
        then es ! i
        else error ("Out of bounds get in Queue at: " ++ show i)

-- Specifies direction and range of search.
data Search
    -- Search from new to old elements.
    = NewToOld
    -- Search from old to new elements.
    | OldToNew
    -- Search from old to new, but with a different starting search point, non-inclusive.
    | SubNewToOld Int

-- Searches for the first element logically newer than the element at the given
-- index, which also satisfies the predicate.
find :: (Show a) => Search -> (a -> Bool) -> Queue a -> Maybe a
find (NewToOld)      cond q = List.find cond (elemsNewOld q)
find (OldToNew)      cond q = List.find cond (reverse $ elemsNewOld q)
find (SubNewToOld s) cond q =
    if inRange s (range q)
        then List.find cond (elemsRange (newStart s (range q)) q)
        else error ("Out of bounds search sub new to old in Queue at: " ++ show s ++ ", range: " ++ show (range q))
