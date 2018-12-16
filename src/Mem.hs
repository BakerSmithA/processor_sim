module Mem where

import Helper
import Data.Array
import Data.List (intercalate)

data Mem k v = Mem { arr :: Array k v, maxAddr :: k }
             | Empty
           deriving (Eq, Show)

-- Return string where each memory address is numbered.
showNumbered :: (Show k, Show v, Ix k) => Mem k (Maybe v) -> String
showNumbered (Empty) = "Empty"
showNumbered (Mem arr _) = intercalate ", " numbered where
    numbered = fmap (\(i, x) -> show (i :: Integer) ++ ":" ++ s x) (zip [0..] (elems arr))
    s = maybe "_" show

-- Return string where memory is displayed in blocks.
showBlocks :: (Show v) => Int -> Mem k v -> String
showBlocks _         (Empty)     = "Empty"
showBlocks lineWidth (Mem arr _) = unlines (map showLine (group lineWidth (elems arr))) where
    showLine line = concatMap ((++"\t") . show) line

-- Return memory containing values in list.
fromList :: (Ix k, Num k, Enum k, Integral k) => [v] -> Mem k v
fromList [] = Empty
fromList xs = Mem { arr = array (0, len) (zip [0..] xs), maxAddr = len } where
    len = fromIntegral $ (length xs) - 1

-- Return memory containing all zeros.
zeroed :: (Ix k, Num k, Enum k, Num v, Integral k) => k -> Mem k v
zeroed maxAddr = fromList (Prelude.take n (repeat 0)) where
    n = fromIntegral maxAddr + 1

-- Checks index is within range 0 to maxAddr, inclusive. If so the operation
-- is performed. Otherwise Nothing is returned.
checkedAddr :: (Num k, Ix k) => (k -> Mem k v -> a) -> k -> Mem k v -> Maybe a
checkedAddr op i m@(Mem _ maxAddr) =
    if i > maxAddr || i < 0
        then Nothing
        else Just (op i m)
checkedAddr _ _ (Empty) = Nothing

-- Returns value from memory, or Nothing if index is invalid.
load :: (Num k, Ix k) => k -> Mem k v -> Maybe v
load = checkedAddr (\i mem -> (arr mem) ! i)

-- Returns new state of memory after storing value, or nothing if index is invalid.
store :: (Num k, Ix k) => k -> v -> Mem k v -> Maybe (Mem k v)
store i val = checkedAddr f i where
    f i' mem = mem { arr = (arr mem) // [(i', val)] }

-- Take a number of elements starting at the given index.
-- If goes past end of memory, then only up to end is returned.
take :: (Num k, Ix k) => k -> k -> Mem k v -> [v]
take num start (Mem arr maxAddr) = take' start where
    take' i | i <= maxAddr && i < (num+start) = (arr ! i):(take' (i+1))
            | otherwise = []
