module Helper where

import Control.Monad

-- Group array into chunks of n elements, last chunk will be shorter if n does
-- not evenly divide length of xs.
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative n"

-- Returns the first non-maybe value produced.
tryPick :: (MonadPlus m) => (a -> m b) -> [a] -> m b
tryPick f xs = msum (map f xs)
