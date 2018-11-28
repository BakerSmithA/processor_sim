module Pipeline where

import Instr
import WriteBack

-- 4 stage pipeline, allowing for 4 instructions (at different stages) to be
-- processed at the same time: fetching, decoding, executing, and write-back.
data Pipeline = Pipeline {
    fetched  :: Maybe Instr
  , decoded  :: Maybe Instr
  , executed :: Maybe WriteBack
} deriving (Show)

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing

type Decoder  m = Instr -> m Instr
type Executer m = Instr -> m WriteBack
type Writer m a = WriteBack -> m a

-- Helper for advance.
op :: (Monad m) => (a -> m b) -> Maybe a ->  m (Maybe b)
op f = maybe (return Nothing) (fmap Just . f)

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Maybe Instr -> Decoder m -> Executer m -> Writer m a -> Pipeline -> m (Maybe a, Pipeline)
advance fetch decode exec write p = do
    d <- op decode (fetched p)
    e <- op exec   (decoded p)
    w <- op write  (executed p)
    return (w, Pipeline fetch d e)
