module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB Queue
