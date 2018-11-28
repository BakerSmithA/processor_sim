module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack

-- Entry into the Reorder Buffer. Whether the instruction is present determines
-- whether the entry is ready to be committed.
data Entry = Entry (Maybe WriteBack)

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB (Queue Entry)
