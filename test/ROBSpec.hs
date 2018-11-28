module ROBSpec where

import Test.Hspec
import ROB

robSpec :: Spec
robSpec = describe "Reorder Buffer" $ do
    it "allocates empty entry" $ do
        pending
