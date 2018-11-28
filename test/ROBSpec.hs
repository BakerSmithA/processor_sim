module ROBSpec where

import Test.Hspec
import ROB

robSpec :: Spec
robSpec = describe "Reorder Buffer" $ do
    it "returns all instructions that can be committed" $ do
        pending
