module Test.Types.Diag where

import Prelude

import App.Types.Diag

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck(Result(), (<?>))

import Data.Either(Either(..))
import Data.Argonaut(decodeJson, encodeJson)

tests = 
  suite "Diagram" do
    test "Add an Element" do
      notWritten
    test "JSON Instances" do
      quickCheck jsonMatches
      
jsonMatches :: Diag -> Result
jsonMatches d = ((Right d) == (decodeJson $ encodeJson d)) 
  <?> "Encoded Json didn't appear to match original data"