module Test.Types.RGB where

import Prelude

import App.Types.RGB

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck(Result(), (<?>))

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either(Either(..))

tests =
  suite "RGB" do
    test "JSON Instances" do
      quickCheck jsonMatches
      
jsonMatches :: RGB -> Result
jsonMatches c = ((Right c) == (decodeJson $ encodeJson c)) 
  <?> "Encoded Json didn't appear to match original data"