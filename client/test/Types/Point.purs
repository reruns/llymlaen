module Test.Types.Point where

import Prelude

import App.Types.Point

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck(Result(), (<?>))

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either(Either(..))

tests =
  suite "Point" do
    test "JSON Instances" do
      quickCheck jsonMatches
      
jsonMatches :: Point -> Result
jsonMatches c = ((Right c) == (decodeJson $ encodeJson c)) 
  <?> "Encoded Json didn't appear to match original data"