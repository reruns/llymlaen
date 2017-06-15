module Test.Types.Element where

import Prelude
import App.Types.Element

import Data.Either(Either(..))
import Data.Argonaut (encodeJson, decodeJson)

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck(Result(), (<?>))

tests = 
  suite "element" do
    test "JSON Instances" do
      quickCheck jsonMatches
    test "Insert Key" do
      notWritten
    test "Get Frame" do
      notWritten

jsonMatches :: Element -> Result
jsonMatches e = ((Right e) == (decodeJson $ encodeJson e)) 
  <?> "Encoded Json didn't appear to match original data"