module Test.Types.Keyframe where

import Prelude

import App.Types.Keyframe

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck (Result(), (<?>))

import Data.Either(Either(..))
import Data.Argonaut(decodeJson, encodeJson)

tests = 
  suite "Keyframe" do
    test "Reconciling" do
      notWritten
    test "Overlap" do
      notWritten
    test "Json Instances" do
      quickCheck jsonMatches
      
jsonMatches :: Keyframe -> Result
jsonMatches f = ((Right f) == (decodeJson $ encodeJson f)) 
  <?> "Encoded Json didn't appear to match original data"