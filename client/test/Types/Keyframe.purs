module Test.Types.Keyframe where

import Prelude

import App.Types.Keyframe

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck (Result(), (<?>))
import Test.QuickCheck.Gen (suchThat)

import Data.Either(Either(..))
import Data.Argonaut(decodeJson, encodeJson)

tests = 
  suite "Keyframe" do
    test "Json Instances" do
      quickCheck jsonMatches
      
jsonMatches :: Keyframe -> Result
jsonMatches f = ((Right f) == (decodeJson $ encodeJson f)) 
  <?> "Encoded Json didn't appear to match original data"