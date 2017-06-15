module Test.Types.Element where

import Prelude
import App.Types.Element
import App.Types.Keyframe

import Data.Either(Either(..))
import Data.Argonaut (encodeJson, decodeJson)
import Data.Array(sort)
import Data.Foldable(foldl)

import Test.Unit
import Test.Unit.QuickCheck
import Test.Helpers
import Test.QuickCheck(Result(), (<?>))

tests = 
  suite "element" do
    test "JSON Instances" do
      quickCheck jsonMatches
    test "Insert Key" do
      quickCheck framesInsertInOrder

jsonMatches :: Element -> Result
jsonMatches e = ((Right e) == (decodeJson $ encodeJson e)) 
  <?> "Encoded Json didn't appear to match original data"

emptyEl = Element {layer:1, keys: []}
  
framesInsertInOrder :: Array Keyframe -> Result
framesInsertInOrder ks = let ts = map time $ getKeys $ foldl insertKey emptyEl ks
  in ts == (sort ts) <?> "Frames out of order."