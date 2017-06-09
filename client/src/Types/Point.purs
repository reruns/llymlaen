module App.Types.Point

import Prelude

import Data.Argonaut

newtype Point = Point { x :: Int, y :: Int }
--getters
getX (Point {x}) = x
getY (Point {y}) = y

--setters
setX p v = p {x=v}
setY p v = p {y=v}

instance showPoint :: Show Point where
  show (Point {x,y}) = "(" <> (show x) <> "," <> (show y) <> ")"

instance encodePoint :: EncodeJson Point where 
encodeJson (Point {x,y}) = 
  "x" := x ~> 
  "y" := y ~> 
  jsonEmptyObject
  
instance decodePoint :: DecodeJson Point where
decodeJson json = do
  obj <- decodeJson json
  x <- obj .? "x"
  y <- obj .? "y"
  pure $ Point {x,y}