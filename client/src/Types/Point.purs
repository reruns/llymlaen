module App.Types.Point

import Prelude

import Data.Argonaut

newtype Point = Point { x :: Int, y :: Int }

instance showPoint :: Show Point where
  show (Point {x,y}) = "(" <> (show x) <> "," <> (show y) <> ")"


instance encodePoint :: EncodeJson Point where 
encodeJson (Point {x,y}) = 
  "x" := encodeJson x ~> 
  "y" := encodeJson y ~> 
  jsonEmptyObject
  
instance decodePoint :: DecodeJson Point where
decodeJson json = do
  obj <- decodeJson json
  x <- obj .? "x"
  y <- obj .? "y"
  pure $ Point {x,y}
  
  
--TODO: Where did we use this?
setX p v = p {x=v}
setY p v = p {y=v}