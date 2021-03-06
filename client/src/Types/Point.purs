module App.Types.Point where

import App.Prelude

newtype Point = Point { x :: Int, y :: Int }
getX (Point {x}) = x
getY (Point {y}) = y

setX (Point p) v = Point $ p {x=v}
setY (Point p) v = Point $ p {y=v}

vectorSub :: Point -> Point -> Point
vectorSub (Point {x:x1,y:y1}) (Point {x:x2,y:y2}) = Point {x:x1-x2, y:y1-y2}

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
    
instance eqPoint :: Eq Point where
  eq (Point a) (Point b) = a.x == b.x && a.y == b.y
  
instance arbPoint :: Arbitrary Point where
  arbitrary = (\x y -> Point {x,y}) <$> arbitrary <*> arbitrary