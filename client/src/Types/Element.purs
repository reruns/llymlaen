module App.Types.Element where

import Prelude

import App.Validators
import App.Types.Keyframe

import Data.Argonaut

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Array (insertBy, (!!), updateAt, findIndex, zipWith)
import Data.Foldable (foldl, and)
import Data.Int (toNumber)
import Data.Ord (comparing)
import Math (pi, sqrt, pow, sin, cos)

import Graphics.Canvas.Free

newtype Element = Element { current :: Keyframe, layer :: Int, keys :: Array Keyframe }

instance encodeElement :: EncodeJson Element where
encodeJson (Element el) =
  "layer" := el.layer ~> 
  "keys"  := el.keys ~> 
  jsonEmptyObject
  
instance decodeElement :: DecodeJson Element where
decodeJson json = do
  obj <- decodeJson json
  layer <- obj .? "layer"
  keys  <- obj .? "keys"
  let current = {time: -1, props: []}
  pure $ Element {current,layer,keys}

instance eqElement :: Eq Element where
(==) (Element a) (Element b) = 
  a.current == b.current &&
  a.layer == b.layer     &&
  (and zipWith (==) a.keys b.keys)
 
--note: this will compile but not work correctly if Position comes after the Shape property
overlap :: Element -> Point -> Boolean
overlap {current:{props}} (Point p) = _.b $ foldl f {d:p, a: 0.0 , b:false} props where
  f s@{d}     (Position (Point {x,y})) = 
    s { d= {x: p.x - x , y: p.y - y} }
  f res       (Angle a')       = res { a = -2.0 * pi * (toNumber a') / 360.0 }
  f s@{d,b}   (Circle r)       = s { b = b || ((sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)) <= (toNumber r))}
  f s@{d,a,b} (Rect w h)       = let rot = { x: (toNumber d.x) * (cos a) - (toNumber d.y) * (sin a)
                                       , y: (toNumber d.y) * (cos a) + (toNumber d.x) * (sin a) }
                                 in s { b = b || (rot.x >= 0.0 && rot.x <= (toNumber w) && rot.y >= 0.0 && rot.y <= (toNumber h)) }
  f s@{d,b}   (Donut r1 r2)    = let dist = sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)
                                 in s { b = b || (dist >= (toNumber r1) && dist <= (toNumber r2))}
  f res _                      = res

insertKey :: Element -> Keyframe -> Element
insertKey (Element el) k = 
  case findIndex (\a -> time a == time k) el.keys of
    Just i  -> Element (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> Element (el {keys = insertBy (comparing time) k el.keys})

getFrame :: Element -> Int -> Maybe Keyframe
getFrame (Element {keys}) t = 
  case findLastIndex (\f -> (time f) <= t) keys of
    Just t' -> if t' == t
               then keys !! t'
               else reconcile <$> (keys !! t') <*> (keys !! (t'+1))
    Nothing -> Nothing