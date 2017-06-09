module App.Types.Element where

import Prelude

import App.Validators
import App.Types.Keyframe

import Data.Argonaut (Json, encodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Array (insertBy, (!!), updateAt, findIndex, concat, zipWith, mapWithIndex)
import Data.Traversable (sequence, sequence_)
import Data.Foldable (foldl, and)
import Data.Int (toNumber, round, toStringAs, hexadecimal)
import Data.String (joinWith)
import Data.Either(Either(..))
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

    

--To be reworked    
renderEl :: Element -> Graphics Unit
renderEl (Element el) = renderCanvas el.current.props

advanceFrame :: Element -> Element
advanceFrame el = setTime el (el.current.time + 1)

setTime :: Element -> Int -> Element
setTime el t =
  let ms = findMoment el.keys t
      l' = fromMaybe (el.current) ms.l
      r' = fromMaybe (el.current) ms.r in
  case ((==) t) <$> _.time <$> ms.r of
    Nothing    -> el {current = l' {time=t}}
    Just false -> el {current = reconcile l' r' t}
    Just true  -> el {current = r'}