module App.Element where

import Prelude

import App.Validators
import App.Property

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

import Halogen (Action, action)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Element = { current :: Moment, layer :: Int, keys :: Array Moment } 
  
encodeElement :: Element -> Json
encodeElement el
  =  "layer" := el.layer
  ~> "keys"  := (map encodeMoment el.keys)
  ~> jsonEmptyObject
  
decodeElement :: Json -> Either String Element
decodeElement json = do
  obj <- decodeJson json
  layer <- obj .? "layer"
  keys'  <- (obj .? "keys")
  keys <- sequence $ map decodeMoment keys'
  let current = {time: -1, props: []}
  pure {current,layer,keys}
  
encodeLayer :: Array Element -> Json
encodeLayer layer = encodeJson $ map encodeElement layer

decodeLayer :: Json -> Either String (Array Element)
decodeLayer json = do
  arr <- decodeJson json
  sequence $ map decodeElement arr
  

--a substitute for being able to have an Eq instance.
--if doing this all the time ends up being slow we could revisit it, though.
matchEl :: Element -> Element -> Boolean
matchEl a b =  ( matchMoment a.current b.current )
            && ( a.layer == b.layer )
            && ( and $ zipWith matchMoment a.keys b.keys ) 
              
renderEl :: Element -> Graphics Unit
renderEl el = renderCanvas el.current.props
 
--note: this will compile but not work correctly if Position comes after the Shape property
--TODO: factor f into a function in Types.Property
overlap :: Element -> Point -> Boolean
overlap {current:{props}} p = _.b $ foldl f {d:p, a: 0.0 , b:false} props where
  f s@{d}     (Position {x,y}) = s { d= {x: p.x - x , y: p.y - y} }
  f res       (Angle a')       = res { a = -2.0 * pi * (toNumber a') / 360.0 }
  f s@{d,b}   (Circle r)       = s { b = b || ((sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)) <= (toNumber r))}
  f s@{d,a,b} (Rect w h)       = let rot = { x: (toNumber d.x) * (cos a) - (toNumber d.y) * (sin a)
                                       , y: (toNumber d.y) * (cos a) + (toNumber d.x) * (sin a) }
                                 in s { b = b || (rot.x >= 0.0 && rot.x <= (toNumber w) && rot.y >= 0.0 && rot.y <= (toNumber h)) }
  f s@{d,b}   (Donut r1 r2)    = let dist = sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)
                                 in s { b = b || (dist >= (toNumber r1) && dist <= (toNumber r2))}
  f res _                      = res

insertKey :: Element -> Keyframe -> Element
insertKey el k = 
  case findIndex (\a -> time a == time k) el.keys of
    Just i  -> (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> (el {keys = insertBy (comparing time) k el.keys})

    
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
           
--functions for Form elements
checkBox props b h = 
  HH.input ([ HP.type_ HP.InputCheckbox
            , HP.checked b
            , HE.onChecked $ HE.input h
            ] <> props)
          
slider props min max v h =
  HH.input ([ HP.type_ HP.InputRange
            , HP.value (show v)
            , HP.prop (HH.PropName "min") min
            , HP.prop (HH.PropName "max") max
            , HE.onValueChange (\s -> (action <<< h) <$> (validateRange s min max))
            ] <> props) 
          
number props v h =
  HH.input ([ HP.prop (HH.PropName "InputType") HP.InputNumber
            , HP.value (show v)
            , HE.onValueChange (\s -> (action <<< h) <$> (validateNonNeg s))
            ] <> props)