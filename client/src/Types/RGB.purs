module App.Types.RGB where

import Prelude

import Data.Int (toStringAs, hexadecimal)
import Data.String (joinWith)

import Data.Argonaut

newtype RGB = RGB { r :: Int, g :: Int, b :: Int }

instance showRgb :: Show RGB where
show (RGB {r,g,b}) = 
  "#" <> 
  (joinWith "" $ map (\x -> (if x < 16 then "0" else "") 
  <> (toStringAs hexadecimal x)) [r,g,b])

encodeRGB {r,g,b}
  =  "r" := encodeJson r
  ~> "g" := encodeJson g
  ~> "b" := encodeJson b
  ~> jsonEmptyObject


--same as point
setR c v = c {r=v}
setG c v = c {g=v}
setB c v = c {b=v}