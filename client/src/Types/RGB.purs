module App.Types.RGB where

import Prelude

import Data.Int (toStringAs, hexadecimal)
import Data.String (joinWith)

import Data.Argonaut

newtype RGB = RGB { r :: Int, g :: Int, b :: Int }
--getters
getR (RGB {r}) = r
getG (RGB {g}) = g
getB (RGB {b}) = b

--setters
setR (RGB c) v = RGB $ c {r=v}
setG (RGB c) v = RGB $ c {g=v}
setB (RGB c) v = RGB $ c {b=v}

instance showRgb :: Show RGB where
show (RGB {r,g,b}) = 
  "#" <> 
  (joinWith "" $ map (\x -> (if x < 16 then "0" else "") 
  <> (toStringAs hexadecimal x)) [r,g,b])

instance encodeRGB :: EncodeJson RGB where
encodeJson (RGB {r,g,b}) = 
  "r" := r ~> 
  "g" := g ~>
  "b" := b ~> 
  jsonEmptyObject
  
instance decodeRGB :: DecodeJson RGB where
decodeJson json = do
  obj <- decodeJson json
  r <- obj .? "r"
  g <- obj .? "g"
  b <- obj .? "b"
  pure $ RGB {r,g,b}