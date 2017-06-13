module App.Types.Element where

import Prelude

import App.Validators
import App.Types.Keyframe
import App.Types.Point

import Data.Argonaut

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Array (insertBy, (!!), updateAt, findIndex, findLastIndex, zipWith, length)
import Data.Foldable (and)
import Data.Int (toNumber)
import Data.Ord (comparing)

import Graphics.Canvas.Free

newtype Element = Element { layer :: Int, keys :: Array Keyframe }

getLayer :: Element -> Int
getLayer (Element e) = e.layer

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
    pure $ Element {layer,keys}

instance eqElement :: Eq Element where
  eq (Element a) (Element b) = 
    (a.layer == b.layer) && 
    (and $ zipWith eq a.keys b.keys)

insertKey :: Element -> Keyframe -> Element
insertKey (Element el) k = 
  case findIndex (\a -> time a == time k) el.keys of
    Just i  -> Element (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> Element (el {keys = insertBy (comparing time) k el.keys})

getFrame :: Element -> Int -> Maybe Keyframe
getFrame (Element {keys}) t = 
  case findLastIndex (\f -> (time f) <= t) keys of
    Just i -> if length keys == (i+1) || (fromMaybe false $ eq <$> (time <$> keys !! i) <*> (Just t))
               then (\(Keyframe f) -> Keyframe $ f {time=t}) <$> keys !! i
               else reconcile <$> (keys !! i) <*> (keys !! (i+1)) <*> (Just t)
    Nothing -> Nothing