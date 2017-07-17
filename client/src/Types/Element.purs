module App.Types.Element where

import App.Prelude

import App.Types.Keyframe
import App.Types.Point

newtype Element = Element { layer :: Int, keys :: Array Keyframe }

getLayer :: Element -> Int
getLayer (Element e) = e.layer

getKeys :: Element -> Array Keyframe
getKeys (Element e) = e.keys

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

instance arbElement :: Arbitrary Element where
  arbitrary = (\layer keys -> Element {layer,keys}) <$> arbitrary <*> arbitrary
  
  
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