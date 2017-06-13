module App.Types.Keyframe where

import Prelude
import App.Types.Property

import Data.Int (toNumber)

import Data.Argonaut

newtype Keyframe = Keyframe { time :: Int, props :: Array Property }

time :: Keyframe -> Int
time (Keyframe k) = k.time

props :: Keyframe -> Array Property
props (Keyframe k) = k.props

instance encodeFrame :: EncodeJson Keyframe where
encodeJson (Keyframe f)
  =  "time"  := f.time
  ~> "props" := f.props
  ~> jsonEmptyObject
  
instance decodeFrame :: DecodeJson Keyframe where
decodeJson json = do
  obj <- decodeJson json
  time <- obj .? "time"
  props <- obj .? "props"
  pure $ Keyframe {time,props}
  
instance eqFrame :: Eq Keyframe where
(==) (Keyframe a) (Keyframe b) = 
  ( a.time == b.time ) && ( a.props == b.props ) 
  
--consider name change: interpolate?
reconcile :: Keyframe -> Keyframe -> Int -> Keyframe
reconcile (Keyframe {time:tl, props: left}) (Keyframe {time: tr, props: right}) t =
  let p = (toNumber (t-tl)) / (toNumber (tr-tl))
      f a b = a + (round $ p * (toNumber (b-a)))  
  in Keyframe { time: t
     , props: fromMaybe left $ sequence $ zipWith (recProp f) left right}

     
renderFrame :: Keyframe -> Graphics Unit
renderFrame (Keyframe {props}) = 
  do save 
    let gfx = sequence $ map renderProp props
    case gfx of
      Nothing -> pure unit
      Just es -> sequence_ es
    restore where