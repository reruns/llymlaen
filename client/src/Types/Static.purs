module App.Types.Static where

import Prelude
import Graphics.Canvas.Free
import Data.Argonaut

import App.Types.Property

newtype Static = Static { props :: Array Property }

renderStatic :: Static -> Graphics Unit
renderStatic (static st) = renderCanvas st.props

instance encodeStatic :: EncodeJson Static where
encodeJson (Static s) =
  "props" := s.props ~>
  jsonEmptyObject
  
instance decodeStatic :: DecodeJson Static where
decodeJson json = do
  obj <- decodeJson json
  props <- obj .? "props"
  pure $ Static {props}