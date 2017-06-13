module App.Types.Static where

import Prelude
import Graphics.Canvas.Free
import Data.Argonaut

import App.Types.Property
import App.Types.Keyframe

newtype Static = Static { props :: Array Property }

renderStatic :: Static -> Graphics Unit
renderStatic (static st) = renderFrame $ Keyframe {time:-1, props:st.props}
--TODO: this pattern implies that renderFrame should be shared logic, like it was before
--just not in Types.Property
--or, perhaps, this shouldn't be its own type at all?

instance encodeStatic :: EncodeJson Static where
encodeJson (Static s) =
  "props" := s.props ~>
  jsonEmptyObject
  
instance decodeStatic :: DecodeJson Static where
decodeJson json = do
  obj <- decodeJson json
  props <- obj .? "props"
  pure $ Static {props}