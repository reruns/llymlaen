module App.Static where

import Graphics.Canvas.Free
import App.Property
import Prelude (Unit, bind, pure)
import Data.Argonaut (class EncodeJson, encodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

type Static = { props :: Array Property }

renderStatic :: Static -> Graphics Unit
renderStatic st = renderCanvas st.props

encodeStatic s = "props" := (encodeJson s.props) ~> jsonEmptyObject

decodeStatic json = do
  obj <- decodeJson json
  props <- obj .? "props"
  pure {props}