module App.Types.Diag

import App.Types.Static
import App.Types.RGB
import App.Types.Element

newtype Diag = Diag { color :: RGB
                    , statics :: Array Static
                    , elements :: Array ( Array Element )
                    }
                    
instance encodeDiag :: EncodeJson Diag where
encodeJson (Diag {color,statics,elements}) = 
  "Color"    := color ~> 
  "Statics"  := statics ~> 
  "Elements" := elements ~> 
  jsonEmptyObject
  
instance decodeDiag :: DecodeJson Diag where
decodeJson json = do
  obj <- decodeJson json
  color <- obj .? "color"
  statics <- obj .? "Statics"
  elements <- obj .? "Elements"
  pure $ Diag {color,statics,elements}