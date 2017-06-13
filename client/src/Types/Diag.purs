module App.Types.Diag

import App.Types.Static
import App.Types.RGB
import App.Types.Element

newtype Diag = Diag { color :: RGB
                    , statics :: Array Static
                    , elements :: Array Element
                    }
                    
getElements :: Diag -> Array Element
getElements (Diag d) = d.elements

setElements :: Diag -> Array Element -> Diag
setElements (Diag d) es = Diag $ d {elements = es}
                    
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
  
  
addElement :: Diag -> Element -> Diag
addElement (Diag d) el = 
  Diag (d {elements = insertBy (comparing _.layer) el elements})