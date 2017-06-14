module App.Types.Diag where

import Prelude

import App.Types.RGB
import App.Types.Element

import Data.Array (insertBy)

import Data.Argonaut

newtype Diag = Diag { color :: RGB
                    , elements :: Array Element
                    }
                    
getElements :: Diag -> Array Element
getElements (Diag d) = d.elements

getColor :: Diag -> RGB
getColor (Diag d) = d.color

setElements :: Diag -> Array Element -> Diag
setElements (Diag d) es = Diag $ d {elements = es}
                    
instance encodeDiag :: EncodeJson Diag where
  encodeJson (Diag {color,elements}) = 
    "Color"    := color ~> 
    "Elements" := elements ~> 
    jsonEmptyObject
    
instance decodeDiag :: DecodeJson Diag where
  decodeJson json = do
    obj <- decodeJson json
    color <- obj .? "Color"
    elements <- obj .? "Elements"
    pure $ Diag {color,elements}
  
  
addElement :: Diag -> Element -> Diag
addElement (Diag d) el = Diag (d {elements = insertBy (comparing getLayer) el d.elements})