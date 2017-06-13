module App.Types.Diag

import App.Types.Static
import App.Types.RGB
import App.Types.Element

newtype Diag = Diag { color :: RGB
                    , statics :: Array Static
                    , elements :: Array Element
                    }
         
type Location = {layer :: Int, idx :: Int}
                    
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
addElement (Diag d) (Element e) = 
  Diag (d { elements = fromMaybe st.elements $ 
                       (\l -> updateAt e.layer l d.elements) =<< 
                       ( (\l -> snoc l e) <$> (d.elements !! e.layer) ) } ))

resolveTarget Diag -> Point -> Location
resolveTarget (Diag {elements}) (Point {x,y}) = 
  fromMaybe {layer:-1, idx:-1} $ last =<< (sequence $ filter isJust $ 
  mapWithIndex (\i l -> map (\v ->{layer:i, idx:v}) (findLastIndex (\el -> overlap el pos) l)) elements)