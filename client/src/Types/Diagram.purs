import App.Types.Static
import App.Types.RGB
import App.Types.Element

newtype Diag = Diag { color :: RGB
                    , statics :: Array Static
                    , elements :: Array ( Array Element )
                    }
                    
--TODO
encodeState :: State -> Json
encodeState {color,statics,elements}
  =  "Color"    := (encodeRGB color)
  ~> "Statics"  := map S.encodeStatic statics
  ~> "Elements" := map E.encodeLayer elements
  ~> jsonEmptyObject
  
decodeState :: Json -> Either String State
decodeState json = do
  obj <- decodeJson json
  color <- do
            rgb <- obj .? "Color"
            r <- rgb .? "r"
            g <- rgb .? "g"
            b <- rgb .? "b"
            pure $ {r,g,b}
  statics' <- obj .? "Statics"
  statics <- sequence $ map S.decodeStatic statics'
  elements' <- obj .? "Elements"
  elements <- sequence $ map E.decodeLayer elements'
  pure $ defaultState {color=color, statics=statics, elements=elements}