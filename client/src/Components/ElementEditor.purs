module App.Components.ElementEditor where

import App.Prelude
import App.Types.Keyframe
import App.Types.Property
import App.Types.RGB
import App.Types.Point
import App.Helpers.Forms


type State = { frame  :: Maybe Keyframe
             , locked :: Boolean
             , heldFrame :: Maybe Keyframe
             }
             
data Query a 
  = FormChange Int Property a 
  | SetFrame (Maybe Keyframe) (Unit -> a)
  | AddFrame a
  | LockFrame a
  | GetFrame (Maybe Keyframe -> a)
  | IsLocked (Boolean -> a)
  
editorComponent :: forall m. Component HTML Query Unit (Maybe Keyframe) m
editorComponent =
  component
    { initialState: const {locked: false, frame: Nothing, heldFrame: Nothing}
    , render
    , eval
    , receiver: const Nothing
    } where
  
  render :: State -> ComponentHTML Query
  render {frame: Nothing} = div [ id_ "el-editor", class_ $ ClassName "off" ] []
  render {frame: Just fr} 
    = div [id_ "el-editor"] $
      [ h2_ [text "Edit Element"] ]
      <> ( concat $ mapWithIndex renderProp (props fr)) 
      <> [ div_ 
        [ a
          [ onClick $ input_ LockFrame, class_ $ ClassName "a-button" ]
          [ text "Lock" ]
        , a
          [ onClick $ input_ AddFrame, class_ $ ClassName "a-button" ] 
          [ text "Apply"]
        ]
      ]
    
  eval :: forall m. Query ~> ComponentDSL State Query (Maybe Keyframe) m
  eval (FormChange i prop next) = do
    fr <- gets _.frame
    let ps = fromMaybe [] $ props <$> fr
    if isJust $ (recProp const prop) <$> (ps !! i) 
      then do modify $ (\st -> st {frame = map (\(Keyframe f) -> Keyframe $ f {props = fromMaybe ps $ updateAt i prop ps}) st.frame})
      else pure unit
    pure next
  
  eval (AddFrame next) = do
    raise =<< gets _.frame
    pure next
  
  eval (GetFrame reply) = do
    frame <- gets _.frame
    pure (reply frame)
    
  eval (LockFrame next) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st {locked=false,frame=st.heldFrame})
      else modify (_ {locked=true})
    pure next
    
  eval (IsLocked reply) = do
    locked <- gets _.locked
    pure (reply locked)
    
  eval (SetFrame mbFrame reply) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st {heldFrame = mbFrame, frame = setTime <$> (time <$> mbFrame) <*> st.frame})
      else modify (_ {frame = mbFrame})
    pure (reply unit)
  

fieldClass = class_ $ ClassName "editor-field"
renderProp i (Enabled b)   = [ label [fieldClass] 
                               [ div_ [text "Visible"]
                               , checkBox [title "enabled"]  b (FormChange i <<< Enabled)] 
                             ]
renderProp i (Bordered b)  = [ label [fieldClass] 
                               [ div_ [text "Border"]
                               , checkBox [title "bordered"] b (FormChange i <<< Bordered)]
                             ]
renderProp i (Color c)     = [ label [fieldClass] 
                               [ div_ [text "Color"]
                               , color [title "color"] c (FormChange i <<< Color) ]
                             ]
renderProp i (Position p)  = 
  concat $ map (\{v,h,l} -> [ label [fieldClass] 
                              [ div_ [text l ]
                              , number [] v (FormChange i <<< Position <<< h)]
                            ]) 
  [{v:getX p,h:setX p,l:"X"}, {v: getY p, h: setY p,l:"Y"}]
renderProp i (Opacity o)   = [ label [fieldClass] 
                               [ div_ [text "Opacity"]
                               , slider [ title "opacity" ] 0 100 o (FormChange i <<< Opacity) 
                               ]
                             ]
renderProp i (Angle a)     = [ label [fieldClass] 
                               [ div_ [text "Rotation"]
                               , slider [ title "angle"] 0 360 a (FormChange i <<< Angle)
                               ]
                             ]
renderProp i (Circle r)    = [ label [fieldClass] 
                               [ div_ [text "Radius" ]
                               , number [ title "radius" ] r (FormChange i <<< Circle)
                               ]
                             ]
renderProp i (Rect w h)    = [ label [fieldClass] 
                               [ div_ [text "Width"]
                               , number [ title "width" ] w (FormChange i <<< (flip Rect) h) 
                               ]
                             , label [fieldClass] 
                               [ div_ [text "Height"]
                               , number [ title "height" ] h (FormChange i <<< Rect w)
                               ]
                             ]
renderProp i (Donut r1 r2) = [ label [fieldClass] 
                               [ div_ [text "Inner Radius"]
                               , number [ title "inner radius" ] r1 (FormChange i <<< (flip Donut) r2)
                               ]
                             , label [fieldClass] 
                               [ div_ [text "Outer Radius"]
                               , number [ title "outer radius" ] r2 (FormChange i <<< Donut r1)
                               ]
                             ]