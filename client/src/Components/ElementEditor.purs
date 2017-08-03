module App.Components.ElementEditor where

import App.Prelude
import App.Types
import App.Helpers.Forms

type LFrame = { layer :: Int
              , kframe :: Keyframe
              }
              
setLFTime :: Int -> LFrame -> LFrame
setLFTime t lf = lf {kframe = setTime t lf.kframe}

type State = { frame  :: Maybe LFrame
             , locked :: Boolean
             , heldFrame :: Maybe LFrame
             }
             
initState = { frame: Nothing 
            , locked: false 
            , heldFrame: Nothing
            }
            
data Query a 
  = FormChange Int Property a 
  | UpdateLayer Int a
  | SetFrame (Maybe LFrame) (Unit -> a)
  | AddFrame a
  | LockFrame a
  | GetFrame (Maybe Keyframe -> a)
  | IsLocked (Boolean -> a)
  | ShiftFrame (Maybe Point) (Unit -> a)
  
editorComponent :: forall m. Component HTML Query Unit (Maybe LFrame) m
editorComponent =
  component
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    } where
  
  render :: State -> ComponentHTML Query
  render {frame: Nothing} = div [ id_ "el-editor", class_ $ ClassName "off" ] []
  render {frame: Just {layer,kframe:fr}, locked} 
    = div [id_ "el-editor"] $
      [ h2_ [text "Edit Element"] ]
      <> [ label [fieldClass] 
           [ div_ [text "Layer"]
           , number [ title "Layer" ] layer UpdateLayer
           ]
         ]
      <> ( concat $ mapWithIndex renderProp (props fr)) 
      <> [ div_ 
        [ a
          [ onClick $ input_ LockFrame, classes $ map ClassName $ if locked then ["a-button","active"] else ["a-button"] ]
          [ text "Lock" ]
        , a
          [ onClick $ input_ AddFrame, class_ $ ClassName "a-button" ] 
          [ text "Apply"]
        ]
      ]
    
  eval :: forall m. Query ~> ComponentDSL State Query (Maybe LFrame) m
  eval (FormChange i prop next) = do
    lframe <- gets _.frame
    case lframe of
      Nothing -> pure unit
      Just {layer,kframe} -> do
        let ps = props kframe
        modify $ _ { frame = Just
          { layer
          , kframe: setProps (fromMaybe ps $ updateAt i prop ps) kframe
          }
        }
    pure next
  
  eval (UpdateLayer l next) = do
    modify (\st -> st {frame = (_ {layer=l}) <$> st.frame })
    pure next
  
  eval (AddFrame next) = do
    raise =<< gets _.frame
    pure next
  
  eval (GetFrame reply) = do
    frame <- gets _.frame
    pure (reply (_.kframe <$> frame))
    
  eval (LockFrame next) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st { locked=false
                             , frame = if isJust st.heldFrame
                                       then st.heldFrame
                                       else st.frame
                             })
      else modify (_ {locked=true})
    pure next
    
  eval (IsLocked reply) = do
    locked <- gets _.locked
    pure (reply locked)
    
  eval (SetFrame mbFrame reply) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st 
        { heldFrame = mbFrame
        , frame = setLFTime <$> (time <$> _.kframe <$> st.frame) <*> mbFrame
        })
      else modify (_ {frame = mbFrame})
    pure (reply unit)
    
  eval (ShiftFrame mp reply) = do
    modify (\st -> st {frame = (\{layer,kframe} -> 
      { layer
      , kframe: shiftPosition (fromMaybe (Point {x:0,y:0}) mp) kframe 
      }) <$> st.frame})
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