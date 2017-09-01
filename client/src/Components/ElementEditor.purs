module App.Components.ElementEditor where

--the obvious thing to do is to just make this thing hold on to the element object
import App.Prelude
import App.Types hiding (renderProp)
import App.Helpers.Forms

type State = { target :: Maybe Element
             , curFrame :: Maybe Keyframe
             , locked :: Boolean
             , layer  :: Int
             , heldTime :: Int
             }
             
initState = { target: Nothing 
            , curFrame: Nothing
            , locked: false
            , layer: -1
            , heldTime: 0
            }
            
data Query a 
  = FormChange Int Property a 
  | UpdateLayer Int a
  | SetTime Int (Unit -> a)
  | SetTarget (Maybe Element) (Unit -> a)
  | Apply a
  | LockFrame a
  | GetFrame (Maybe Keyframe -> a)
  | IsLocked (Boolean -> a)
  | ShiftFrame (Maybe Point) (Unit -> a)
  
data Message 
  = UpdateEl Element
  | RefreshEl
  -- | SetTime
  
  
editorComponent :: forall m. Component HTML Query Unit Message m
editorComponent =
  component
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    } where
  
  render :: State -> ComponentHTML Query
  render {target: Nothing} = div [ id_ "el-editor", class_ $ ClassName "off" ] []
  render {target, curFrame, locked, layer} =
    let fr = fromMaybe blankFrame curFrame  in
    div [id_ "el-editor"] $
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
          [ onClick $ input_ Apply, class_ $ ClassName "a-button" ] 
          [ text "Apply"]
        ]
      ]
    
  eval :: forall m. Query ~> ComponentDSL State Query Message m
  eval (FormChange i prop next) = do
    mbframe <- gets _.curFrame
    case mbframe of
      Nothing -> pure unit
      Just kframe -> do
        let ps = props kframe
        modify $ _ 
          { curFrame = Just $ setProps (fromMaybe ps $ updateAt i prop ps) kframe
          }
    raise RefreshEl
    pure next
  
  eval (UpdateLayer l next) = do
    modify _ {layer = l}
    pure next
  
  eval (Apply next) = do
    st <- get
    case setLayer st.layer <$> (insertKey <$> st.target <*> st.curFrame) of
      Nothing -> pure unit
      Just el -> raise (UpdateEl el)
    pure next
  
  eval (GetFrame reply) = do
    frame <- gets _.curFrame
    pure (reply frame)
    
  eval (LockFrame next) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st 
        { locked=false
        , curFrame = if st.heldTime /= -1
                     then fromMaybe Nothing $ (flip getFrame) st.heldTime <$> st.target
                     else st.curFrame
        }
      )
      else modify (_ 
        { locked = true
        , heldTime = -1
        }
      )
    pure next
    
  eval (IsLocked reply) = do
    locked <- gets _.locked
    pure (reply locked)
    
  eval (SetTime t reply) = do
    locked <- gets _.locked
    if locked
      then modify (\st -> st 
        { heldTime = t
        , curFrame = setTime t <$> st.curFrame
        })
      else modify (\st -> st {curFrame = fromMaybe Nothing $ getFrame <$> st.target <*> (Just t)})
    pure (reply unit)
    
  eval (SetTarget mel reply) = do
    locked <- gets _.locked
    if locked
      then pure unit
      else modify (\st -> st 
        { target = mel
        , curFrame = fromMaybe Nothing $ getFrame <$> mel <*> (time <$> st.curFrame)
        , layer = fromMaybe (-1) $ getLayer <$> mel
        }
      )
    pure (reply unit)
    
  eval (ShiftFrame mp reply) = do
    modify (\st -> st 
      { curFrame = shiftPosition (fromMaybe (Point {x:0,y:0}) mp) <$> st.curFrame 
      }
    )
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