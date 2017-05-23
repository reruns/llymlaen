module App.ElementEditor where

import App.Element
import App.Property

import Prelude
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Array (concat, mapWithIndex, updateAt, (!!))


import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Maybe Element
data Query a = FormChange Int Property a | HandleInput (Maybe Element) a | AddMoment a

renderProp i (Enabled b)   = [checkBox [HP.title "enabled"]  b (FormChange i <<< Enabled)]
renderProp i (Bordered b)  = [checkBox [HP.title "bordered"] b (FormChange i <<< Bordered)]
renderProp i (Color c)     = map (\{v,h} -> slider [] 0 255 v (FormChange i <<< Color <<< h)) [{v:c.r,h:setR c}, {v:c.g,h:setG c}, {v:c.b,h:setB c}]
renderProp i (Position p)  = map (\{v,h} -> number [] v (FormChange i <<< Position <<< h)) [{v:p.x,h:setX p}, {v: p.y, h: setY p}]
renderProp i (Opacity o)   = [slider [HP.title "opacity"] 0 100 o (FormChange i <<< Opacity)]
renderProp i (Angle a)     = [slider [HP.title "angle"] 0 360 a (FormChange i <<< Angle)]
renderProp i (Circle r)    = [ number [] r (FormChange i <<< Circle) ]
renderProp i (Rect w h)    = [ number [] w (FormChange i <<< (flip Rect) h)
                             , number [] h (FormChange i <<< Rect w) ]
renderProp i (Donut r1 r2) = [ number [] r1 (FormChange i <<< (flip Donut) r2)
                             , number [] r2 (FormChange i <<< Donut r1) ]
  
component :: forall m. H.Component HH.HTML Query State State m
component =
  H.component
    { initialState: const Nothing
    , render
    , eval
    , receiver: HE.input HandleInput
    } where
  
  render :: State -> H.ComponentHTML Query
  render Nothing = HH.div_ []
  render (Just state) 
    = HH.div [HP.id_ "el-editor"] $ 
      ( concat $ mapWithIndex renderProp state.current.props) 
      <> [ HH.button 
            [ HE.onClick $ HE.input_ AddMoment ] 
            [ HH.text "Apply"]
         ]
    
  eval :: forall m. Query ~> H.ComponentDSL State Query State m
  eval (HandleInput mel next) = do
    st <- H.get
    when (not $ fromMaybe false $ matchEl <$> mel <*> st) $ H.put mel
    pure next
    
  eval (FormChange i prop next) = do
    props <- H.gets $ (fromMaybe []) <<< (map _.current.props)
    if isJust $ (recProp const prop) <$> (props !! i) 
      then do 
        H.modify $ map (\st -> st {current = st.current {props = fromMaybe props $ updateAt i prop props}})
        H.raise =<< H.get
      else pure unit
    pure next
  
  eval (AddMoment next) = do
    H.modify $ map (\st -> insertKey st st.current)
    H.raise =<< H.get
    pure next
    