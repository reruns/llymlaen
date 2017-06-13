module App.Components.ElementEditor where

import Prelude

import App.Types.Keyframe
import App.Types.Property
import App.Types.RGB
import App.Types.Point
import App.Helpers.Forms

import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Array (concat, mapWithIndex, updateAt, (!!))
import Data.Either (Either(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State = { frame  :: Maybe Keyframe
             , locked :: Boolean
             , heldFrame :: Maybe Keyframe
             }
             
data Query a 
  = FormChange Int Property a 
  | HandleInput (Maybe Keyframe) a 
  | AddFrame a
  | LockFrame a
  | GetFrame (Maybe Keyframe -> a)
  | IsLocked (Boolean -> a)
  
component :: forall m. H.Component HH.HTML Query (Maybe Keyframe) (Maybe Keyframe) m
component =
  H.component
    { initialState: const {locked: false, frame: Nothing, heldFrame: Nothing}
    , render
    , eval
    , receiver: HE.input HandleInput
    } where
  
  render :: State -> H.ComponentHTML Query
  render {frame: Nothing} = HH.div_ []
  render {frame: Just fr} 
    = HH.div [HP.id_ "el-editor"] $ 
      ( concat $ mapWithIndex renderProp (props fr)) 
      <> [ HH.button
            [ HE.onClick $ HE.input_ LockFrame ]
            [ HH.text "Lock" ]
         ]
      <> [ HH.button 
            [ HE.onClick $ HE.input_ AddFrame ] 
            [ HH.text "Apply"]
         ]
    
  eval :: forall m. Query ~> H.ComponentDSL State Query (Maybe Keyframe) m
  eval (HandleInput mbFrame next) = do
    locked <- H.gets _.locked
    if locked
      then H.modify (\st -> st {heldFrame = mbFrame, frame = setTime <$> (time <$> mbFrame) <*> st.frame})
      else H.modify (_ {frame = mbFrame})
    pure next
    
  eval (FormChange i prop next) = do
    fr <- H.gets _.frame
    let ps = fromMaybe [] $ props <$> fr
    if isJust $ (recProp const prop) <$> (ps !! i) 
      then do H.modify $ (\st -> st {frame = map (\(Keyframe f) -> Keyframe $ f {props = fromMaybe ps $ updateAt i prop ps}) st.frame})
      else pure unit
    pure next
  
  eval (AddFrame next) = do
    H.raise =<< H.gets _.frame
    pure next
  
  eval (GetFrame reply) = do
    frame <- H.gets _.frame
    pure (reply frame)
    
  eval (LockFrame next) = do
    locked <- H.gets _.locked
    if locked
      then H.modify (\st -> st {locked=false,frame=st.heldFrame})
      else H.modify (_ {locked=true})
    pure next
    
  eval (IsLocked reply) = do
    locked <- H.gets _.locked
    pure (reply locked)

renderProp i (Enabled b)   = [checkBox [HP.title "enabled"]  b (FormChange i <<< Enabled)]
renderProp i (Bordered b)  = [checkBox [HP.title "bordered"] b (FormChange i <<< Bordered)]
renderProp i (Color c)     = 
  map (\{v,h} -> slider [] 0 255 v (FormChange i <<< Color <<< h)) 
  [{v:getR c,h:setR c}, {v:getG c,h:setG c}, {v:getB c,h:setB c}]
renderProp i (Position p)  = 
  map (\{v,h} -> number [] v (FormChange i <<< Position <<< h)) 
  [{v:getX p,h:setX p}, {v: getY p, h: setY p}]
renderProp i (Opacity o)   = [slider [HP.title "opacity"] 0 100 o (FormChange i <<< Opacity)]
renderProp i (Angle a)     = [slider [HP.title "angle"] 0 360 a (FormChange i <<< Angle)]
renderProp i (Circle r)    = [ number [] r (FormChange i <<< Circle) ]
renderProp i (Rect w h)    = [ number [] w (FormChange i <<< (flip Rect) h)
                             , number [] h (FormChange i <<< Rect w) ]
renderProp i (Donut r1 r2) = [ number [] r1 (FormChange i <<< (flip Donut) r2)
                             , number [] r2 (FormChange i <<< Donut r1) ]
    