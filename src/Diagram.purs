module App.Diagram where

import Prelude

import Data.Foldable
import Data.Array (insertBy, (!!), length, updateAt, modifyAt, findLastIndex, insertAt, cons)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Apply ((*>))

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Types (MouseEvent)
import Halogen.HTML.Events.Handler (preventDefault, EventHandler)

import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free (fillRect, setFillStyle, runGraphics)

import App.Element as E
import App.Validators (validateSetTime)

type State = { paused :: Boolean
             , time :: Int
             , ctx :: Maybe Context2D
             , color :: { r :: Int, g :: Int, b :: Int }
             , statics :: Array E.Drawable
             , elements :: Array E.Drawable 
             , targetIndex :: Int
             }
             
data Query a 
  = TogglePlay a
  | Initialize a
  | Tick a
  | SetTime Int a
  | UpdateTarget {x :: Number, y :: Number} a
  | ModTarget E.Drawable a
  | AddMoment a
  | AddElement E.Drawable a --TODO: redo the interaction model
  


advanceFrame :: State -> State
advanceFrame st = st {elements = map updateOne st.elements, time=st.time+1 } where
  updateOne (E.Drawable d) = d.updated unit
  
drawGraphics st = let 
  drawOne (E.Drawable d) = d.drawn in
  case st.ctx of
    Just ctx -> runGraphics ctx $ do
                  setFillStyle $ E.colorToStr st.color
                  fillRect {x: 0.0, y:0.0, w: 800.0, h: 800.0}
                  traverse_ drawOne st.statics
                  traverse_ drawOne st.elements
    Nothing  -> pure unit
    
getCoords e = do
  pure $ Just $ action $ UpdateTarget {x:e.pageX, y:e.pageY}
  

diaComp :: forall eff. Component State Query (Aff (canvas :: CANVAS, console :: CONSOLE | eff))
diaComp = lifecycleComponent
  { render: render
  , eval: eval
  , initializer: Just (action Initialize)
  , finalizer: Nothing
  } where
  render :: State -> ComponentHTML Query
  render st =
    H.div_
      [ H.canvas [ HP.id_ "canvas"
                 , HE.onClick (\e -> preventDefault *> getCoords e) ]
      , H.div_ [ H.button_ [H.Text "Circle"]
               , H.button_ [H.Text "Rectangle"]
               , H.button_ [H.Text "Donut"]
               ]
      , case ((\(E.Drawable d) -> (d.formed ModTarget)) <$> (st.elements !! st.targetIndex)) of
          Just props -> H.form_ $ props <> [H.button [HE.onClick (\_ -> preventDefault $> (Just (action AddMoment)))] [H.Text "Apply"]]
          Nothing    -> H.div_ []
      , H.div_ [ H.button_ [H.Text "Play"]
               , H.input [ HP.inputType HP.InputRange 
                         , HP.IProp $ H.prop (H.propName "min") (Just $ H.attrName "min") 0
                         , HP.IProp $ H.prop (H.propName "max") (Just $ H.attrName "max") 1000 --TODO: config max time
                         , HE.onValueChange (\s -> (map (action <<< SetTime)) <$> (validateSetTime s 1000))
                         ]
               , H.h1_ [H.text (show st.time)]
               ]
      ]
      
  eval :: Query ~> ComponentDSL State Query (Aff (canvas :: CANVAS, console :: CONSOLE | eff))
  eval (TogglePlay next) = do
    pause <- gets _.paused
    modify (\st -> st {paused = not pause})
    pure next
  
  eval (Tick next) = do
    pause <- gets _.paused
    if pause
      then pure unit
      else modify advanceFrame
    st <- get
    fromEff $ drawGraphics st
    pure next
    
  eval (SetTime t next) = do
    modify (\st -> st {elements = map (\(E.Drawable d) -> d.setTime t) st.elements, time=t})
    pure next
    
  eval (Initialize next) = do
    cv <- fromEff $ getCanvasElementById "canvas"
    case cv of
      Nothing -> pure next
      Just canvas -> do
          fromEff $ setCanvasDimensions {width: 800.0, height: 800.0} canvas
          context <- fromEff $ getContext2D canvas
          modify (\state -> state {ctx = Just context})
          pure next
          
  eval (UpdateTarget pos next) = do
    es <- gets _.elements
    modify (\s -> s {targetIndex = go es ((length es) - 1) })
    pure next 
    where go es' i = case (es' !! i) of
            Just (E.Drawable e) -> if e.overlap pos then i else go es' (i-1)
            Nothing             -> -1
            
  eval (ModTarget d next) = do
    st <- get
    case updateAt (st.targetIndex) d (st.elements) of
      Just as -> modify (\st -> st {elements=as})
      Nothing -> pure unit
    pure next
    
  eval (AddMoment next) = do
    st <- get
    case modifyAt (st.targetIndex) (\(E.Drawable d) -> d.addMoment unit) st.elements of
      Just as -> modify (\st -> st {elements=as})
      Nothing -> pure unit
    pure next
    
  eval (AddElement (E.Drawable el) next) = do
    modify (\st ->
      case findLastIndex (\(E.Drawable d) -> d.layer < el.layer) st.elements of
        Just i  -> st {elements = fromMaybe st.elements $ insertAt i (E.Drawable el) st.elements, targetIndex = i}
        Nothing -> st {elements = cons (E.Drawable el) st.elements, targetIndex = 0})
    pure next
    