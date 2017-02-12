module App.Diagram where

import Prelude

import Data.Foldable
import Data.Array (insertBy, (!!), length)
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

type State = { paused :: Boolean
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
  | UpdateTarget {x :: Number, y :: Number} a


advanceFrame :: State -> State
advanceFrame st = st {elements = map updateOne st.elements } where
  updateOne (E.Drawable d) = d.updated unit

insertElement :: State -> E.Drawable -> State
insertElement st dr = 
  st {elements = insertBy (comparing (\(E.Drawable d) -> d.layer)) dr st.elements}
  
  
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
      , fromMaybe (H.div_ []) $ ((\(E.Drawable d) -> d.formed) <$> (st.elements !! st.targetIndex))
      ]
      
  eval :: Query ~> ComponentDSL State Query (Aff (canvas :: CANVAS, console :: CONSOLE | eff))
  eval (TogglePlay next) = do
    pause <- gets _.paused
    modify (\st -> st {paused = not pause})
    pure next
  
  eval (Tick next) = do
    pause <- gets _.paused
    if pause
      then pure next
      else do
        modify advanceFrame
        st <- get
        fromEff $ drawGraphics st
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