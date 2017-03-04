module App.Diagram where

import Prelude

import Data.Foldable
import Data.Array (insertBy, (!!), length, updateAt, modifyAt, findLastIndex, insertAt, cons)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Int (round)

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE) --currently unused, but useful for debug potentially
import Control.Apply ((*>))

import Halogen
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import DOM.Event.Types (MouseEvent)

import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free (fillRect, setFillStyle, runGraphics)

import App.Element as E
import App.Element.Presets
import App.ElementEditor as ElEdit
import App.Toolbar as Toolbar
import App.TimeControls as TControls
import App.Static as S
import App.Helpers
import Example.IntermissionA

type State = { time :: Int
             , ctx :: Maybe Context2D
             , color :: { r :: Int, g :: Int, b :: Int }
             , statics :: Array S.Static
             , elements :: Array E.Element
             , targetIndex :: Int
             }
             
data Query a 
  = Initialize a
  | Tick a
  | SetTime Int a
  | ModTarget (Maybe E.Element) a
  | ClickCanvas E.Point a
  
type ChildQuery = Coproduct3 ElEdit.Query Toolbar.Query TControls.Query
type ChildSlot = Either3 Unit Unit Unit

type UIEff eff = Aff (canvas :: CANVAS, console :: CONSOLE | eff)

advanceFrame :: State -> State
advanceFrame st = st { elements = map E.advanceFrame st.elements, time=st.time+1 }
  
drawGraphics st = case st.ctx of
  Just ctx -> runGraphics ctx $ do
                setFillStyle $ E.colorToStr st.color
                fillRect {x: 0.0, y:0.0, w: 800.0, h: 800.0}
                traverse_ S.renderStatic st.statics
                traverse_ E.renderEl st.elements
  Nothing  -> pure unit
    
diaComp :: forall eff. Component HH.HTML Query Unit Void (UIEff eff)
diaComp = lifecycleParentComponent
  { render
  , eval
  , initializer: Just (action Initialize)
  , finalizer: Nothing
  , initialState: const interA
  , receiver: const Nothing
  } 
  where
  
  render :: State -> ParentHTML Query ChildQuery ChildSlot (UIEff eff)
  render st =
    HH.div_
      [ HH.slot' cp2 unit Toolbar.toolbar unit absurd
      , HH.canvas [ HP.id_ "canvas"
                  , HE.onClick $ HE.input (\e -> ClickCanvas {x: pageX e, y: pageY e}) ]
      , case st.elements !! st.targetIndex of
          Just el -> HH.slot' cp1 unit ElEdit.component el (HE.input ModTarget)
          Nothing -> HH.div_ []
      , HH.slot' cp3 unit TControls.controls st.time (tcListener st.time)
      ]
      
  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void (UIEff eff)
  eval (Tick next) = do
    pause <- query' cp3 unit (request TControls.Paused)
    if pause == Just false
      then modify advanceFrame
      else pure unit
    st <- get
    liftEff $ drawGraphics st
    pure next
    
  eval (SetTime t next) = do
    modify (\st -> st {elements = map (\e -> E.setTime e t) st.elements, time=t})
    pure next
    
  eval (Initialize next) = do
    cv <- liftEff $ getCanvasElementById "canvas"
    case cv of
      Nothing -> pure next
      Just canvas -> do
          liftEff $ setCanvasDimensions {width: 800.0, height: 800.0} canvas
          context <- liftEff $ getContext2D canvas
          modify (\state -> state {ctx = Just context})
          pure next
          
  --this isn't ideal, but we can't look at the toolbar's state outside of eval
  --so the other option is replicating the state on the diagram, which I like less
  --TODO: check the offset of the canvas
  eval (ClickCanvas pos next) = do
    t <- gets _.time
    mode <- query' cp2 unit (request Toolbar.CheckClick)
    case fromMaybe Nothing mode of
      Nothing -> (gets _.elements) >>= (\es -> modify (\s -> s {targetIndex = go es ((length es) - 1) }))
      Just Toolbar.CircB -> addElement $ circBase t pos
      Just Toolbar.RectB -> addElement $ rectBase t pos
      Just Toolbar.DnutB -> addElement $ dnutBase t pos
    pure next
    where go es' i = case (es' !! i) of
                      Just e  -> if E.overlap e {x: pos.x, y: pos.y} then i else go es' (i-1)
                      Nothing -> -1
          addElement e = modify (\st ->
            case findLastIndex (\d -> d.layer < e.layer) st.elements of
              Just i  -> st {elements = fromMaybe st.elements $ insertAt i e st.elements, targetIndex = i}
              Nothing -> st {elements = cons e st.elements, targetIndex = 0})
     
  eval (ModTarget Nothing next) = do
    pure next
    
  eval (ModTarget (Just e) next) = do
    st <- get
    case updateAt (st.targetIndex) e (st.elements) of
      Just es -> modify (\st -> st {elements=es})
      Nothing -> pure unit
    pure next
    
  tcListener :: Int -> Int -> Maybe (Query Unit)
  tcListener t t' = if t == t'
                      then Nothing
                      else Just $ action $ SetTime t'