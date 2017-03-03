module App.Diagram where

import Prelude

import Data.Foldable
import Data.Array (insertBy, (!!), length, updateAt, modifyAt, findLastIndex, insertAt, cons)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Int (round)

import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE)
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
import App.ElementEditor as ElEdit
import App.Static as S
import App.Helpers
import Example.IntermissionA

type State = { paused :: Boolean
             , time :: Int
             , ctx :: Maybe Context2D
             , color :: { r :: Int, g :: Int, b :: Int }
             , statics :: Array S.Static
             , elements :: Array E.Element
             , targetIndex :: Int
             }
             
data Query a 
  = TogglePlay a
  | Initialize a
  | Tick a
  | SetTime Int a
  | UpdateTarget {x :: Int, y :: Int } a
  | ModTarget (Maybe E.Element) a
  | AddElement E.Element a --TODO: redo the interaction model
  
type ChildQuery = Coproduct1 ElEdit.Query
type ChildSlot = Either1 Unit

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
      [ HH.canvas [ HP.id_ "canvas"
                  , HE.onClick $ HE.input (\e -> UpdateTarget {x: pageX e, y: pageY e}) ]
      , HH.div_ [ HH.button [HE.onClick $ HE.input_ $ AddElement E.circBase ]  [HH.text "Circle"]
                , HH.button [HE.onClick $ HE.input_ $ AddElement E.rectBase ]  [HH.text "Rectangle"]
                , HH.button [HE.onClick $ HE.input_ $ AddElement E.donutBase ] [HH.text "Donut"]
                ]
      , case st.elements !! st.targetIndex of
          Just el -> HH.slot' cp1 unit ElEdit.component el (HE.input ModTarget)
          Nothing -> HH.div_ []
      , HH.div_ [ HH.button [HE.onClick $ HE.input_ TogglePlay ] [HH.text "Play"]
                , E.slider [HP.title "time"] 0 1000 st.time SetTime
                , HH.h1_ [HH.text (show st.time)]
                ]
      ]
      
  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void (UIEff eff)
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
          
  eval (UpdateTarget pos next) = do
    es <- gets _.elements
    modify (\s -> s {targetIndex = go es ((length es) - 1) })
    pure next 
    where go es' i = case (es' !! i) of
            Just e  -> if E.overlap e {x: pos.x, y: pos.y} then i else go es' (i-1)
            Nothing -> -1
            
  eval (ModTarget Nothing next) = do
    pure next
    
  eval (ModTarget (Just e) next) = do
    st <- get
    case updateAt (st.targetIndex) e (st.elements) of
      Just es -> modify (\st -> st {elements=es})
      Nothing -> pure unit
    pure next
    
  eval (AddElement e next) = do
    modify (\st ->
      case findLastIndex (\d -> d.layer < e.layer) st.elements of
        Just i  -> st {elements = fromMaybe st.elements $ insertAt i e st.elements, targetIndex = i}
        Nothing -> st {elements = cons e st.elements, targetIndex = 0})
    pure next
    