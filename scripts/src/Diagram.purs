module App.Diagram where

import Prelude

import Data.Foldable
import Data.Array (insertBy, (!!), length, updateAt, modifyAt, findLastIndex, insertAt, cons, last, filter, mapWithIndex, snoc)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe, isJust)
import Data.Int (round)
import Data.Traversable (sequence)

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

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement)
import DOM.HTML.Window (scrollX, scrollY)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import DOM.Event.Types (MouseEvent)

import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free (fillRect, setFillStyle, runGraphics)

import App.Element as E
import App.Element.Presets
import App.ElementEditor as ElEdit
import App.Toolbar as Toolbar
import App.TimeControls as TControls
import App.Static as S
import App.Property
import App.Helpers
import Example.IntermissionA

type State = { time :: Int
             , ctx :: Maybe Context2D
             , color :: { r :: Int, g :: Int, b :: Int }
             , statics :: Array S.Static
             , elements :: Array ( Array E.Element )
             , targetIndex :: { layer :: Int, idx :: Int }
             }
             
data Query a 
  = Initialize a
  | Tick a
  | SetTime Int a
  | ModTarget (Maybe E.Element) a
  | ClickCanvas Point a
  | FetchState String a
  
type ChildQuery = Coproduct3 ElEdit.Query Toolbar.Query TControls.Query
type ChildSlot = Either3 Unit Unit Unit

type UIEff eff = Aff (canvas :: CANVAS, console :: CONSOLE, dom :: DOM | eff)
    
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
  render st = let 
    loc = st.targetIndex 
    target = (\l -> l !! loc.idx) =<< (st.elements !! loc.layer) in
    HH.div_
      [ HH.slot' cp2 unit Toolbar.toolbar unit absurd
      , HH.span [ HP.id_ "center-col" ]
                [ HH.canvas [ HP.id_ "canvas"
                            , HP.ref (RefLabel "cvs")
                            , HE.onClick $ HE.input (\e -> ClickCanvas {x: pageX e, y: pageY e}) 
                            ]
                , HH.slot' cp3 unit TControls.controls st.time (tcListener st.time)
                ]
      , HH.slot' cp1 unit ElEdit.component target (HE.input ModTarget) 
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
    modify (\st -> st {elements = map (map (\e -> E.setTime e t)) st.elements, time=t})
    pure next
    
  eval (Initialize next) = do
    cv <- liftEff $ getCanvasElementById "canvas"
    case cv of
      Nothing -> pure next
      Just canvas -> do
          liftEff $ setCanvasDimensions {width: 800.0, height: 800.0} canvas
          context <- liftEff $ getContext2D canvas
          modify (\state -> state { ctx = Just context })
          pure next
    
  --this isn't ideal, but we can't look at the toolbar's state outside of eval
  --so the other option is replicating the state on the diagram, which I like less
  eval (ClickCanvas p next) = do
    t <- gets _.time
    e <- getHTMLElementRef (RefLabel "cvs")
    pos <- liftEff $ getOffset p e
    mode <- query' cp2 unit (request Toolbar.CheckClick)
    case fromMaybe Nothing mode of
      Nothing -> modify (\st -> st {time = t, targetIndex = resolveTarget st.elements pos } ) 
      Just Toolbar.CircB -> addElement $ circBase t pos
      Just Toolbar.RectB -> addElement $ rectBase t pos
      Just Toolbar.DnutB -> addElement $ dnutBase t pos
    pure next
    --This is probably WAY slower than it would be with lazy eval, so potentially revisit if it's an issue.
    where resolveTarget els pos = fromMaybe def $ last =<< (sequence $ filter isJust 
            $ mapWithIndex (\i l -> map (\v ->{layer:i, idx:v}) (findLastIndex (\element -> E.overlap element pos) l)) els)
          def = { layer: -1, idx: -1 }
          addElement e = do
            modify (\st -> st { elements = fromMaybe st.elements $ (\l -> updateAt e.layer l st.elements) =<< ( (\l -> snoc l e) <$> (st.elements !! e.layer) ) } )
     
  eval (ModTarget Nothing next) = do
    pure next
    
  --TODO: Move the target if layer has changed.
  eval (ModTarget (Just e) next) = do
    st <- get
    let loc = st.targetIndex
    case (\l -> updateAt loc.layer l st.elements) =<< (updateAt loc.idx e) =<< (st.elements !! loc.layer) of
      Just es -> modify (\st -> st {elements=es})
      Nothing -> pure unit
    pure next 
    
  eval (FetchState id next) = do
    liftEff $ log id
    pure next
    
  getOffset p Nothing = do
    pure p
    
  getOffset {x, y} (Just el) = do
    w    <- window
    rect <- getBoundingClientRect el
    scX  <- scrollX w
    scY  <- scrollY w
    pure {x: x - (round rect.left) - scX , y: y - (round rect.top) - scY }
    
    
  tcListener :: Int -> Int -> Maybe (Query Unit)
  tcListener t t' = if t == t'
                      then Nothing
                      else Just $ action $ SetTime t'
                      
  advanceFrame :: State -> State
  advanceFrame st = st { elements = map (map E.advanceFrame) st.elements, time=st.time+1 }
    
  drawGraphics st = case st.ctx of
    Just ctx -> runGraphics ctx $ do
                  setFillStyle $ colorToStr st.color
                  fillRect {x: 0.0, y:0.0, w: 800.0, h: 800.0}
                  traverse_ S.renderStatic st.statics
                  traverse_ (traverse_ E.renderEl) st.elements
    Nothing  -> pure unit