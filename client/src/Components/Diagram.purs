module App.Components.Diagram where

import Prelude

import Data.Argonaut (Json, jsonParser, encodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))
import Data.Foldable (traverse_)
import Data.Array ((!!), updateAt, findLastIndex, last, filter, mapWithIndex, snoc, unsafeIndex)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe, isJust)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Traversable (sequence)

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Halogen
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (scrollX, scrollY)
import DOM.HTML.HTMLElement (getBoundingClientRect)

import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free (fillRect, setFillStyle, runGraphics)


import App.Element.Presets (circBase, dnutBase, rectBase)
import App.ElementEditor as ElEdit
import App.Toolbar as Toolbar
import App.TimeControls as TControls

import App.Helpers (pageX, pageY)

type State = { time :: Int
             , ctx :: Maybe Context2D
             , body :: Diag
             , targetIndex :: { layer :: Int, idx :: Int }
             }
   
   
decodeResponse :: Json -> Either String Diag
decodeResponse response = do
  obj <- decodeJson response
  str <- obj .? "body"
  bodyStr <- jsonParser str
  decodeJson bodyStr
         
data Query a 
  = Initialize a
  | Save a
  | Load String a
  | Tick a
  | SetTime Int a
  | ModTarget (Maybe E.Element) a
  | ClickCanvas Point a
 
type ChildQuery = Coproduct3 ElEdit.Query Toolbar.Query TControls.Query
type ChildSlot = Either3 Unit Unit Unit

type UIEff eff = Aff (canvas :: CANVAS, console :: CONSOLE, dom :: DOM, ajax :: AX.AJAX | eff)
    
diaComp :: forall eff. Component HH.HTML Query Unit Void (UIEff eff)
diaComp = lifecycleParentComponent
  { render
  , eval
  , initializer: Just (action Initialize)
  , finalizer: Nothing
  , initialState: const defaultState
  , receiver: const Nothing
  } 
  where
  
  render :: State -> ParentHTML Query ChildQuery ChildSlot (UIEff eff)
  render st = let 
    loc = st.targetIndex 
    target = (\l -> l !! loc.idx) =<< (st.elements !! loc.layer) in
    HH.div_
      [ HH.slot' cp2 unit Toolbar.toolbar unit absurd
      , HH.button [HE.onClick $ HE.input_ Save] [HH.text "Save"]
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
    
  eval (Save next) = do
    st <- gets encodeState
    let pl = "body" := (show st) ~> jsonEmptyObject
    response <- liftAff $ (AX.post "/api/diagrams/" pl :: AX.Affjax _ Json)
    pure next
    
  eval (Load id next) = do
    response <- liftAff $ (AX.get ("/api/diagrams/" <> id) :: AX.Affjax _ Json)
    case decodeResponse response.response of
      Right b -> modify (_ {body=b})
      Left  s -> liftEff $ log s
    pure next
    
  eval (SetTime t next) = do
    modify (\st -> st {elements = map (map (\e -> E.setTime e t)) st.elements, time=t})
    pure next
    
  eval (Initialize next) = do
    cv <- liftEff $ getCanvasElementById "canvas"
    case cv of
      Nothing -> pure next
      Just canvas -> do
          _ <- liftEff $ setCanvasDimensions {width: 800.0, height: 800.0} canvas
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
      Just es -> modify (\state -> state {elements=es})
      Nothing -> pure unit
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
    