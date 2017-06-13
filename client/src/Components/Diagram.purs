module App.Components.Diagram where

import App.Element.Presets (circBase, dnutBase, rectBase)
import App.Components.ElementEditor as ElEdit
import App.Components.Toolbar as Toolbar
import App.Components.TimeControls as TControls

import App.Helpers (pageX, pageY)

import Prelude

import Data.Argonaut

import Data.Foldable (traverse_)
import Data.Array ((!!), updateAt, modifyAt, findIndex, findLastIndex)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe, isJust)
import Data.Either (Either(..))
import Data.Int (round)

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

type State = { time :: Int
             , ctx :: Maybe Context2D
             , body :: Diag
             , targetIndex :: Int
             }
   
decodeResponse :: Json -> Either String Diag
decodeResponse response = do
  obj <- decodeJson response
  str <- obj .? "body"
  bodyStr <- jsonParser str
  decodeJson bodyStr
  
encodeMessage :: State -> Json
encodeState st =
  "body" := st.body ~>
  jsonEmptyObject
         
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
      then modify (\st -> st {time=st.time+1})
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
    modify (\st -> st {time=t})
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
      Nothing -> modify (\st -> st {time = t, targetIndex = resolveTarget st.body pos } ) 
      Just Toolbar.CircB -> insertElem $ circBase t pos
      Just Toolbar.RectB -> insertElem $ rectBase t pos
      Just Toolbar.DnutB -> insertElem $ dnutBase t pos
    pure next
    where insertElem el = modify (\st -> st {body = addElement st.body el})
          resolveTarget (Diag d) pos = fromMaybe -1 $ findIndex (flip overlap pos) d.elements
     
  eval (ModTarget Nothing next) = do
    pure next
    
  eval (ModTarget (Just f) next) = do
    st <- get
    case modifyAt st.targetIndex (flip insertKey f) (getElements st.body) of
      Just es -> modify $ _ {body=setElements body es}
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
    
  drawGraphics st = case st.ctx of
    Just ctx -> runGraphics ctx $ do
                  setFillStyle $ show st.color
                  fillRect {x: 0.0, y:0.0, w: 800.0, h: 800.0}
                  traverse_ S.renderStatic st.statics
                  traverse_ (renderTime st.time) st.elements
    Nothing  -> pure unit
    