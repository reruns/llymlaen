module App.Components.Diagram where

import Prelude

import App.Components.ElementEditor as ElEdit
import App.Components.Toolbar as Toolbar
import App.Components.TimeControls as TControls
import App.Components.Modal as Modal

import App.Types.Element
import App.Types.Keyframe
import App.Types.Diag
import App.Types.Point
import App.Types.RGB

import App.Helpers (pageX, pageY)

import Data.Argonaut

import Data.Foldable (traverse_)
import Data.Array ((!!), updateAt, modifyAt, findIndex, findLastIndex, mapWithIndex)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Either (Either(..))
import Data.Int (round)

import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Halogen
import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.StatusCode

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
             , stagedEl :: Maybe (Int -> Point -> Element)
             , mousePos :: Maybe Point
             }
             
defaultState = { time: 0
               , ctx: Nothing
               , body: Diag {color: RGB {r:255,g:255,b:255}, elements: []}
               , targetIndex: -1
               , stagedEl: Nothing
               , mousePos: Nothing
               }
   
decodeResponse :: Json -> Either String Diag
decodeResponse response = do
  obj <- decodeJson response
  str <- obj .? "body"
  bodyStr <- jsonParser str
  decodeJson bodyStr
         
data Query a 
  = Initialize a
  | Load String a
  | Tick a
  | SetTime Int a
  | ModTarget (Maybe Keyframe) a
  | HandleTB Toolbar.Message a
  | ClickCanvas Point a
  | ElShadow Point a
  | ClearPos a
 
type ChildQuery = Coproduct4 ElEdit.Query Toolbar.Query TControls.Query Modal.Query
type ChildSlot = Either4 Unit Unit Unit Unit

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
    target = fromMaybe Nothing $ getFrame <$> ((getElements st.body) !! st.targetIndex) <*> (Just st.time) in
    HH.div_
      [ HH.slot' cp2 unit Toolbar.toolbar unit (HE.input HandleTB)
      , HH.span [ HP.id_ "center-col" ]
        [ HH.canvas [ HP.id_ "canvas"
                    , HP.ref (RefLabel "cvs")
                    , HE.onClick $ HE.input (\e -> ClickCanvas $ Point {x: pageX e, y: pageY e}) 
                    , HE.onMouseMove $ HE.input (\e -> ElShadow $ Point {x: pageX e, y: pageY e})
                    , HE.onMouseLeave $ HE.input_ ClearPos
                    ]
        , HH.slot' cp1 unit ElEdit.component target (HE.input ModTarget)
        , HH.slot' cp3 unit TControls.controls st.time (HE.input SetTime)
        ]
      , HH.slot' cp4 unit Modal.component unit absurd
      ]
      
  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void (UIEff eff)
  eval (Tick next) = do
    pause <- query' cp3 unit (request TControls.Paused)
    editorFrame' <- query' cp1 unit (request ElEdit.GetFrame)
    let editorFrame = fromMaybe Nothing editorFrame'
    if pause == Just false
      then modify (\st -> st {time=st.time+1})
      else pure unit
    st <- get
    let frames = mapWithIndex (\i e -> if i == st.targetIndex then editorFrame else getFrame e st.time ) $ getElements st.body
        shadow = case st.stagedEl of
          Nothing -> []
          Just el -> case st.mousePos of
            Nothing -> []
            Just p  -> [getFrame (el st.time p) st.time]
    case st.ctx of
      Just ctx -> liftEff $ runGraphics ctx $ do
                    setFillStyle $ show $ getColor st.body
                    fillRect {x: 0.0, y:0.0, w: 800.0, h: 800.0}
                    traverse_ (renderFrame <<< fromMaybe blankFrame) (frames <> shadow)
      Nothing  -> pure unit
    pure next
  
  eval (HandleTB (Toolbar.Insert el) next) = do
    modify $ _ {stagedEl = Just el}
    pure next
    
  eval (HandleTB (Toolbar.Save) next) = do
    st <- gets (encodeJson <<< _.body)
    _ <- query' cp2 unit (request (Toolbar.SetState true))
    let pl = "body" := (show st) ~> jsonEmptyObject
    response <- liftAff $ (AX.post "/api/diagrams/" pl :: AX.Affjax _ Json)
    when (response.status == StatusCode 200) $ do
      let idStr = "Saved! The ID for this diagram is " <> show response.response
      _ <- query' cp4 unit (request (Modal.SetString idStr))
      pure unit
    _ <- query' cp2 unit (request (Toolbar.SetState false))
    pure next
    
  eval (Load id next) = do
    response <- liftAff $ (AX.get ("/api/diagrams/" <> id) :: AX.Affjax _ Json)
    case decodeResponse response.response of
      Right b -> modify (_ {body=b, time=0})
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
          _ <- liftEff $ setCanvasDimensions {width: 600.0, height: 600.0} canvas
          context <- liftEff $ getContext2D canvas
          modify (\state -> state { ctx = Just context })
          pure next

  eval (ClickCanvas p next) = do
    t <- gets _.time
    e <- getHTMLElementRef (RefLabel "cvs")
    pos <- liftEff $ getOffset p e
    locked <- query' cp1 unit (request ElEdit.IsLocked)
    stagedEl <- gets _.stagedEl
    case stagedEl of
      Nothing -> unless (fromMaybe false locked) $ modify (\st -> st {targetIndex = resolveTarget st.body pos t} )
      Just el -> modify (\st -> st {body = addElement st.body (el t pos), stagedEl = Nothing})
    pure next
    where resolveTarget (Diag d) pos t = fromMaybe (-1) $ 
            findIndex (\mf -> fromMaybe false $ flip overlap pos <$> mf) $ 
            map (flip getFrame t) d.elements
     
  eval (ModTarget Nothing next) = do
    pure next
    
  eval (ModTarget (Just f) next) = do
    st <- get
    case modifyAt st.targetIndex (flip insertKey f) (getElements st.body) of
      Just es -> modify $ (\st -> st {body=setElements st.body es})
      Nothing -> pure unit
    pure next

  eval (ElShadow p next) = do
    e <- getHTMLElementRef (RefLabel "cvs")
    pos <- liftEff $ getOffset p e
    modify $ _ {mousePos = Just pos}
    pure next
    
  eval (ClearPos next) = do
    modify $ _ {mousePos = Nothing}
    pure next
    
  getOffset p Nothing = do
    pure p
    
  getOffset (Point {x, y}) (Just el) = do
    w    <- window
    rect <- getBoundingClientRect el
    scX  <- scrollX w
    scY  <- scrollY w
    pure $ Point {x: x - (round rect.left) - scX , y: y - (round rect.top) - scY }