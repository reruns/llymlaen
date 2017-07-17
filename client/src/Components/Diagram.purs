module App.Components.Diagram where

import App.Prelude
import App.Components.ElementEditor as ElEdit
import App.Components.Toolbar as Toolbar
import App.Components.TimeControls as TControls
import App.Components.SaveResult as SaveResult
import App.Components.Settings as Settings

import App.Types.Element
import App.Types.Keyframe
import App.Types.Diag
import App.Types.Point
import App.Types.RGB

import App.Helpers.Mouse

import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4, cp5)
import Network.HTTP.Affjax as AX
import Network.HTTP.StatusCode



type State = { time :: Int
             , ctx :: Maybe Context2D
             , body :: Diag
             , targetIndex :: Int
             , stagedEl :: Maybe (Int -> Point -> Element)
             , mousePos :: Maybe Point
             }
             
defaultState = { time: 0
               , ctx: Nothing
               , body: Diag {color: RGB {r:255,g:255,b:255}, length: 1000, elements: []}
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
  | UpdateSettings Settings.State a
 
type ChildQuery = Coproduct5 ElEdit.Query Toolbar.Query TControls.Query SaveResult.Query Settings.Query
type ChildSlot = Either5 Unit Unit Unit Unit Unit

type UIEff eff = Aff (canvas :: CANVAS, console :: CONSOLE, dom :: DOM, ajax :: AX.AJAX | eff)
    
diaComp :: forall eff. Component HTML Query Unit Void (UIEff eff)
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
  render st =
    div_
      [ slot' cp2 unit Toolbar.toolbar unit (input HandleTB)
      , span [ id_ "center-col" ]
        [ canvas [ id_ "canvas"
                    , ref (RefLabel "cvs")
                    , onClick $ input (\e -> ClickCanvas $ Point {x: pageX e, y: pageY e}) 
                    , onMouseMove $ input (\e -> ElShadow $ Point {x: pageX e, y: pageY e})
                    , onMouseLeave $ input_ ClearPos
                    ]
        , slot' cp1 unit ElEdit.editorComponent unit (input ModTarget)
        , slot' cp3 unit TControls.controls st.time (input SetTime)
        ]
      , slot' cp4 unit SaveResult.saveComponent unit absurd
      , slot' cp5 unit Settings.settingsComponent unit (input UpdateSettings)
      ]
      
  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void (UIEff eff)
  eval (Tick next) = do
    pause <- query' cp3 unit (request TControls.Paused)
    editorFrame' <- query' cp1 unit (request ElEdit.GetFrame)
    let editorFrame = fromMaybe Nothing editorFrame'
    if pause == Just false
      then do
        modify (\st -> st {time=st.time+1})
        refreshTarget
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
      _ <- query' cp4 unit (request (SaveResult.Succeed (show response.response)))
      pure unit
    _ <- query' cp2 unit (request (Toolbar.SetState false))
    pure next
    
  eval (HandleTB (Toolbar.Settings) next) = do
    bod <- gets _.body
    let vals = Just {length: getLength bod, color: getColor bod}
    _ <- query' cp5 unit (request (Settings.SetValues vals))
    pure next
    
    
  eval (Load id next) = do
    response <- liftAff $ (AX.get ("/api/diagrams/" <> id) :: AX.Affjax _ Json)
    case decodeResponse response.response of
      Right b -> do
        _ <- query' cp3 unit (request (TControls.SetMax $ getLength b))
        modify (_ {body=b, time=0})
      Left  s -> liftEff $ log s
    pure next
    
  eval (SetTime t next) = do
    modify (\st -> st {time=t})
    refreshTarget
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
    tar <- gets _.targetIndex
    pos <- liftEff $ getOffset p e
    locked <- query' cp1 unit (request ElEdit.IsLocked)
    stagedEl <- gets _.stagedEl
    case stagedEl of
      Nothing -> unless (fromMaybe false locked) $ modify (\st -> st {targetIndex = resolveTarget st.body pos t})
      Just el -> modify (\st -> st {body = addElement st.body (el t pos), stagedEl = Nothing, targetIndex = st.targetIndex+1})
    tar' <- gets _.targetIndex
    when (tar /= tar') refreshTarget
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
    
  eval (UpdateSettings set next) = do
    case set of
      Nothing       -> pure unit
      Just settings -> do
        _ <- query' cp3 unit (request (TControls.SetMax settings.length))
        modify $ (\st 
          -> st { body = setLength settings.length 
                       $ setColor settings.color st.body})
    pure next  
    
  refreshTarget = do
    target <- gets (\st -> (getElements st.body) !! st.targetIndex)
    t <- gets _.time
    case target of --this can be consolidated somehow
      Nothing -> do
        _ <- query' cp1 unit (request (ElEdit.SetFrame Nothing))
        pure unit
      Just el -> do
        _ <- query' cp1 unit (request (ElEdit.SetFrame (getFrame el t)))
        pure unit