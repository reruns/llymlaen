module App.Components.Diagram where

import App.Prelude
import App.Components.ElementEditor as ElEdit
import App.Components.Toolbar as Toolbar
import App.Components.TimeControls as TControls
import App.Components.SaveResult as SaveResult
import App.Components.Settings as Settings

import App.Types
import App.Helpers.Mouse

import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4, cp5)
import Network.HTTP.Affjax as AX
import Network.HTTP.StatusCode(StatusCode(..))

type State = { time :: Int
             , ctx :: Maybe Context2D
             , body :: Diag
             , targetIndex :: Int
             , stagedEl :: Maybe (Int -> Point -> Element)
             , mousePos :: Maybe Point
             , mouseHeld :: Boolean
             }
          
defaultState :: State          
defaultState = { time: 0
               , ctx: Nothing
               , body: Diag {color: RGB {r:255,g:255,b:255}, length: 1000, elements: []}
               , targetIndex: -1
               , stagedEl: Nothing
               , mousePos: Nothing
               , mouseHeld: false
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
  | SetTime Int a
  | HandleElEdit ElEdit.Message a
  | HandleTB Toolbar.Message a
  | ClickCanvas Point a
  | ElShadow Point a
  | ClearPos a
  | UpdateSettings Settings.State a
  | MouseUnhold a
 
type ChildQuery = Coproduct5 ElEdit.Query Toolbar.Query TControls.Query SaveResult.Query Settings.Query
type ChildSlot = Either5 Unit Unit Unit Unit Unit

type UIEff eff = Aff 
  ( canvas  :: CANVAS
  , console :: CONSOLE
  , dom     :: DOM
  , ajax    :: AX.AJAX 
  , avar    :: AVAR
  , timer   :: TIMER
  , ref     :: REF
  | eff
  )
    
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
    div 
      [ onMouseUp   $ input_ MouseUnhold ]
      [ slot' cp2 unit Toolbar.toolbar unit (input HandleTB)
      , span [ id_ "center-col" ]
        [ div [id_ "cvs-container"]
          [ canvas  
            [ id_ "canvas"
              , ref (RefLabel "cvs")
              , onMouseDown $ input (\e -> ClickCanvas $ Point {x: pageX e, y: pageY e}) 
              , onMouseMove $ input (\e -> ElShadow $ Point {x: pageX e, y: pageY e})
              , onMouseLeave $ input_ ClearPos
            ]
          ]
        , slot' cp1 unit ElEdit.editorComponent unit (input HandleElEdit)
        , slot' cp3 unit TControls.controls st.time (input SetTime)
        ]
      , slot' cp4 unit SaveResult.saveComponent unit absurd
      , slot' cp5 unit Settings.settingsComponent unit (input UpdateSettings)
      ]
      
  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void (UIEff eff)
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
          drawCanvas
          pure next

  eval (ClickCanvas p next) = do
    t <- gets _.time
    e <- getHTMLElementRef (RefLabel "cvs")
    tar <- gets _.targetIndex
    tframe <- query' cp1 unit (request ElEdit.GetFrame)
    pos <- liftEff $ getOffset p e
    locked <- query' cp1 unit (request ElEdit.IsLocked)
    stagedEl <- gets _.stagedEl
    case stagedEl of
      Nothing -> unless (fromMaybe false locked) 
        $ modify (\st -> st { targetIndex = resolveTarget st.body tframe tar pos t
                            , mouseHeld = true})
      Just el -> modify (\st -> st { body = addElement st.body (el t pos)
                                   , stagedEl = Nothing
                                   , targetIndex = st.targetIndex+1})
    tar' <- gets _.targetIndex
    when (tar /= tar') refreshTarget
    pure next
    where resolveTarget (Diag d) tframe cur pos t = fromMaybe (-1) $ 
            findIndex (\{i,mf} -> fromMaybe false $ flip overlap pos <$> 
              if i == cur
              then fromMaybe Nothing $ tframe
              else mf) $ 
            mapWithIndex (\i el ->{i,mf: flip getFrame t $ el}) d.elements
    
  eval (HandleElEdit (ElEdit.UpdateEl elem) next) = do
    st <- get
    case (getElements st.body) !! st.targetIndex of
      Nothing -> pure unit
      Just target -> do
        let es   = fromMaybe [] $ deleteAt st.targetIndex $ getElements st.body
            idx  = fromMaybe (length es) $ findIndex (\el -> getLayer el >= (getLayer elem)) es
        modify $ _ { body = fromMaybe st.body $ setElements st.body 
                            <$> insertAt idx elem es
                   , targetIndex = idx}
    pure next
    
  eval (HandleElEdit ElEdit.RefreshEl next) = do
    drawCanvas
    pure next
    
  eval (MouseUnhold next) = do
    modify $ _ {mouseHeld = false, mousePos = Nothing}
    pure next

  eval (ElShadow p next) = do
    e <- getHTMLElementRef (RefLabel "cvs")
    pos <- liftEff $ getOffset p e
    held <- gets _.mouseHeld
    when held do
      prevPos <- gets _.mousePos
      let diff = vectorSub pos <$> prevPos
      _ <- query' cp1 unit (request (ElEdit.ShiftFrame diff))
      pure unit
    modify $ _ {mousePos = Just pos}
    drawCanvas
    pure next
    
  eval (ClearPos next) = do
    modify $ (\st -> if not st.mouseHeld then st {mousePos = Nothing} else st)
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
  
  drawCanvas = do
    editorFrame' <- query' cp1 unit (request ElEdit.GetFrame)
    let editorFrame = fromMaybe Nothing editorFrame'
    st <- get
    let frames = reverse $ mapWithIndex 
          (\i e -> 
            if i == st.targetIndex 
            then editorFrame 
            else getFrame e st.time 
          ) $ getElements st.body
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
    
  refreshTarget = do
    target <- gets (\st -> (getElements st.body) !! st.targetIndex)
    t <- gets _.time
    _ <- query' cp1 unit (request (ElEdit.SetTarget target))
    _ <- query' cp1 unit (request (ElEdit.SetTime t))
    drawCanvas
    pure unit