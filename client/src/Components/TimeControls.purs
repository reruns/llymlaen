module App.Components.TimeControls where

import App.Prelude
import App.Helpers.Forms

import Data.Number.Format (toStringWith, fixed)
import Control.Monad.Eff.Timer (setInterval, clearInterval, TIMER)
import Control.Coroutine.Aff (produce)
import Halogen.Query.EventSource (EventSource(..))
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, REF())



type State = { paused :: Boolean
             , time :: Int
             , max :: Int
             , tickStopper :: Aff (HalogenEffects TimerEffects) Unit
             }
             
type TimerEffects = 
  ( timer :: TIMER
  , ref   :: REF
  , dom   :: DOM
  )
  
initState :: State
initState = { paused: true
            , time: 0
            , max: 1000
            , tickStopper: pure unit
            }

data Query a = SetTime Int a
             | SetMax Int (Unit -> a)
             | TogglePlay a
             | HandleInput Int a
             | Paused (Boolean -> a)
             | FrameAdvance a
             
formatTime :: Int -> String
formatTime f = (show m) <> ":" <> (if s < 10.0 then "0" else "") <> sStr
  where time = (toNumber f) * (toNumber framerate) / 1000.0       
        m = (floor time) / 60
        s = (time - toNumber (60*m)) --it's legal to call mod on a Number, but it doesn't work.
        sStr = toStringWith (fixed 2) s
  

controls :: forall m. Component HTML Query Int Int m
controls = component 
  { initialState: const initState
  , render
  , eval
  , receiver: input HandleInput
  } 
  where
  
  render :: State -> ComponentHTML Query
  render st = 
    div [ id_ "time-controls" ]
    [ slider [title "time"] 0 st.max st.time SetTime
    , a 
      [ onClick $ input_ TogglePlay
      , class_ $ ClassName "a-button" 
      ] 
      [ if st.paused then text "Play" else text "Pause"]
    , h3_ [text (formatTime st.time)]
    ]
  
  eval :: forall m. Query ~> ComponentDSL State Query Int m
  eval (SetTime t next) = do
    modify ( _ {time = t} )
    raise t
    pure next
  
  eval (HandleInput t next) = do
    max <- gets _.max
    if t <= max
      then modify ( _ {time = t} )
      else modify (_ {paused = true, time = max})
    pure next
    
  eval (SetMax max reply) = do
    t <- gets _.time
    if t <= max
      then modify $ _ {max=max}
      else modify $ _ {max=max, time=max}
    pure (reply unit)
    
  eval (TogglePlay next) = do
    modify (\st -> st {paused = not st.paused})
    paused <- gets _.paused
    if paused
      then do
        ts <- gets _.tickStopper
        -- liftAff ts
        pure unit
      else do
        --ts <- startInterval
        modify $ _ {tickStopper = pure unit}
    pure next
    
  eval (Paused reply) = do
    paused <- gets _.paused
    pure (reply paused)
    
  eval (FrameAdvance next) = do
    pure next
    
  startInterval :: forall t. ComponentDSL State Query Int t (Aff (HalogenEffects TimerEffects) Unit)
  startInterval = do
    ref <- liftEff $ newRef Nothing

    subscribe $ EventSource $ pure 
      { producer: produce \emit -> do
          i <- setInterval framerate $ emit $ Left (action FrameAdvance)
          writeRef ref (Just i)
      , done: pure unit
      }

    pure $ maybe (pure unit) (liftEff <<< clearInterval) =<< liftEff (readRef ref)