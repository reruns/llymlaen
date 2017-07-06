module App.Components.TimeControls where

import Prelude

import App.Helpers.Forms

import Data.Int (floor, toNumber)
import Data.Number.Format (toStringWith, fixed)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { paused :: Boolean, time :: Int, max :: Int}
initState = {paused: true, time: 0, max: 1000}

data Query a = SetTime Int a
             | SetMax Int (Unit -> a)
             | TogglePlay a
             | HandleInput Int a
             | Paused (Boolean -> a)
             
formatTime :: Int -> String
formatTime f = (show m) <> ":" <> (if s < 10.0 then "0" else "") <> sStr
  where time = (toNumber f) / 62.5        
        m = div (floor time) 60
        s = (time - toNumber (60*m)) --it's legal to call mod on a Number, but it doesn't work.
        sStr = toStringWith (fixed 2) s
  

controls :: forall m. H.Component HH.HTML Query Int Int m
controls = H.component 
  { initialState: const initState
  , render
  , eval
  , receiver: HE.input HandleInput
  } 
  where
  
  render :: State -> H.ComponentHTML Query
  render st = 
    HH.div [ HP.id_ "time-controls" ]
    [ slider [HP.title "time"] 0 st.max st.time SetTime
    , HH.a 
      [ HE.onClick $ HE.input_ TogglePlay
      , HP.class_ $ HH.ClassName "a-button" 
      ] 
      [ if st.paused then HH.text "Play" else HH.text "Pause"]
    , HH.h3_ [HH.text (formatTime st.time)]
    ]
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Int m
  eval (SetTime t next) = do
    H.modify ( _ {time = t} )
    H.raise t
    pure next
  
  eval (HandleInput t next) = do
    max <- H.gets _.max
    if t <= max
      then H.modify ( _ {time = t} )
      else H.modify (_ {paused = true, time = max})
    pure next
    
  eval (SetMax max reply) = do
    t <- H.gets _.time
    if t <= max
      then H.modify $ _ {max=max}
      else H.modify $ _ {max=max, time=max}
    pure (reply unit)
    
  eval (TogglePlay next) = do
    H.modify (\st -> st {paused = not st.paused})
    pure next
    
  eval (Paused reply) = do
    paused <- H.gets _.paused
    pure (reply paused)