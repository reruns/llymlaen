module App.Components.TimeControls where

import Prelude

import App.Helpers.Forms

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { paused :: Boolean, time :: Int}
initState = {paused: true, time: 0}

data Query a = SetTime Int a
             | TogglePlay a
             | HandleInput Int a
             | Paused (Boolean -> a)

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
    HH.div_ [ HH.button [HE.onClick $ HE.input_ TogglePlay ] [HH.text "Play"]
            , slider [HP.title "time"] 0 1000 st.time SetTime
            , HH.h1_ [HH.text (show st.time)]
            ]
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Int m
  eval (SetTime t next) = do
    H.modify ( _ {time = t} )
    H.raise t
    pure next
  
  eval (HandleInput t next) = do
    H.modify ( _ {time = t} )
    pure next
    
  eval (TogglePlay next) = do
    H.modify (\st -> st {paused = not st.paused})
    pure next
    
  eval (Paused reply) = do
    paused <- H.gets _.paused
    pure (reply paused)