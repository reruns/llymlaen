module App.Components.Modal where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Maybe String

data Query a = HandleInput State a
             | Close a
             
component :: forall m. H.Component HH.HTML Query State State m
controls = H.component 
  { initialState: const initState
  , render
  , eval
  , receiver: HE.input HandleInput
  } 
  where
  
  render :: State -> H.ComponentHTML Query
  render st = 
    let outerClasses = if isJust st then ["modal"] else ["modal","off"]
        text  = fromMaybe "" st
    HH.div [HP.classes outerClasses] 
      [ HH.span [HP.class_ "close"] 
          [ HH.text "X"]
        HH.div [HP.class_ "modal-content"] 
          [ HH.text text]
      ]
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Int m
  eval (HandleInput mstr next) = do
    H.put mstr
    pure next
    
  eval (Close next) = do
    H.put Nothing
    H.raise Nothing
    pure next
  