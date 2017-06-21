module App.Components.Modal where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Maybe

type State = Maybe String

data Query a = SetString String (Unit -> a)
             | Close a
             
component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.component 
  { initialState: const Nothing
  , render
  , eval
  , receiver: const Nothing
  } 
  where
  
  render :: State -> H.ComponentHTML Query
  render st = 
    let outerClasses = HH.ClassName <$> (if isJust st then ["modal"] else ["modal","off"])
        text  = fromMaybe "" st in
    HH.div [HP.classes outerClasses] 
      [ HH.div [HP.class_ $ HH.ClassName "modal-content"] 
          [ HH.div_ [ HH.text text ]
          , HH.button [HE.onClick $ HE.input_ Close] 
              [ HH.text "OK" ]
          ]
      ]
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Void m
  eval (SetString text reply) = do
    H.put $ Just text
    pure (reply unit)
    
  eval (Close next) = do
    H.put Nothing
    pure next
  