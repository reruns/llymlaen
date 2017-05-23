module App.Toolbar where

import Prelude
import Data.Maybe (Maybe(..), isJust)

import App.Element

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Button = RectB | CircB | DnutB
derive instance eqButton :: Eq Button

type State = Maybe Button
data Query a = Press Button a
             | CheckClick (State -> a)

           
toolbar :: forall m. H.Component HH.HTML Query Unit Void m
toolbar = H.component 
  { initialState: const Nothing
  , render
  , eval
  , receiver: const Nothing
  }
  where
  
  render :: State -> H.ComponentHTML Query
  render st = 
    HH.span_ [ HH.button [ HE.onClick $ HE.input_ $ Press CircB ] [ HH.text "Circle" ]
             , HH.button [ HE.onClick $ HE.input_ $ Press RectB ] [ HH.text "Rectangle" ]
             , HH.button [ HE.onClick $ HE.input_ $ Press DnutB ] [ HH.text "Donut" ]
             , HH.h1_    [ HH.text $ if isJust st then "Add" else "Select"]
             ]
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Void m
  eval (Press b next) = do
    st <- H.get
    case ((==) b) <$> st of
      Just true -> H.put Nothing
      _         -> H.put (Just b)
    pure next
    
  eval (CheckClick reply) = do
    state <- H.get
    H.put Nothing
    pure (reply state)