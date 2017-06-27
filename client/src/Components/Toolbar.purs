module App.Components.Toolbar where

import Prelude

import Data.Maybe (Maybe(..), isJust)

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
    HH.span [HP.id_ "toolbar"] $ 
      [ HH.h2_ 
        [ 
          HH.text "Add an Element"
        ] 
      ] <> elemButtons 
    where
    active = map HH.ClassName ["a-button","active"]
    inactive = map HH.ClassName ["a-button"] 
    elemButtons = map (\{b,t} -> 
      HH.a [ HP.classes $ if st==Just b then active else inactive 
           , HE.onClick $ HE.input_ $ Press b ] 
           [ HH.text t ]) 
      [{b:CircB,t:"Circle"},{b:RectB,t:"Rectangle"},{b:DnutB,t:"Donut"}]
  
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