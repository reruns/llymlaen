module App.Components.Toolbar where

import Prelude

import App.Types.Element
import App.Types.Point
import App.Element.Presets (circBase, dnutBase, rectBase)

import Data.Maybe(Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean
data Message = Insert (Int -> Point -> Element) | Save

data Query a = InsertCirc a
             | InsertRect a
             | InsertDnut a
             | ReqSave    a
             | SetState Boolean (Unit -> a)

           
toolbar :: forall m. H.Component HH.HTML Query Unit Message m
toolbar = H.component 
  { initialState: const false
  , render
  , eval
  , receiver: const Nothing
  }
  where
  
  render :: State -> H.ComponentHTML Query
  render st =
    HH.span [HP.id_ "toolbar"] $ 
      [ HH.a [] [HH.text "Llymlaen"]
      , HH.div [HP.class_ $ HH.ClassName "dropdown"] 
          [ HH.a [HP.class_ $ HH.ClassName "dropbtn"] [ HH.text "Add an Element" ]
          , HH.div [HP.class_ $ HH.ClassName "dropdown-content"] 
              [ HH.a [HE.onClick $ HE.input_ InsertCirc] [HH.text "Circle"]
              , HH.a [HE.onClick $ HE.input_ InsertRect] [HH.text "Rectangle"]
              , HH.a [HE.onClick $ HE.input_ InsertDnut] [HH.text "Donut"]
              ]            
          ]
      , HH.a [ HE.onClick $ HE.input_ ReqSave ] 
             [ HH.text (if st then "Saving..." else "Share") ]    
      ] 
  
  eval :: forall m. Query ~> H.ComponentDSL State Query Message m
  eval (InsertCirc next) = do
    H.raise $ Insert circBase
    pure next
    
  eval (InsertRect next) = do
    H.raise $ Insert rectBase
    pure next
    
  eval (InsertDnut next) = do
    H.raise $ Insert dnutBase
    pure next
    
  eval (ReqSave next) = do
    H.raise $ Save
    pure next
    
  eval (SetState b reply) = do
    H.put b
    pure (reply unit)
    