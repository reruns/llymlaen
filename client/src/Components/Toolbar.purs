module App.Components.Toolbar where

import App.Prelude

import App.Types.Element
import App.Types.Point
import App.Types.RGB
import App.Element.Presets (circBase, dnutBase, rectBase)

type State = Boolean
data Message 
  = Insert (Int -> Point -> Element)
  | Settings
  | Save

data Query a = InsertCirc a
             | InsertRect a
             | InsertDnut a
             | ReqSave    a
             | SetState Boolean (Unit -> a)
             | OpenSettings a

           
toolbar :: forall m. Component HTML Query Unit Message m
toolbar = component 
  { initialState: const false
  , render
  , eval
  , receiver: const Nothing
  }
  where
  
  render :: State -> ComponentHTML Query
  render st =
    span [id_ "toolbar"] $ 
      [ a [id_ "home-link", href "/"] []
      , div [id_ "tools"] 
        [ div [class_ $ ClassName "dropdown"] 
          [ a [class_ $ ClassName "dropbtn"] [ text "Add an Element" ]
          , div [class_ $ ClassName "dropdown-content"] 
            [ a [onClick $ input_ InsertCirc] [text "Circle"]
            , a [onClick $ input_ InsertRect] [text "Rectangle"]
            , a [onClick $ input_ InsertDnut] [text "Donut"]
            ]            
          ]
        , a [onClick $ input_ OpenSettings ]
          [ text "Settings"]
        , a [ onClick $ input_ ReqSave ] 
          [ text (if st then "Saving..." else "Share") ]   
        ] 
      ] 
  
  eval :: forall m. Query ~> ComponentDSL State Query Message m
  eval (InsertCirc next) = do
    raise $ Insert circBase
    pure next
    
  eval (InsertRect next) = do
    raise $ Insert rectBase
    pure next
    
  eval (InsertDnut next) = do
    raise $ Insert dnutBase
    pure next
    
  eval (ReqSave next) = do
    raise $ Save
    pure next
    
  eval (SetState b reply) = do
    put b
    pure (reply unit)
    
  eval (OpenSettings next) = do
    raise $ Settings
    pure next
    