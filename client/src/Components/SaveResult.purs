module App.Components.SaveResult where

import App.Prelude

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (href) as D
import DOM.HTML.Window (location)
import DOM.Classy.Event (toEvent, stopPropagation)
import DOM.Event.Types (Event)

import Control.Monad.Aff (Aff)

data State = Success String | Fail | Off

data Query a = Succeed String (Unit -> a)
             | Close a
             | StopPropagation Event a
             
off :: State -> Boolean
off Off = true
off _ = false
             
saveComponent :: forall eff. Component HTML Query Unit Void (Aff (dom :: DOM | eff))
saveComponent = component 
  { initialState: const Off
  , render
  , eval
  , receiver: const Nothing
  } 
  where
  
  render :: State -> ComponentHTML Query
  render Off = div [class_ $ ClassName "off"] []
  render Fail = div [class_ $ ClassName "off"] []
  render (Success link) =
    div 
    [ class_ $ ClassName "modal"
    , onClick $ input_ Close
    ] 
    [ div 
      [ class_ $ ClassName "modal-content"
      , onClick $ \e -> Just $ action (StopPropagation (toEvent e))
      ] 
      [ h3_ [ text "Saved!" ]
      , div_ 
        [ text "The link to share your work is here:"]
      , a [href link] [text link]
      , a [class_ $ ClassName "a-button", onClick $ input_ Close ] 
        [ text "Ok" ]
      ]
    ]
  
  eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (Succeed id reply) = do
    url <- liftEff $ D.href =<< location =<< window
    let link = (fromMaybe "" $ head $ split (Pattern "#") url) <> "#/" <> id
    put $ Success link
    pure (reply unit)
    
  eval (Close next) = do
    put Off
    pure next
    
  eval (StopPropagation ev next) = do
    liftEff (stopPropagation ev)
    pure next
  