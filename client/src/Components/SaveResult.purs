module App.Components.SaveResult where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (href)
import DOM.HTML.Window (location)
import DOM.Classy.Event (toEvent, stopPropagation)
import DOM.Event.Types (Event)

import Control.Monad.Aff (Aff)

import Data.Maybe
import Data.String (split, Pattern(..))
import Data.Array (head)

data State = Success String | Fail | Off

data Query a = Succeed String (Unit -> a)
             | Close a
             | StopPropagation Event a
             
off :: State -> Boolean
off Off = true
off _ = false
             
component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
component = H.component 
  { initialState: const Off
  , render
  , eval
  , receiver: const Nothing
  } 
  where
  
  render :: State -> H.ComponentHTML Query
  render Off = HH.div [HP.class_ $ HH.ClassName "off"] []
  render Fail = HH.div [HP.class_ $ HH.ClassName "off"] []
  render Success link =
    HH.div 
    [ HP.classes $ HH.ClassName <$> ["modal","off"]
    , HE.onClick $ HE.input_ Close
    ] 
    [ HH.div 
      [ HP.class_ $ HH.ClassName "modal-content"
      , HE.onClick $ \e -> Just $ H.action (StopPropagation (toEvent e))
      ] 
      [ HH.h3_ [ HH.text "Saved!" ]
      , HH.div_ 
        [ HH.text "The link to share your work is here:"
        , HH.a [HP.href link] [HH.text link]
        ]
      , HH.a [HP.class_ $ HH.ClassName "a-button", HE.onClick $ HE.input_ Close ] 
        [ HH.text "OK" ]
      ]
    ]
  
  eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (Succeed id reply) = do
    url <- H.liftEff $ href =<< location =<< window
    let link = (fromMaybe "" $ head $ split (Pattern "#") url) <> "#/" <> id
    H.put $ Success link
    pure (reply unit)
    
  eval (Close next) = do
    H.put Off
    pure next
    
  eval (StopPropagation ev next) = do
    H.liftEff (stopPropagation ev)
    pure next
  