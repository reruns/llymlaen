module App.Components.Settings where

import Prelude
import App.Types.RGB
import App.Helpers.Forms

import DOM (DOM)
import DOM.Classy.Event (toEvent, stopPropagation)
import DOM.Event.Types (Event)
import Control.Monad.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Maybe
import Data.Int (toNumber, round)

type State = Maybe { length :: Int
                   , color :: RGB
                   }

data Query a = SetValues State (Unit -> a)
             | Close a
             | Ok a
             | SetLen Int a
             | SetRgb RGB a
             | StopPropagation Event a
             
component :: forall eff. H.Component HH.HTML Query Unit State (Aff (dom :: DOM | eff))
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
        l = fromMaybe 0 $ _.length <$> st
        c = fromMaybe (RGB {r:0,g:0,b:0}) $ _.color <$> st in
    HH.div [HP.classes outerClasses, HE.onClick $ HE.input_ Close] 
      [ HH.div 
        [ HP.class_ $ HH.ClassName "settings"
        , HE.onClick $ \e -> Just $ H.action (StopPropagation (toEvent e))
        ]
        
        [ HH.h2_ [HH.text "Diagram Settings"]
        , HH.label [] 
          [ HH.div_ [HH.text "Length"]
          , number [ HP.title "Length", HP.id_ "length"] (round $ (toNumber l) / 62.5) SetLen
          , HH.text "seconds"
          ]
        , HH.label [] 
          [ HH.div_ [HH.text "Color"]
          , color [HP.title "color"] c SetRgb
          ]
        , HH.div [HP.class_ $ HH.ClassName "buttons"] 
          [ HH.a [ HP.class_ $ HH.ClassName "a-button", HE.onClick $ HE.input_ Close]
            [ HH.text "Cancel"]
          , HH.a [HP.class_ $ HH.ClassName "a-button", HE.onClick $ HE.input_ Ok] 
            [ HH.text "Apply" ]
          ]
        ]
      ]
  
  eval :: forall eff. Query ~> H.ComponentDSL State Query State (Aff (dom :: DOM | eff))
  eval (SetValues dat reply) = do
    H.put dat
    pure (reply unit)
    
  eval (Close next) = do
    H.put Nothing
    pure next
    
  eval (Ok next) = do
    H.raise =<< H.get
    H.put Nothing
    pure next
    
  eval (SetLen l next) = do
    H.modify $ map (_ {length = round ((toNumber l) * 62.5)})
    pure next
    
  eval (SetRgb c next) = do
    H.modify $ map (_ {color = c})
    pure next
 
  eval (StopPropagation ev next) = do
    H.liftEff (stopPropagation ev)
    pure next