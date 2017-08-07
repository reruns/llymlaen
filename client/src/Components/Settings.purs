module App.Components.Settings where

import App.Prelude
import App.Types.RGB
import App.Helpers.Forms

import DOM (DOM)
import DOM.Classy.Event (toEvent, stopPropagation)
import DOM.Event.Types (Event)
import Control.Monad.Aff (Aff)

type State = Maybe { length :: Int
                   , color :: RGB
                   }

data Query a = SetValues State (Unit -> a)
             | Close a
             | Ok a
             | SetLen Int a
             | SetRgb RGB a
             | StopPropagation Event a
             
settingsComponent :: forall eff. Component HTML Query Unit State (Aff (dom :: DOM | eff))
settingsComponent = component 
  { initialState: const Nothing
  , render
  , eval
  , receiver: const Nothing
  } 
  where
  
  render :: State -> ComponentHTML Query
  render st = 
    let outerClasses = ClassName <$> (if isJust st then ["modal"] else ["modal","off"]) 
        l = fromMaybe 0 $ _.length <$> st
        c = fromMaybe (RGB {r:0,g:0,b:0}) $ _.color <$> st in
    div [classes outerClasses, onClick $ input_ Close] 
      [ div 
        [ class_ $ ClassName "settings"
        , onClick $ \e -> Just $ action (StopPropagation (toEvent e))
        ]
        
        [ h2_ [text "Diagram Settings"]
        , label [] 
          [ div_ [text "Length"]
          , number [ title "Length", id_ "length"] (round $ (toNumber l) * (toNumber framerate) / 1000.0) SetLen
          , text "seconds"
          ]
        , label [] 
          [ div_ [text "Color"]
          , color [title "color"] c SetRgb
          ]
        , div [class_ $ ClassName "buttons"] 
          [ a [ class_ $ ClassName "a-button", onClick $ input_ Close]
            [ text "Cancel"]
          , a [class_ $ ClassName "a-button", onClick $ input_ Ok] 
            [ text "Apply" ]
          ]
        ]
      ]
  
  eval :: forall eff. Query ~> ComponentDSL State Query State (Aff (dom :: DOM | eff))
  eval (SetValues dat reply) = do
    put dat
    pure (reply unit)
    
  eval (Close next) = do
    put Nothing
    pure next
    
  eval (Ok next) = do
    raise =<< get
    put Nothing
    pure next
    
  eval (SetLen l next) = do
    modify $ map (_ {length = round ((toNumber l) * 1000.0 / (toNumber framerate))})
    pure next
    
  eval (SetRgb c next) = do
    modify $ map (_ {color = c})
    pure next
 
  eval (StopPropagation ev next) = do
    liftEff (stopPropagation ev)
    pure next