module Main where

import App.Prelude hiding (liftEff)

import Control.Monad.Aff (forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Timer (setInterval, TIMER)

import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

import App.Components.Diagram (Query(..), diaComp)
import App.Routes (Locations(..), routing)
import Routing (matchesAff)

main :: forall e. Eff ( "avar" :: AVAR
                      , "ref" :: REF
                      , "exception" :: EXCEPTION
                      , "dom" :: DOM
                      , "canvas" :: CANVAS
                      , "console" :: CONSOLE
                      , "timer" :: TIMER
                      , "ajax"  :: AJAX
                      | e ) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI diaComp unit body
  _ <- forkAff $ do
    Tuple old new <- matchesAff routing  
    case new of
      (Look id) -> driver.query $ action (Load id)
      _         -> pure unit
  liftEff $ setInterval framerate $ do
    HA.runHalogenAff (driver.query $ action Tick) --what.