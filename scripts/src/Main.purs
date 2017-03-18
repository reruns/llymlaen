module Main where

import Prelude (bind, pure, unit, ($), Unit)
import Control.Monad.Aff (forkAff)
import Control.Monad.Eff.Class (liftEff)

import Control.Monad.Eff.Timer (setInterval, TIMER)

import Data.Tuple (Tuple(..))

import Halogen (action)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

import App.Diagram (Query(..), diaComp)
import App.Routes (Locations(..), routing)
import Routing (matchesAff)

--imports used only for the type signature
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import DOM(DOM)
import Graphics.Canvas (CANVAS)

main :: forall e. Eff ( "avar" :: AVAR
                      , "ref" :: REF
                      , "err" :: EXCEPTION
                      , "dom" :: DOM
                      , "canvas" :: CANVAS
                      , "console" :: CONSOLE
                      , "timer" :: TIMER
                      | e ) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI diaComp unit body
  forkAff $ do
    Tuple old new <- matchesAff routing  
    case new of
      (Look id) -> driver.query $ action (FetchState id)
      _         -> pure unit
  liftEff $ setInterval 16 $ do
    HA.runHalogenAff (driver.query $ action Tick) --what.