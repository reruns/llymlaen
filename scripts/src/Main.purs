module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (forkAff)
import Control.Monad.Eff.Class

import Data.Tuple

import Halogen (action)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

import App.Diagram
import App.Routes
import Routing (matchesAff)

import Control.Monad.Eff.Timer (setInterval)

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