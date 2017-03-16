module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Graphics.Canvas (CANVAS)
import Control.Monad.Eff.Console (CONSOLE)

import Halogen (action)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

import Example.IntermissionA
import App.Diagram

import Control.Monad.Eff.Timer (setInterval)

main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI diaComp unit body
  liftEff $ setInterval 16 $ do
    HA.runHalogenAff (driver.query $ action Tick) --what.