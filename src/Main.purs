module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class

import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)

import Example.IntermissionA
import App.Diagram

import Control.Monad.Eff.Timer (setInterval)

main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI diaComp interA body
  liftEff $ setInterval 16 $ runHalogenAff (driver (H.action Tick)) --what.