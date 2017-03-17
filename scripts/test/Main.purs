module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Property
import Test.Element
import Test.Diagram
import Test.TimeControls
import Test.ElementEditor

import Test.Unit
import Test.Unit.Main (runTest)

main = runTest do
  suite "Main" do
    propSpec

    
--and then also however we use selenium here.