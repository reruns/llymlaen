module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Types as Types

import Test.Unit
import Test.Unit.Main (runTest)

main = runTest do
  suite "Main" do
    Types.tests

    
--and then also however we use selenium here.