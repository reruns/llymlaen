module Test.Components where

import Prelude
import Test.Unit

import Test.Components.Toolbar as Toolbar
import Test.Components.TimeControls as TimeControls
import Test.Components.ElementEditor as ElementEditor
import Test.Components.Diagram as Diagram

tests = 
  suite "Components" do
    Toolbar.tests
    TimeControls.tests
    ElementEditor.tests
    Diagram.tests