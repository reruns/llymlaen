module Test.Types where

import Prelude
import Test.Unit

import Test.Types.RGB as RGB
import Test.Types.Point as Point
import Test.Types.Property as Property
import Test.Types.Keyframe as Keyframe
import Test.Types.Element as Element
import Test.Types.Diag as Diag

tests =
  suite "Types" do
    RGB.tests
    Point.tests
    Property.tests
    Keyframe.tests
    Element.tests
    Diag.tests