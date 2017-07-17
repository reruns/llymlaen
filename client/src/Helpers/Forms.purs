module App.Helpers.Forms where

import Prelude

import App.Helpers.Validators

import Halogen
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--functions for Form elements
checkBox props b h = 
  HH.input ([ HP.type_ HP.InputCheckbox
            , HP.checked b
            , HE.onChecked $ HE.input h
            ] <> props)
          
slider props min max v h =
  HH.input ([ HP.type_ HP.InputRange
            , HP.value (show v)
            , HP.prop (HH.PropName "min") min
            , HP.prop (HH.PropName "max") max
            , HE.onValueChange (\s -> (action <<< h) <$> (validateRange s min max))
            ] <> props) 
          
number props v h =
  HH.input ([ HP.prop (HH.PropName "InputType") HP.InputNumber
            , HP.value (show v)
            , HE.onValueChange (\s -> (action <<< h) <$> (validateNonNeg s))
            ] <> props)
            
color props v h =
  HH.input ([ HP.type_ HP.InputColor
            , HP.value (show v)
            , HE.onValueChange (\s -> (action <<< h) <$> (validateRGB s))
            ] <> props)