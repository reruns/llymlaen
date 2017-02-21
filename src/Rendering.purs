module App.Element.Rendering where

import Prelude
import Data.Maybe (Maybe (Just,Nothing))

import App.Validators
import App.Element

import Halogen (Action, action)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P


--again, we can do some abstraction of this if we make lenses.
colorInput qr (Element el) g s = 
  H.input [ P.inputType P.InputRange
          , P.value $ (s el)
          , P.IProp $ H.prop (H.propName "min") (Just $ H.attrName "min") 0
          , P.IProp $ H.prop (H.propName "max") (Just $ H.attrName "max") 255
          , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateColor g s (Element el)))
          ]
          
redInput qr el = colorInput qr el (\el v -> el{current=el.current{color=el.current.color{r=v}}}) (\el -> show el.current.color.r)
greenInput qr el = colorInput qr el (\el v -> el{current=el.current{color=el.current.color{g=v}}}) (\el -> show el.current.color.g)
blueInput qr el = colorInput qr el (\el v -> el{current=el.current{color=el.current.color{b=v}}}) (\el -> show el.current.color.b)