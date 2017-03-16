module App.Routes where

import Prelude
import Control.Alt ((<|>))
import Control.Apply
import Routing
import Routing.Match
import Routing.Match.Class

data Locations
  = Home
  | Look String
  
  
oneSlash :: Match Unit
oneSlash = lit "/"


homeSlash :: Match Unit
homeSlash = lit ""

home :: Match Locations
home = Home <$ oneSlash

look :: Match Locations
look = Look <$> (homeSlash *> str)

routing :: Match Locations
routing = 
  look <|>
  home