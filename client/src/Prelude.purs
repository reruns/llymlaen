module App.Prelude 
  ( module Prelude
  , module Graphics.Canvas
  , module Graphics.Canvas.Free
  , module Data.Argonaut
  , module Data.Maybe
  , module Data.Array
  , module Data.Either
  , module Data.Int
  , module Data.Ord
  , module Data.Traversable
  , module Data.Foldable
  , module Math
  , module Test.QuickCheck
  , module Test.QuickCheck.Gen
  , module Data.String
  , module Halogen
  , module Halogen.HTML
  , module Halogen.HTML.Events
  , module Halogen.HTML.Properties
  ) where

import Prelude
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free

import Data.Argonaut
import Data.Array (insertBy, (!!), updateAt, modifyAt, findIndex, findLastIndex, mapWithIndex, zipWith, length, sort, head, concat)
import Data.Foldable (and, foldl, traverse_)
import Data.Int (toNumber, floor, round, toStringAs, fromStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Either (Either(..))
import Data.Ord (comparing)
import Data.String (Pattern(..), take, drop, joinWith, split)
import Data.Traversable (sequence, sequence_)

import Test.QuickCheck(class Arbitrary, arbitrary)
import Test.QuickCheck.Gen

import Math (pi, sqrt, pow, sin, cos)

import Halogen (Component, ComponentHTML, ComponentDSL, ParentHTML, ParentDSL, RefLabel(..), raise, put, get, gets, component, lifecycleParentComponent, modify, action, liftEff, liftAff, query', request, getHTMLElementRef)
import Halogen.HTML (ClassName(..), HTML, a, canvas, span, div, div_, text, h2_, h3_, span, label, slot')
import Halogen.HTML.Events (input, input_, onClick, onMouseMove, onMouseLeave)
import Halogen.HTML.Properties (id_, class_, classes, title, href, ref)