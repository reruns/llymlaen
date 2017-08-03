module App.Prelude 
  ( module Prelude
  , module Data.Argonaut
  , module Data.Array
  , module Data.Either
  , module Data.Foldable
  , module Data.Int
  , module Data.Maybe
  , module Data.Ord
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Graphics.Canvas
  , module Graphics.Canvas.Free
  , module Halogen
  , module Halogen.HTML
  , module Halogen.HTML.Events
  , module Halogen.HTML.Properties
  , module Math
  , module Test.QuickCheck
  , module Test.QuickCheck.Gen
  , module Network.HTTP.Affjax
  , module Control.Monad.Aff.AVar
  , module Control.Monad.Eff.Console
  , module DOM
  , module Control.Monad.Eff.Exception
  , module Control.Monad.Eff.Ref
  , framerate
  ) where

import Prelude hiding (div)

import Data.Argonaut hiding (toNumber, fromString)
import Data.Array (insertBy, (!!), updateAt, modifyAt, findIndex, findLastIndex, mapWithIndex, zipWith, length, sort, head, concat)
import Data.Foldable (and, foldl, traverse_)
import Data.Int (toNumber, floor, round, toStringAs, fromString, fromStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Either (Either(..))
import Data.Ord (comparing)
import Data.String (Pattern(..), take, drop, joinWith, split)
import Data.Traversable (sequence, sequence_)
import Data.Tuple(Tuple(..))

import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free

import Halogen (Component, ComponentHTML, ComponentDSL, ParentHTML, ParentDSL, RefLabel(..), raise, put, get, gets, component, lifecycleParentComponent, modify, action, liftEff, liftAff, query', request, getHTMLElementRef)
import Halogen.HTML (ClassName(..), HTML, a, canvas, span, div, div_, text, h2_, h3_, span, label, slot')
import Halogen.HTML.Events (input, input_, onClick, onMouseDown, onMouseMove, onMouseLeave, onMouseUp)
import Halogen.HTML.Properties (id_, class_, classes, title, href, ref)

import Math (pi, sqrt, pow, sin, cos)

import Test.QuickCheck(class Arbitrary, arbitrary)
import Test.QuickCheck.Gen

import Network.HTTP.Affjax (AJAX)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import DOM(DOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)

--I guess this is a misnomer? The value is ms/frame
framerate :: Int
framerate = 20