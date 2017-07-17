module App.Prelude 
  ( module Prelude
  , module Graphics.Canvas
  , module Graphics.Canvas.Free
  , module Data.Argonaut
  , module Data.Maybe
  , module Data.Array
  , module Data.Int
  , module Data.Ord
  , module Data.Traversable
  , module Data.Foldable
  , module Math
  , module Test.QuickCheck
  , module Test.QuickCheck.Gen
  , module Data.String
  ) where

import Prelude
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasDimensions, Context2D)
import Graphics.Canvas.Free

import Data.Argonaut
import Data.Array (insertBy, (!!), updateAt, findIndex, findLastIndex, zipWith, length, sort)
import Data.Foldable (and, foldl)
import Data.Int (toNumber, round, toStringAs, fromStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Ord (comparing)
import Data.String (take, drop, joinWith)
import Data.Traversable (sequence, sequence_)

import Test.QuickCheck(class Arbitrary, arbitrary)
import Test.QuickCheck.Gen

import Math (pi, sqrt, pow, sin, cos)