module Main where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

main = do
  cv <- getCanvasElementById "canvas"
  case cv of
    Nothing -> log "no canvas element found?"
    Just canvas -> do
      context <- getContext2D canvas
      log "we good"
