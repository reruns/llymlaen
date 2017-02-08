module Main where

import App.Diagram (advanceFrame, render)
import Example.IntermissionA

import Prelude (bind, ($))
import Data.Maybe (Maybe(Just, Nothing))

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Timer (setInterval)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef)

import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions)
import Graphics.Canvas.Free (runGraphics)

main = do
  cv <- getCanvasElementById "canvas"
  case cv of
    Nothing -> log "no canvas element found?"
    Just canvas -> do
      setCanvasDimensions {width: 800.0, height: 800.0} canvas
      context <- getContext2D canvas
      ref <- newSTRef interA
      setInterval 16 do 
        modifySTRef ref \el -> advanceFrame el
        l <- readSTRef ref
        runGraphics context $ render l
      log "ok"