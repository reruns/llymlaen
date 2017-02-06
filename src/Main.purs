module Main where

import App.Element

import Prelude (bind, negate, show, ($))
import Data.Maybe (Maybe(Just, Nothing))

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (setInterval)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef)

import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions)
import Graphics.Canvas.Free (runGraphics)

main = do
  cv <- getCanvasElementById "canvas"
  case cv of
    Nothing -> log "no canvas element found?"
    Just canvas -> do
      setCanvasDimensions {width: 1000.0, height: 1000.0} canvas
      context <- getContext2D canvas
      ref <- newSTRef testInit
      setInterval 16 do 
        modifySTRef ref \el -> advanceFrame el
        l <- readSTRef ref
        runGraphics context $ do
          render l
      log "ok"
      

      
testInit = {
              layer: 1,
              shape: Circle,
              pindex: -1,
              keys: [{enabled: true,
                  time: 0,
                  angle: 0,
                  size: {w:100, h:100},
                  pos: {x:0, y:0},
                  opacity: 100,
                  color: {r:255, g:0, b:255}},
                  {enabled: false,
                  time: 600,
                  angle: 0,
                  size: {w:100, h:100},
                  pos: {x:500, y:500},
                  opacity: 100,
                  color: {r:255, g:0, b:255}}],
              current: {
                  enabled: false,
                  time: -1,
                  angle: 0,
                  size: {w:100, h:100},
                  pos: {x:0, y:0},
                  opacity: 100,
                  color: {r:255, g:0, b:255}
                }
}