module Main where

import App
import System.Environment

main :: IO ()
main = do
  putStrLn "Starting Llymlaen Server!"
  run =<< (getEnv "DATABASE_URL")