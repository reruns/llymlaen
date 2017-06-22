module Main where

import App
import System.Environment

--TODO: Where actually is the db?
main :: IO ()
main = run =<< (lookupEnv "DATABASE_URL")