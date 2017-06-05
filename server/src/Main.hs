module Main where

import App

--TODO: Where actually is the db?
main :: IO ()
main = run "host=localhost dbname=llymtest user=testt password=test port=5433"