{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)

import Data.String.Conversions
import Data.Int
import qualified Data.ByteString.Lazy as BS

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql

import Network.Wai
import Network.Wai.Handler.Warp as Warp

import Servant

import Data.Text

import Api
import Models

server :: ConnectionPool -> Server Api
server pool = serveRootH
  :<|> saveDiagH 
  :<|> findDiagH
  where
    saveDiagH newDiag   = liftIO $ saveDiag newDiag
    findDiagH idStr     = liftIO $ findDiag idStr
    
    saveDiag :: Diag -> IO (Maybe (Key Diag))
    saveDiag newDiag = flip runSqlPersistMPool pool $ do
      Just <$> insert newDiag

    findDiag :: Key Diag -> IO (Maybe Diag)
    findDiag id = flip runSqlPersistMPool pool $ do
      mDiag <- get id
      return $ mDiag
    
    serveRootH = fmap RawHtml (liftIO $ BS.readFile "index.html")

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp pgFile = do
  pool <- runStderrLoggingT $ do
    createPostgresqlPool (cs pgFile) 5
    
  runSqlPool (runMigration migrateAll) pool
  return $ app pool
  
run :: FilePath -> IO ()
run pgFile = Warp.run 3000 =<< mkApp pgFile