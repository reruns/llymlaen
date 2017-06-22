{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Text
import Data.Aeson

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Diag
  body Text
  deriving Eq Read Show
|]

instance FromJSON Diag where
  parseJSON = withObject "Diag" $ \v ->
    Diag <$> v .: "body"
    
instance ToJSON Diag where
  toJSON (Diag body) =
    object [ "body" .= body ]