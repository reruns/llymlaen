{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Text
import Data.Proxy

import Models
import Database.Persist

import Servant.API

type Api =
  "api" :>
    ( "diagrams" :> ReqBody '[JSON] Diag :> Post '[JSON] (Maybe (Key Diag)) :<|>
      "diagrams" :> Capture "diagId" (Key Diag) :> Get '[JSON] (Maybe Diag) 
    )
     
api :: Proxy Api
api = Proxy