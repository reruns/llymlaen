{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Text
import Data.Proxy
import qualified Data.ByteString.Lazy as BS

import Models
import Database.Persist

import Servant.API
import Servant.HTML.Blaze

newtype RawHtml = RawHtml { unRaw :: BS.ByteString }

instance MimeRender HTML RawHtml where
  mimeRender _ =  unRaw

type Api =
  Get '[HTML] RawHtml :<|>
  "api" :>
    ( "diagrams" :> ReqBody '[JSON] Diag :> Post '[JSON] (Maybe (Key Diag)) :<|>
      "diagrams" :> Capture "diagId" (Key Diag) :> Get '[JSON] (Maybe Diag) 
    )
     
api :: Proxy Api
api = Proxy