{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-3-227-149-67.compute-1.amazonaws.com"
          5432
          "ukeqlfncmauusi"
          "cc11078155f1e898de77893258a0f3e47252908fb4364e4a72539ca97dc11a28"
          "ddlsndl0ml9eq7"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
            BackendRoute_Cliente :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do
                    execute_ dbcon migrate
                    execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
