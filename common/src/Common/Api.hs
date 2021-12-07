{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple


data Cliente = Cliente {
    clienteId :: Int,
    clienteNome :: Text,    
    clienteTel :: Text,
    clienteEnd :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Produto = Produto {
    produtoId :: Int,
    produtoNome :: Text,
    produtoValor :: Double,
    produtoQt :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Funcionario = Funcionario {
    funcId :: Int,
    funcNome :: Text,
    funcSalario :: Double,
    funcHr :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


commonStuff :: String
commonStuff = "Integrantes: Lucas Vasques | Rafael Lima | Wellington Bispo    #Common.Api"
