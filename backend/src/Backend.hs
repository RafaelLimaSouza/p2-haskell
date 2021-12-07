{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-44-199-158-170.compute-1.amazonaws.com"
                        5432
                        "gjudnjcmvyszpr"
                        "3ab8f72468543ba0e549a90ff098eded750c15ff3de34b95735aae40e635dd23"
                        "dj8hmse6rhmsf"
                        
migrateClie :: Query
migrateClie = "CREATE TABLE IF NOT EXISTS clientee (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, telefone TEXT NOT NULL, endereco TEXT NOT NULL)"

migrateProd :: Query
migrateProd = "CREATE TABLE IF NOT EXISTS produtos (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"

migrateFunc :: Query
migrateFunc = "CREATE TABLE IF NOT EXISTS funcionarios (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, salario REAL NOT NULL, hr INTEGER NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
   { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
         \case
            BackendRoute_Listar :/ () -> method GET $ do
               res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProd
                  query_ dbcon "SELECT * from produtos"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_Buscar :/ pid -> method GET $ do
               res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProd
                  query dbcon "SELECT * from produtos WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                  
            BackendRoute_Delete :/ pid -> method DELETE $ do
               res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProd
                  query dbcon "DELETE from produtos WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                  
            BackendRoute_Editar :/ pid -> method POST $ do
               prod <- A.decode <$> readRequestBody 2000
               case prod of
                    Just produto -> do
                        liftIO $ do
                            execute_ dbcon migrateProd
                            execute dbcon "UPDATE produtos SET nome = ?, \
                                            \ valor = ?, qt = ? WHERE id = ?"
                                    (produtoNome produto,
                                    produtoValor produto,
                                    produtoQt produto,
                                    pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                    
                    
                  
            BackendRoute_Produto :/ () -> method POST $ do
               prod <- A.decode <$> readRequestBody 2000
               case prod of
                  Just produto -> do
                     liftIO $ do
                        execute_ dbcon migrateProd
                        execute dbcon "INSERT INTO produtos (nome,valor,qt) VALUES (?,?,?)"
                                 (produtoNome produto, produtoValor produto, produtoQt produto)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                  
            BackendRoute_Funcionario :/ () -> method POST $ do
               func <- A.decode <$> readRequestBody 2000
               case func of
                  Just funcionario -> do
                     liftIO $ do
                        execute_ dbcon migrateFunc
                        execute dbcon "INSERT INTO funcionarios (nome,salario,hr) VALUES (?,?,?)"
                                 (funcNome funcionario, funcSalario funcionario, funcHr funcionario)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                  
            BackendRoute_ListarF :/ () -> method GET $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query_ dbcon "SELECT * from funcionarios"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_BuscarF :/ fid -> method GET $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query dbcon "SELECT * from funcionarios WHERE id=?" (Only (fid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                  
            BackendRoute_EditarF :/ fid -> method POST $ do
               func <- A.decode <$> readRequestBody 2000
               case func of
                    Just funcionario -> do
                        liftIO $ do
                            execute_ dbcon migrateFunc
                            execute dbcon "UPDATE funcionarios SET nome = ?, \
                                            \ salario = ?, hr = ? WHERE id = ?"
                                    (funcNome funcionario,
                                    funcSalario funcionario,
                                    funcHr funcionario,
                                    fid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                    
            BackendRoute_DeleteF :/ fid -> method DELETE $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query dbcon "DELETE from funcionarios WHERE id=?" (Only (fid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                  
            BackendRoute_Clientes :/ () -> method POST $ do
               clie <- A.decode <$> readRequestBody 2000
               case clie of
                  Just cliente -> do
                     liftIO $ do
                        execute_ dbcon migrateClie
                        execute dbcon "INSERT INTO clientee (nome, telefone, endereco ) VALUES (?,?,?)"
                                 (clienteNome cliente, clienteTel cliente, clienteEnd cliente )
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_ListarC :/ () -> method GET $ do
               res :: [Cliente] <- liftIO $ do
                  execute_ dbcon migrateClie
                  query_ dbcon "SELECT * from clientee"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_BuscarC :/ cid -> method GET $ do
               res :: [Cliente] <- liftIO $ do
                  execute_ dbcon migrateClie
                  query dbcon "SELECT * from clientee WHERE id=?" (Only (cid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                  
            BackendRoute_EditarC :/ cid -> method POST $ do
               clie <- A.decode <$> readRequestBody 2000
               case clie of
                    Just cliente -> do
                        liftIO $ do
                            execute_ dbcon migrateClie
                            execute dbcon "UPDATE clientee SET nome = ?, \
                                            \ telefone = ?,endereco  = ? WHERE id = ?"
                                    (clienteNome cliente,                                
                                    clienteTel cliente,
                                    clienteEnd cliente,                                    
                                    cid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                    
            BackendRoute_DeleteC :/ cid -> method DELETE $ do
               res :: [Cliente] <- liftIO $ do
                  execute_ dbcon migrateClie
                  query dbcon "DELETE from clientee WHERE id=?" (Only (cid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
   , _backend_routeEncoder = fullRouteEncoder
   }
