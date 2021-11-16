{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Data.Map (Map)
import Data.Maybe
import Text.Read

import Common.Api
import Common.Route

caixaSoma :: (PostBuild t m, DomBuilder t m) => m()
caixaSoma = do
    n1 <- numberInput --m (Dynamic t Double)
    text " "
    n2 <- numberInput --m (Dynamic t Double)
    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))


numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)

listaAttr :: Map T.Text T.Text
listaAttr = "class" =: "class1" <> "id" =: "li2"

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- Dynamic Text
  s <- inputElement def -- Dynamic Text
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t)
                            (_inputElement_value s)

menu :: DomBuilder t m => m ()
menu = do
      el "div" $ do
          el "ol" $ do
              el "span" (text "Integrantes: ")
              el "li" (text "Lucas Vasques")
              el "li" (text "Rafael Lima")
              el "li" (text "Wellinton Bispo")
              elAttr "li" 
                      listaAttr 
                      (text "Item de função")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css"
                         <> "type" =: "text/css"
                         <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Trabalho P2"
      caixaSoma

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"haskell.png") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
