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
import Data.Map (Map)
import Reflex.Dom.Core
import Text.Read
import Data.Maybe
import Control.Monad.Fix
import Common.Api
import Common.Route

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3

buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m (Event t T.Text)
buttonClick = do
   t <- inputElement def
   (e,_) <- el' "button" (text "OK")
   return $ attachPromptlyDynWith const
      (fmap revText (_inputElement_value t))
      (domEvent Click e)

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
   evt <- buttonClick
   hl <- holdDyn "" evt -- Event -> Dynamic
   el "div" (dynText hl)
   
numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
   n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes .~ ("type" =: "number")
   return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                 (_inputElement_value n)

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m)
          => m (Event t Double)
sumButton = do
   n1 <- numberInput
   text " "
   n2 <- numberInput
   text " "
   (e,_) <- el' "button" (text "=")
   let dynDouble = zipDynWith (+) n1 n2
   return $ attachPromptlyDynWith const
      dynDouble
      (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
   evt <- sumButton
   numero <- holdDyn 0 evt
   el "div" (dynText $ fmap (T.pack . show) numero)


revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))

caixas :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- Dynamic Text
  s <- inputElement def -- Dynamic Text
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)

caixaSoma :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
caixaSoma = do
   n1 <- numberInput -- m (Dynamic t Double)
   text " "
   n2 <- numberInput -- m (Dynamic t Double)
   dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))
   
   
   
              
countClick :: DomBuilder t m => m (Event t Int)
countClick = do
   (ev, _) <- el' "button" (text "+")
   return ((const 1) <$> domEvent Click ev)

pagClick :: (MonadHold t m, DomBuilder t m, PostBuild t m,
            MonadFix m) => m ()
pagClick = do
   evt <- countClick
   st <- accumDyn (+) 0 evt
   el "div" (dynText (fmap (T.pack . show) st))
   
   
      

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
   (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
   return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
   evs <- el "ul" $ do
      p1 <- clickLi Pagina1 "1.Inversor de texto"
      p2 <- clickLi Pagina2 "2.Soma"
      p3 <- clickLi Pagina3 "3.Contador de cliques"
      return (leftmost [p1,p2,p3])
   holdDyn Pagina0 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
        => Pagina -> m ()
currPag p =
   case p of
      Pagina0 -> blank
      Pagina1 -> bttnEvt
      Pagina2 -> sumEvt
      Pagina3 -> pagClick

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => m ()
mainPag = do
   pag <- el "div" menuLi
   dyn_ $ currPag <$> pag
   
   

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk - Trabalho P2 "
      elAttr "link" ("href" =: static @"main.css" 
                         <> "type" =: "text/css" 
                         <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Trabalho P2 "
      
      
      mainPag
      
      
      el "p" $ text $ T.pack commonStuff
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      elAttr "img" ("src" =: static @"haskell.png") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }