{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}


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
import Data.Aeson

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getProdReq :: Int -> XhrRequest ()
getProdReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

getDelReq :: Int -> XhrRequest () 
getDelReq pid = xhrRequest "DELETE" (getPath (BackendRoute_Delete :/ pid)) def

getFuncReq :: Int -> XhrRequest ()
getFuncReq fid = xhrRequest "GET" (getPath (BackendRoute_BuscarF :/ fid)) def

getListfReq :: XhrRequest ()
getListfReq = xhrRequest "GET" (getPath (BackendRoute_ListarF :/ ())) def

getDelfReq :: Int -> XhrRequest () 
getDelfReq fid = xhrRequest "DELETE" (getPath (BackendRoute_DeleteF :/ fid)) def

getCliReq :: Int -> XhrRequest ()
getCliReq cid = xhrRequest "GET" (getPath (BackendRoute_BuscarC :/ cid)) def

getListcReq :: XhrRequest ()
getListcReq = xhrRequest "GET" (getPath (BackendRoute_ListarC :/ ())) def

getDelcReq :: Int -> XhrRequest () 
getDelcReq cid = xhrRequest "DELETE" (getPath (BackendRoute_DeleteC :/ cid)) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados


-- Produto

reqProd :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqProd = do
   el "div" (text "Nome: ")
   nome <- inputElement def
   el "div" (text "Preço: ")
   vl <- numberInput
   el "div" (text "Estoque: ")
   qt <- numberInput
   el "div" (text "")
   let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
   (submitBtn,_) <- el' "button" (text "Cadastrar")
   let click = domEvent Click submitBtn
   let prodEvt = tag (current prod) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
   return ()


data Acao = Perfil Int | Editar Int | Deletar Int 


tabProduto :: (PostBuild t m, DomBuilder t m) => Dynamic t Produto
                                                -> m (Event t Acao)
tabProduto pr = do
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . produtoId) pr)
        el "td" (dynText $ fmap (T.pack . show .produtoNome) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoValor) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoQt) pr)
        evt1 <- fmap (fmap (const Perfil)) (button "perfil")
        evt2 <- fmap (fmap (const Editar)) (button "editar")
        evt3 <- fmap (fmap (const Deletar)) (button "deletar")
        return (attachPromptlyDynWith (flip ($)) (fmap produtoId pr) (leftmost [evt1,evt2,evt3]))
        
pagPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq pid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap produtoNome dynP)
        el "div" (dynText $ fmap (T.pack . show . produtoValor) dynP)
        el "div" (dynText $ fmap (T.pack . show . produtoQt) dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ ret)
    
    
editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq pid) <$> btn))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)
    el "div" (text "Nome: ")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap produtoNome dynE)
    el "div" (text "Preço: ")
    vl <- numberInputDyn (fmap produtoValor dynE)
    el "div" (text "Quantidade em estoque: ")
    qt <- numberInputDyn (fmap produtoQt dynE)
    el "div" (text "")
    
    let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt) 
    submitBtn <- button "Editar"
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
         (pure never)
         (fmap decodeXhrResponse <$>
             performRequestAsync (sendRequest (BackendRoute_Editar :/ pid) <$> prodEvt))
    
    return ("Editar: " <> (T.pack $ show pid), reqTabela <$ submitBtn)
    where 
        novoInput x = inputElement $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes .~ ("value" =: x)
      
deletarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deletarPerfil pid = Workflow $ do
    btn <- button "Deletar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getDelReq pid) <$> btn))
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ ret)
reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) =>  Workflow t m T.Text
reqTabela = Workflow $ do
    btn <- button "Listar"
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Estoque")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
            simpleList dynP tabProduto
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Disponibilidade", escolherPag <$> tb')
    where
        escolherPag (Perfil pid) = pagPerfil pid
        escolherPag (Editar pid) = editarPerfil pid
        escolherPag (Deletar pid) = deletarPerfil pid
        
reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    r <- workflow reqTabela
    el "div" (dynText r)
    
    -- Funcionario

reqFunc :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqFunc = do
   el "div" (text "Nome: ")
   nome <- inputElement def
   el "div" (text "Salario: ")
   salario <- numberInput
   el "div" (text "Horas trabalhadas: ")
   horas <- numberInput
   el "div" (text "")
   let func = fmap (\((n,s),h) -> Funcionario 0 n s h) (zipDyn (zipDyn (_inputElement_value nome) salario) horas)
   (submitBtn,_) <- el' "button" (text "Cadastrar")
   let click = domEvent Click submitBtn
   let funcEvt = tag (current func) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Funcionario :/ ()) <$> funcEvt))
   return ()

data AcaoFun = PerfilFun Int | EditarFun Int | DeletarFun Int 


tabFuncionario :: (PostBuild t m, DomBuilder t m) => Dynamic t Funcionario
                                                -> m (Event t AcaoFun)
tabFuncionario fu = do
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . funcId) fu)
        el "td" (dynText $ fmap (T.pack . show .funcNome) fu)
        el "td" (dynText $ fmap (T.pack . show . funcSalario) fu)
        el "td" (dynText $ fmap (T.pack . show . funcHr) fu)
        evt1 <- fmap (fmap (const PerfilFun)) (button "perfil")
        evt2 <- fmap (fmap (const EditarFun)) (button "editar")
        evt3 <- fmap (fmap (const DeletarFun)) (button "deletar")
        return (attachPromptlyDynWith (flip ($)) (fmap funcId fu) (leftmost [evt1,evt2,evt3]))
        
pagPerfilf :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilf fid = Workflow $ do
    btn <- button "mostrar"
    func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getFuncReq fid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn func)
    dynP <- return ((fromMaybe (Funcionario 0 "" 0 0)) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap funcNome dynP)
        el "div" (dynText $ fmap (T.pack . show . funcSalario) dynP)
        el "div" (dynText $ fmap (T.pack . show . funcHr) dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show fid), reqTabelaf <$ ret)
    
    
editarPerfilf :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilf fid = Workflow $ do
    btn <- button "mostrar"
    func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getFuncReq fid) <$> btn))
    mdyn <- return (switchDyn func)
    dynE <- return ((fromMaybe (Funcionario 0 "" 0 0)) <$> mdyn)
    el "div" (text "Nome: ")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap funcNome dynE)
    el "div" (text "Salario: ")
    salario <- numberInputDyn (fmap funcSalario dynE)
    el "div" (text "Horas trabalhadas: ")
    horas <- numberInputDyn (fmap funcHr dynE)
    el "div" (text "")
    
    let func = fmap (\((n,s),h) -> Funcionario 0 n s h) (zipDyn (zipDyn (_inputElement_value nome) salario) horas) 
    submitBtn <- button "Editar"
    let funcEvt = tag (current func) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
         (pure never)
         (fmap decodeXhrResponse <$>
            performRequestAsync (sendRequest ( BackendRoute_EditarF :/ fid ) <$> funcEvt))
    
    return ("Editar: " <> (T.pack $ show fid), reqTabelaf <$ submitBtn)
    where 
        novoInput x = inputElement $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes .~ ("value" =: x)
            
deletarPerfilf :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deletarPerfilf fid = Workflow $ do
    btn <- button "Deletar"
    func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getDelfReq fid) <$> btn))
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show fid), reqTabelaf <$ ret)
    
reqTabelaf :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) =>  Workflow t m T.Text
reqTabelaf = Workflow $ do
    btn <- button "Listar"
    func :: Dynamic t (Event t (Maybe [Funcionario])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListfReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn func)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Salario")
                el "th" (text "Horas")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
            simpleList dynP tabFuncionario
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Lista de funcionarios", escolherPag <$> tb')
    where
        escolherPag (PerfilFun fid) = pagPerfilf fid
        escolherPag (EditarFun fid) = editarPerfilf fid
        escolherPag (DeletarFun fid) = deletarPerfilf fid
        
        
reqListaf :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaf = do
    r <- workflow reqTabelaf
    el "div" (dynText r)

    
        
--cliente
reqCliente :: ( DomBuilder t m
       , Prerender js t m      
       ) => m ()
reqCliente = do
   el "div" (text "Nome: ")
   nome <- inputElement def
   el "div" (text "Telefone: ")
   tel <- inputElement def
   el "div" (text "Endereço")
   end <- inputElement def
   el "div" (text "")
   let clie = fmap (\((n,t),e) -> Cliente 0 n t e) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value tel)) (_inputElement_value end))
   (submitBtn,_) <- el' "button" (text "Cadastrar")
   let click = domEvent Click submitBtn
   let clieEvt = tag (current clie) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Clientes :/ ()) <$> clieEvt))
   return ()


data AcaoCli = PerfilCli Int | EditarCli Int | DeletarCli Int 

tabCliente :: (PostBuild t m, DomBuilder t m) => Dynamic t Cliente
                                                -> m (Event t AcaoCli)
tabCliente cl = do
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . clienteId) cl)
        el "td" (dynText $ fmap (T.pack . show .clienteNome) cl)        
        el "td" (dynText $ fmap (T.pack . show . clienteTel) cl)
        el "td" (dynText $ fmap (T.pack . show . clienteEnd) cl)
        evt1 <- fmap (fmap (const PerfilCli)) (button "perfil")
        evt2 <- fmap (fmap (const EditarCli)) (button "editar")
        evt3 <- fmap (fmap (const DeletarCli)) (button "deletar")
        return (attachPromptlyDynWith (flip ($)) (fmap clienteId cl) (leftmost [evt1,evt2,evt3]))
        
pagPerfilc :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilc cid = Workflow $ do
    btn <- button "mostrar"
    clie :: Dynamic t (Event t (Maybe Cliente)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getCliReq cid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn clie)
    dynP <- return ((fromMaybe (Cliente 0 "" "" "")) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap clienteNome dynP)
        el "div" (dynText $ fmap (T.pack . show . clienteTel) dynP)
        el "div" (dynText $ fmap (T.pack . show . clienteEnd) dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show cid), reqTabelac <$ ret)
    
    
editarPerfilc :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilc cid = Workflow $ do
    btn <- button "mostrar"
    clie :: Dynamic t (Event t (Maybe Cliente)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getCliReq cid) <$> btn))
    mdyn <- return (switchDyn clie)
    dynE <- return ((fromMaybe (Cliente 0 "" "" "")) <$> mdyn)
    
    el "div" (text "Nome: ")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE)
    el "div" (text "Telefone: ")
    tel <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTel dynE)
    el "div" (text "Endereço: ")
    end <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEnd dynE)
    el "div" (text "")
    
    let clie = fmap (\((n,t),e) -> Cliente 0 n t e) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value tel)) (_inputElement_value end))
    submitBtn <- button "Editar"
    let clieEvt = tag (current clie) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
         (pure never)
         (fmap decodeXhrResponse <$>
            performRequestAsync (sendRequest ( BackendRoute_EditarC :/ cid ) <$> clieEvt))
    
    return ("Editar: " <> (T.pack $ show cid), reqTabelac <$ submitBtn)
    where 
        novoInput x = inputElement $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes .~ ("value" =: x)
            
deletarPerfilc :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deletarPerfilc cid = Workflow $ do
    btn <- button "Deletar"
    func :: Dynamic t (Event t (Maybe Cliente)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getDelcReq cid) <$> btn))
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show cid), reqTabelac <$ ret)
    
reqTabelac :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) =>  Workflow t m T.Text
reqTabelac = Workflow $ do
    btn <- button "Listar"
    clie :: Dynamic t (Event t (Maybe [Cliente])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListcReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn clie)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")                
                el "th" (text "Telefone")
                el "th" (text "Endereco")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
            simpleList dynP tabCliente
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Lista de clientes", escolherPag <$> tb')
    where
        escolherPag (PerfilCli cid) = pagPerfilc cid
        escolherPag (EditarCli cid) = editarPerfilc cid
        escolherPag (DeletarCli cid) = deletarPerfilc cid
        
        
reqListac :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListac = do
    r <- workflow reqTabelac
    el "div" (dynText r)
    

    
numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
   n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes .~ ("type" =: "number")
   return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                 (_inputElement_value n)
         
numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                (_inputElement_value n)
                

                
clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
   (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
   return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
   evs <- el "ul" $ do
      
      p1 <- clickLi Pagina1 "1.Cadastro de Jornais e Revistas"
      p2 <- clickLi Pagina2 "2.Listar Jornais e Revistas"
      p3 <- clickLi Pagina3 "3.Cadastro de Funcionarios"
      p4 <- clickLi Pagina4 "4.Listar Funcionarios"
      p5 <- clickLi Pagina5 "5.Cadastro de clientes"
      p6 <- clickLi Pagina6 "6.Listar clientes"
      
      return (leftmost [p1,p2,p3,p4,p5,p6])
   holdDyn Pagina0 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m)
        => Pagina -> m ()
currPag p =
   case p of
      Pagina0 -> blank
      Pagina1 -> reqProd
      Pagina2 -> reqLista
      Pagina3 -> reqFunc
      Pagina4 -> reqListaf
      Pagina5 -> reqCliente
      Pagina6 -> reqListac


mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => m ()
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
      el "h1" $ text "Banca Digital "
      
      
      mainPag
      
      
      el "p" $ text $ T.pack commonStuff
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      elAttr "img" ("src" =: static @"bancadig.png") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
