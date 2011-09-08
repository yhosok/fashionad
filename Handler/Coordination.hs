{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Coordination where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)

import Yesod.Form.Jquery
import Foundation
import Handler.Item
import Settings.StaticFiles (js_jquery_resize_js)

coordForm :: UserId -> 
             Maybe B.ByteString -> 
             Maybe Coordination -> 
             Html -> 
             Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mf mc = renderTable $ Coordination
    <$> pure uid
    <*> areq textField "title" (fmap coordinationTitle mc)
    <*> aopt textField "description" (fmap coordinationDesc mc)
    <*> pure mf

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  mu <- requireAuth
  cos <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "fashionad homepage"
    addWidget $(widgetFile "coordinations")

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  mf <- getFileData "coimg"
  ((res, form), enc) <- runFormPost $ coordForm uid mf mc
  ((_, itemForm), _) <- runFormPost $ itemForm cid Nothing
  y <- getYesod
  case res of
    FormSuccess c -> do
      runDB $ replace cid c
      setMessage "Updated Coordination"
      redirect RedirectTemporary CoordinationsR
    _ -> return ()
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_resize_js
    addWidget $(widgetFile "coordination")

getFileData :: Text -> Handler (Maybe B.ByteString)
getFileData s = do
  mfi <- lookupFile s
  return $ fmap (B.pack . L.unpack . fileContent) mfi 

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  let items = []
  mf <- getFileData "coimg"
  ((res,form),enc) <- runFormPost $ coordForm uid mf Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary CoordinationsR
    _ -> return ()
  defaultLayout $ do
    addWidget $(widgetFile "item")

postAddCoordinationR :: Handler RepHtml
postAddCoordinationR = getAddCoordinationR

postDelCoordinationR :: CoordinationId -> Handler RepHtml
postDelCoordinationR = undefined

getCoordinationImgR :: CoordinationId -> Handler (ContentType, Content)
getCoordinationImgR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  case mc of
    Just c -> do
      img <- return $ maybe B.empty id (coordinationImage c)
      return (typeJpeg, toContent img)

